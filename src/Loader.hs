{-# LANGUAGE DataKinds, TypeOperators, ScopedTypeVariables #-}
module Loader (
    Loader, evalLoader,
    Resource(..), resource, resource1, resourceN,
) where

import qualified Data.Map as M
import Data.Vinyl
import Control.Wire hiding ((.), hold)
import Data.Typeable
import Data.Dynamic
import Control.Monad.Trans.State -- strict?
import Control.Monad.Trans.Class

import Util

evalLoader :: Loader a -> IO a
evalLoader ldr = evalStateT ldr M.empty

type Loader = StateT (M.Map String RefCounted) IO

data Resource a b = Resource { resName :: String
                             , resLoad :: b -> IO a
                             , resUnload :: a -> IO ()
                             }

--TODO: not have to spell out this type to avoid Loader -> Component circularity
resource :: Typeable a => Resource a () -> PlainWire Loader (PlainRec '[]) (PlainRec '[sy ::: a])
resource r = resource1 r <<< pure (singleton ())

resource1 :: Typeable a => Resource a b -> PlainWire Loader (PlainRec '[sy1 ::: b]) (PlainRec '[sy ::: a])
resource1 r = resourceN r {resLoad = resLoad r . unSingleton}

resourceN :: Typeable a => Resource a (PlainRec rs) -> PlainWire Loader (PlainRec rs) (PlainRec '[sy ::: a])
resourceN Resource {resName = name, resLoad = load, resUnload = unload} =
    nowNLater loader' (unloader =<< getTracked name)
    where loader' d = loader d =<< getTracked name
          loader dep Nothing = do
              realResource <- lift $ load dep
              modify $ M.insert name $ RefCounted 1 $ toDyn realResource
              lift $ putStrLn $ "Loaded " ++ show (typeOf realResource) ++ " " ++ name
              return (Field =: realResource)
          loader _ (Just (RefCounted c r, realResource)) = do
              modify $ M.insert name $ RefCounted (c + 1) r
              return (Field =: realResource)

          unloader (Just (RefCounted c r, realResource)) --if this fails we have a bug
              | c == 1 = do lift (unload realResource)
                            lift $ putStrLn $ "Unloaded " ++ show (typeOf realResource) ++ " " ++ name
                            modify (M.delete name)
              | otherwise = modify $ M.insert name $ RefCounted (c - 1) r
          unloader _ = error "Attempting to unload again!"

data RefCounted = RefCounted Int Dynamic

-- not found is not an error, but type mismatch is
getTracked :: forall a. Typeable a => String -> Loader (Maybe (RefCounted, a))
getTracked name = gets $ fmap extract . M.lookup name
    where extract val@(RefCounted _ r) =
              case fromDynamic r of
                  Nothing -> error $ "Asked for type '" ++ show (typeOf (undefined :: a)) ++
                                     "' but resource '" ++ name ++
                                     "'' has type '" ++ show (dynTypeRep r)
                  Just ret -> (val, ret)

nowNLater :: Monad m => (a -> m b) -> m e -> Wire s e m a b
nowNLater doNow doLater = WGen setup
    where -- wait until we're un-inhibited
          setup _ (Left e) = return (Left e, WGen setup)
          -- run the action once, then hold the value
          setup _ (Right v) = do
              a <- doNow v
              return (Right a, WGen (hold a))
          -- hold the value until we inhibit
          hold a _ (Right _) = return (Right a, WGen (hold a))
          -- run the later action when we inhibit again
          hold _ _ (Left _) = do
              e <- doLater
              return (Left e, WGen setup)