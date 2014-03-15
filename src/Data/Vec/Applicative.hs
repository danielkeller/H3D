{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Vec.Applicative (
    (.**.), (.*.),
) where

--import qualified Prelude as P
--import Prelude (Functor(..))
import Control.Wire

--import Control.Applicative
import Data.Vec as V

(.**.) :: Applicative f => f a -> f b -> f (a :. (b :. ()))
l .**. r = l .*. r .*. (pure ())

(.*.) :: Applicative f => f a -> f b -> f (a :. b)
(.*.) = liftA2 (:.)

infixr 4 .*.

--instance (Applicative f, Vec n a v) => Vec n (f a) (f v) where
--    mkVec = liftA2 mkVec

instance (Monad m, Map x y u v) => Map x y (Wire s e m a u) (Wire s e m a v) where
    map f = fmap (V.map f)

instance (Monad m, Transpose u v) => Transpose (Wire s e m a u) (Wire s e m a v) where
    transpose  = fmap transpose

instance (Monad m, Fold v b) => Fold (Wire s e m a v) (Wire s e m a b) where
    fold f = fmap (V.fold f)
    foldl f = fmap (V.foldl f)
    foldr f = fmap (V.foldr f)