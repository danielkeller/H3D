{-# LANGUAGE DataKinds, TypeOperators, Arrows, GADTs, TypeFamilies,
    MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Util (
    bundle,
    Bundled,
    GameSession,
    PlainWire
) where

import Prelude hiding ((.))
import Data.Vinyl as VR
import Control.Wire

type GameSession = Timed NominalDiffTime ()
type PlainWire b = Wire GameSession () IO () b
--type SimpleWire a b = Wire GameSession () IO a b

type family Bundled rw :: [*]
type instance Bundled (PlainRec '[]) = '[]
type instance Bundled (PlainRec (sy ::: (Wire s e m a b) ': ws)) = sy ::: b ': Bundled (PlainRec ws)

class Bundle rw w where
    bundle :: rw -> w (PlainRec (Bundled rw))

instance Monad m => Bundle (PlainRec '[]) (Wire s e m a) where
    bundle RNil = pure RNil

instance (Monad m, Bundle (PlainRec rws) (Wire s e m a)) =>
        Bundle (PlainRec (sy ::: (Wire s e m a b) ': rws)) (Wire s e m a) where
    bundle (Identity r :& rs) = proc inp -> do
        r' <-r-< inp
        rs' <-bundle rs-< inp
        returnA -< (Identity r' :& rs')