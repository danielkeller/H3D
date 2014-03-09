module Util (
    GameSession,
    PlainWire
) where

import Prelude hiding ((.))
import Control.Wire

type GameSession = Timed NominalDiffTime ()
type PlainWire b = Wire GameSession () IO () b
--type SimpleWire a b = Wire GameSession () IO a b