module Util (
    GameSession,
    PlainWire,
    PlainWire2,
    fromEither
) where

import Control.Wire

type GameSession = Timed NominalDiffTime ()
type PlainWire b = Wire GameSession () IO () b
type PlainWire2 a b = Wire GameSession () IO a b

fromEither :: String -> Either String b -> b
fromEither message (Left err) = error $ message ++ ": " ++ err
fromEither _ (Right res) = res