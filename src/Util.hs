module Util (
    GameSession,
    PlainWire,
    PlainWire2,
    DrawFun,
    fromEither,
    (>>=&)
) where

import Control.Wire

type GameSession = Timed Float ()
type PlainWire b = Wire GameSession () IO () b
type PlainWire2 a b = Wire GameSession () IO a b

type DrawFun = Float -> IO ()

fromEither :: String -> Either String b -> b
fromEither message (Left err) = error $ message ++ ": " ++ err
fromEither _ (Right res) = res

(>>=&) :: (a -> IO b) -> (a -> IO c) -> a -> IO c
(>>=&) l r a = l a >> r a

infixl 1 >>=&