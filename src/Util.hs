{-# LANGUAGE DataKinds, TypeOperators, GADTs #-}
module Util (
    GameSession,
    PlainWire0, PlainWire,
    fromEither, fromEitherGeneric,

    DrawFun, Draw, draw,
    DrawScene, drawScene,
    Transform, transform,
    Camera, camera,

    singleton, unSingleton,
) where

import Control.Wire (Timed, Wire)
import Data.Vinyl
import Data.Vinyl.Idiom.Identity (Identity(..))
import Linear.GL

type GameSession = Timed Float ()
type PlainWire0 m b = Wire GameSession () m () b
type PlainWire m a b = Wire GameSession () m a b

fromEither :: String -> Either String b -> b
fromEither message (Left err) = error $ message ++ ": " ++ err
fromEither _ (Right res) = res

fromEitherGeneric :: String -> Either a b -> b
fromEitherGeneric message (Left _) = error message
fromEitherGeneric _ (Right res) = res

type DrawFun = Float -> IO ()

type Draw = "draw" ::: DrawFun
draw :: Draw
draw = Field

type DrawScene = "drawScene" ::: DrawFun
drawScene :: DrawScene
drawScene = Field

type Transform = "transform" ::: Mat4
transform :: Transform
transform = Field

type Camera = "camera" ::: Mat4
camera :: Camera
camera = Field

singleton :: a -> PlainRec '[sy ::: a]
singleton = (Field =:)

unSingleton :: PlainRec '[sy ::: a] -> a
unSingleton (Identity a :& _) = a