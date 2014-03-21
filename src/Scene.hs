{-# LANGUAGE DataKinds, TypeOperators, ConstraintKinds, FlexibleContexts, Arrows #-}
module Scene (
    Transform, transform,
    Children, children,
    Draw, draw,
    Camera, camera,

    sceneRoot
) where

import Data.Vinyl
import Linear.GL
import Control.Wire hiding ((<+>), (.))

import Util

type Draw = "draw" ::: (PlainWire Mat4 -> PlainWire DrawFun)
draw :: Draw
draw = Field

type Transform = "transform" ::: PlainWire Mat4
type Children = "children" ::: [PlainRec '[Draw]]
transform :: Transform
transform = Field
children :: Children
children = Field

type Camera = "camera" ::: PlainWire Mat4
camera :: Camera
camera = Field
type Scene r = (Camera `IElem` r, Children `IElem` r)

--there's probably a nicer way to do this
sceneRoot :: Scene r => PlainRec r -> PlainWire DrawFun
sceneRoot scene = rest $ map (rGet draw) (rGet children scene) <*> [rGet camera scene]
      where rest [] = pure (\_ -> return ())
            rest (c:cs) = proc () -> do
                            draw1 <- c -< ()
                            drawRest <- rest cs -< ()
                            returnA -< \alpha -> draw1 alpha >> drawRest alpha