{-# LANGUAGE DataKinds, TypeOperators, ConstraintKinds, FlexibleContexts #-}
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

type Draw = "draw" ::: (PlainWire Mat4 -> PlainWire ())
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

sceneRoot :: Scene r => PlainRec r -> PlainWire ()
sceneRoot scene = rest (rGet children scene) 
      where rest [] = pure ()
            rest (c:cs) = (rGet draw c (rGet camera scene)) >>> rest cs