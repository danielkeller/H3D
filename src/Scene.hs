{-# LANGUAGE DataKinds, TypeOperators, ConstraintKinds, FlexibleContexts, Arrows, GADTs #-}
module Scene (
    Transform, transform,
    Draw, draw,
    ModelView, modelView,
    Camera, camera,

    DrawScene, drawScene,
    withModelView, sceneRoot
) where

import Data.Vinyl
import Linear
import Linear.GL
import Control.Wire hiding ((<+>), (.), Identity)
import Data.Traversable (sequenceA)

import Util
import Uniforms

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

type ModelView = "modelView" ::: Uniform Mat4
modelView :: ModelView
modelView = Field

-- | replace the modelview with one relative to this object's location
withModelView :: Component '[Transform, Camera] '[ModelView]
withModelView = arr $ \object -> modelView =: Uniform (rGet camera object !*! rGet transform object)

-- | draw all children
sceneRoot :: [Component '[Camera] '[Draw]]
              -> Component '[Transform, Camera] '[DrawScene]
sceneRoot cs = arr combine <<< arr (map (rGet draw)) <<< sequenceA cs <<< subScene <<< withModelView
    where combine draws = drawScene =: (\alpha -> sequence_ (draws <*> pure alpha))
          subScene :: Component '[ModelView] '[Camera]
          subScene = arr (\(Identity (Uniform mv) :& RNil) -> camera =: mv)