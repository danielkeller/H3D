{-# LANGUAGE DataKinds, TypeOperators, ConstraintKinds, FlexibleContexts, Arrows, GADTs #-}
module Scene (
    Transform, transform,
    Children, children,
    Draw, draw,
    ModelView, modelView,
    Camera, camera,
    defaultObject,

    DrawScene, drawScene,
) where

import Data.Vinyl
import Linear.GL
import Control.Wire hiding ((<+>), (.), Identity)

import Util
import Uniforms

type Draw = "draw" ::: DrawFun
draw :: Draw
draw = Field

type DrawScene = "drawScene" ::: DrawFun
drawScene :: DrawScene
drawScene = Field

type Transform = "transform" ::: Mat4
type Children = "children" ::: DrawFun
transform :: Transform
transform = Field
children :: Children
children = Field

type Camera = "camera" ::: Mat4
camera :: Camera
camera = Field

type ModelView = "modelView" ::: Uniform Mat4
modelView :: ModelView
modelView = Field

{-
sceneRoot :: Component '[ModelView, Children] '[DrawScene]
sceneRoot = arr $ \(Identity (Uniform mv) :& Identity childs :& RNil) ->
                    let drawFuns = childs <*> pure mv
                    in  drawScene =: (sequence_ . (<*>) drawFuns . pure)
-}
-- and other stuff, eventually
defaultObject :: Component '[] '[Children]
defaultObject = pure (children =: const (return ()))