{-# LANGUAGE DataKinds, TypeOperators, FlexibleContexts #-}
module Scene (
    ModelView, modelView,

    withModelView,
    sceneRoot, child
) where

import Prelude hiding (id)
import Linear
import Linear.GL
import Data.Traversable (sequenceA)

import Util
import Uniforms
import Components
import Object

type ModelView = "modelView" ::: Uniform Mat4
modelView :: ModelView
modelView = Field

-- | replace the modelview with one relative to this object's location
withModelView :: Component '[Transform, Camera] '[ModelView]
withModelView = arr $ \object -> modelView =: Uniform (rGet camera object !*! rGet transform object)

-- | collapse an object's type so it can be put in a list and sent to 'sceneRoot'
child :: (Transform `IElem` atts, Obj `IElem` atts, DrawScene `IElem` atts, HasUniforms atts) => 
          Component '[Camera] atts ->
          Component '[Camera] '[Draw]
child obj = drawObject <<< arr cast <<< polyInline setAllUniforms <<< inline withModelView <<< mergedCamera
    where mergedCamera = id &&& obj >>> arr (uncurry (<+>)) --get rid of this?

-- I think sceneRoot needs to be separate from drawing (ie, entirely its own thing)
-- so we can make collision a thing

-- | draw all children
sceneRoot :: [Component '[Camera] '[Draw]]
              -> Component '[Transform, Camera] '[DrawScene]
sceneRoot cs = arr combine <<< arr (map (rGet draw)) <<< sequenceA cs <<< subScene <<< withModelView
    where combine draws = drawScene =: (\alpha -> sequence_ (draws <*> pure alpha))
          subScene :: Component '[ModelView] '[Camera]
          subScene = arr (\mv -> camera =: unUnif (unSingleton mv))