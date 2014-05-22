{-# LANGUAGE DataKinds, TypeOperators, ScopedTypeVariables, FlexibleContexts, GADTs #-}
module Main (
	main
) where

import Prelude hiding ((.), id)

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Graphics.GLUtil
import Linear.Applicative(perspective, rotation)
import Linear
import Linear.GL
import Data.Vinyl
import Control.Wire hiding ((<+>), Identity)
import Data.Traversable (sequenceA)

import Window
import Object
import Scene
import Wavefront
import Util
import Collision
import Uniforms

keyCB :: GLFW.KeyCallback
keyCB wnd GLFW.Key'Escape _ _ _ = GLFW.setWindowShouldClose wnd True
keyCB _ _ _ _ _ = return ()
--keyCB _ key scan state mods = return ()

--color :: "color" ::: Uniform (V4 GL.GLfloat)
--color = Field
texture :: "tex" ::: GL.TextureObject
texture = Field

main :: IO ()
main = withWindow setup scene cleanup
    where setup wnd = do GLFW.setKeyCallback wnd (Just keyCB)
                         obj <- loadWavefront "assets/capsule.obj"
                         tex <- fromEither "texture" <$> readTexture "assets/capsule.png"
                         generateMipmap' GL.Texture2D
                         GL.textureFilter GL.Texture2D GL.$= ((GL.Linear', Just GL.Linear'), GL.Linear')
                         return (obj, tex)
          cleanup (obj, _) = freeObject obj

scene :: GLFW.Window -> (Object, GL.TextureObject) -> Component '[] '[DrawScene]
scene wnd (obj, tex) = arr cast <<< inline (sceneRoot [child object1]) <<<
                                    inline (defaultCamera wnd) <<<
                                    polyInline (pure (transform =: camLoc)) <<<
                                    defaultObject
    
    where 
      camLoc = mkTransformationMat eye3 (V3 0 0 (-3))
      simpleObject = pure $ objRec =: obj <+> texture =: tex
      object1 = inline spinaround <<< inline defaultObject <<< simpleObject
      spinaround :: Component '[] '[Transform]
      spinaround = arr move'n'scale <<< rotation timeF
      move'n'scale mat = transform =: (mat !*! mkTransformationMat eye3 (V3 0 0 0)
                                           !*! mkTransformationMat (eye3 !!* 0.5) (V3 0 0 0))

defaultTransform :: Component '[] '[Transform]
defaultTransform = pure (transform =: eye4)

--sceenRoot' :: [Entity () '[Draw]] -> 

-- |replace the modelview with one relative to this object's location
withModelView :: Component '[Transform, Camera] '[ModelView]
withModelView = arr $ \object -> modelView =: Uniform (rGet camera object !*! rGet transform object)

child :: (Transform `IElem` atts, Obj `IElem` atts, Children `IElem` atts, HasUniforms atts) => 
          Component '[] (atts) ->
          Component '[Camera] '[Draw]
child obj = drawObject <<< arr cast <<< polyInline setAllUniforms <<< inline withModelView <<< mergeCamera obj

mergeCamera :: Component '[] outs -> Component '[Camera] (Camera ': outs)
mergeCamera obj = id &&& (arr cast >>> obj) >>> arr (uncurry (<+>))

--just rename ModelView to Camera
subScene :: Component '[ModelView] '[Camera]
subScene = arr (\(Identity (Uniform mv) :& RNil) -> camera =: mv)

sceneRoot :: [Component '[Camera] '[Draw]]
              -> Component '[Transform, Camera] '[DrawScene]
sceneRoot cs = arr combine <<< arr (map (rGet draw)) <<< sequenceA cs <<< subScene <<< withModelView
    where combine draws = drawScene =: (\alpha -> sequence_ (draws <*> pure alpha))

defaultCamera :: GLFW.Window -> Component '[] '[Camera]
defaultCamera wnd = (camera =:) <$> defaultPerspective <<< pure () --hack
    where defaultPerspective = perspective 0.1 100 (pi/2) . uncurry (/) <$> fbSize wnd