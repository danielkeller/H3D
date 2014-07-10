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
import Data.Vinyl
import Control.Wire hiding ((<+>), Identity)

import Window
import Object
import Scene
import Wavefront
import Util
import Collision

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
                         let aabb = buildBVH (objMesh obj)
                         bvhObj <- simpleShaderProgram "assets/color.vert" "assets/color.frag"
                                      >>= makeWireframe (bvhWireframe aabb)
                         return (obj, tex, bvhObj)
          cleanup (obj, _, bvhObj) = freeObject obj >> freeObject bvhObj

scene :: GLFW.Window -> (Object, GL.TextureObject, Object) -> Component '[] '[DrawScene]
scene wnd (obj, tex, bvhObj) = arr cast <<< sceneRoot [child object1, child bvhObject1] <<<<
                                    defaultCamera wnd <<<<
                                    transform =:< camLoc
    
    where 
      camLoc = mkTransformationMat eye3 (V3 0 0 (-2))
      --this is the least grungy, but grungy nonetheless
      simpleObject = {-withBVH <<<< -} objRec =:< obj <<<< texture =:< tex <<<< id
      bvhObject = {-withBVH <<<< -} objRec =:< bvhObj <<<< id
      -- drawBVHOf (arr cast <<< object2) -- broken
      object1 = sceneRoot [child object2] <<<< spinaround <<<< simpleObject
      object2 = sceneRoot [] <<<< spinaround2 <<<< simpleObject
      bvhObject1 = sceneRoot [] <<<< spinaround <<<< bvhObject
      spinaround = arr move'n'scale <<< rotation (timeF / 2) <<< void
      spinaround2 = arr move'n'scale2 <<< flip mkTransformation 0 <$> axisAngle (V3 1 0 0) <$> 2 * timeF <<< void
      move'n'scale mat = transform =: (mat !*! mkTransformationMat eye3 (V3 0 0 0)
                                           !*! mkTransformationMat (eye3 !!* 0.5) (V3 0 0 0))
      move'n'scale2 mat = transform =: (mat !*! mkTransformationMat eye3 (V3 0 0 2)
                                            !*! mkTransformationMat (eye3 !!* 0.5) (V3 0 0 0))

defaultCamera :: GLFW.Window -> Component '[] '[Camera]
defaultCamera wnd = (camera =:) <$> defaultPerspective <<< pure () --hack
    where defaultPerspective = perspective 0.1 100 (pi/2) . uncurry (/) <$> fbSize wnd