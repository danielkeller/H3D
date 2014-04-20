{-# LANGUAGE DataKinds, TypeOperators #-}
module Main (
	main
) where

import Prelude hiding ((.))

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Graphics.GLUtil
import Linear.Applicative
import Linear.GL
import Data.Vinyl
import Control.Wire hiding ((<+>))

import Window
import Object
import Scene
import Wavefront
import Util

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

scene :: GLFW.Window -> (Object, GL.TextureObject) -> PlainWire (Float -> IO ())
scene wnd (obj, tex) = sceneRoot $
    camera =: (defaultPerspective wnd !*! camLoc) <+>
    children =: [
        child $ simpleObject <+>
        transform =: eye4 <+>
        children =: [
            child $ simpleObject <+>
            transform =: spinaround <+>
            children =: [
                child $ simpleObject <+>
                transform =: spinaround2 <+>
                children =: []
            ],
            child $ simpleObject <+>
            transform =: spinaround2 <+>
            children =: []
        ]
    ]
    where camLoc = mkTransformationMat eye3 (vec3 0 0 (-6))
          spinaround = rotation timeF
                       !*! mkTransformationMat eye3 (vec3 3 0 0)
                       !*! scale 0.5
          spinaround2 = rotation (3 * timeF)
                       !*! mkTransformationMat eye3 (vec3 2 0 0)
                       !*! scale 0.5
          simpleObject = objRec =: obj <+> texture =: tex

defaultPerspective :: GLFW.Window -> PlainWire Mat4
defaultPerspective wnd = perspective 0.1 100 (pi/2) . uncurry (/) <$> fbSize wnd