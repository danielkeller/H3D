{-# LANGUAGE DataKinds, TypeOperators #-}
module Main (
	main
) where

import Prelude hiding ((.))

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Graphics.GLUtil
import Linear.Applicative
import Data.Vinyl
import Control.Wire hiding ((<+>))

import Window
import Object
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
main = withWindow setup action cleanup
    where setup wnd = do GLFW.setKeyCallback wnd (Just keyCB)
                         obj <- loadWavefront "assets/capsule.obj"
                         tex <- fromEither "texture" <$> readTexture "assets/capsule.png"
                         GL.textureFilter GL.Texture2D GL.$= ((GL.Linear', Nothing), GL.Linear')
                         return (obj, tex)
          action wnd (obj, tex) = drawObject
                                $   camera =: (defaultPerspective wnd !*! camLoc)
                                <+> objRec =: obj
                                <+> objXfrm =: (scale 3 !*! rotation timeF)
                                <+> texture =: tex
                            where camLoc = mkTransformationMat eye3 (vec3 0 0 (-6))
          cleanup (obj, _) = freeObject obj

defaultPerspective :: Floating a => GLFW.Window -> PlainWire (M44 a)
defaultPerspective wnd = perspective 0.1 100 (pi/2) . uncurry (/) <$> fbSize wnd