{-# LANGUAGE DataKinds, TypeOperators #-}
module Main (
	main
) where

import Prelude hiding ((.))

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Graphics.GLUtil
import Linear
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
                                $   camera =: ((!*!) <$> defaultPerspective wnd <*> pure camLoc)
                                <+> objRec =: obj
                                <+> objXfrm =: ((!*!) <$> scale . (pure 3) <*> (rotation . timeF))
                                <+> texture =: tex
                                {-
                                color =: Uniform (V4 <$> osc 0 timeF
                                                     <*> osc (2*pi/3) timeF
                                                     <*> osc (2*pi/3) timeF
                                                     <*> pure 1) -}
                            where --osc ang = fmap $ (/2) . (+1) . sin . (+ang)
                                  camLoc = mkTransformationMat (eye3) (V3 0 0 (-6))
          cleanup (obj, _) = freeObject obj

scale :: (Floating a, Epsilon a, Monad m) => Wire s e m a (M44 a)
scale = arr (\amt -> V4 (V4 amt 0 0 0) (V4 0 amt 0 0) (V4 0 0 amt 0) (V4 0 0 0 1))

rotation :: (Floating a, Epsilon a, Monad m) => Wire s e m a (M44 a)
rotation = arr (\ang -> mkTransformation (axisAngle (V3 0 1 0) ang) zero)

defaultPerspective :: Floating a => GLFW.Window -> PlainWire (M44 a)
defaultPerspective wnd = perspective 0.1 100 (pi/2) . uncurry (/) <$> fbSize wnd

perspective :: Floating a => a -> a -> a -> a -> M44 a
perspective near far fovx aspect = V4 (V4 (1/tanHalfFovx) 0 0 0)
                                      (V4 0 (aspect/tanHalfFovx) 0 0)
                                      (V4 0 0 ((far+near)*dst) (2*far*near*dst))
                                      (V4 0 0 (-1) 0)
    where tanHalfFovx = tan (fovx / 2)
          dst = 1/(near - far)