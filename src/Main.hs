{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, DataKinds, TypeOperators #-}
module Main (
	main
) where

import Prelude hiding ((.))

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Linear
import Control.Wire hiding ((<+>))

import Data.Record

import Window
import Object
import Util

keyCB :: GLFW.KeyCallback
keyCB wnd GLFW.Key'Escape _ _ _ = GLFW.setWindowShouldClose wnd True
keyCB _ _ _ _ _ = return ()
--keyCB _ key scan state mods = return ()

data Color = Color deriving Show
instance Name Color where name = Color

main :: IO ()
main = withWindow setup action cleanup
    where setup wnd = do GLFW.setKeyCallback wnd (Just keyCB)
                         loadObject "teapot.obj"
          action _ teapot = drawObject $ X
                                :& Xfrm := (rotation . timeF)
                                :& Obj := teapot
                                :& Camera := pure (mkTransformationMat (eye3 !!* 0.2) (V3 0 0 0))
                                :& Uniform Color := (V4 <$> osc 0 timeF
                                                        <*> osc (2*pi/3) timeF
                                                        <*> osc (2*pi/3) timeF
                                                        <*> pure 1 :: PlainWire (V4 GL.GLfloat))
                            where osc ang = fmap $ (/2) . (+1) . sin . (+ang)
          cleanup teapot = freeObject teapot

rotation :: (Floating a, Epsilon a, Monad m) => Wire s e m a (M44 a)
rotation = arr (\ang -> mkTransformation (axisAngle (V3 0 1 0) ang) zero)