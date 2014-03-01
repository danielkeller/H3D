{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
module Main (
	main
) where

import Prelude hiding ((.))

import qualified Graphics.UI.GLFW as GLFW
import Linear
import Data.Vinyl
import Control.Wire hiding ((<+>))

import Window
import Object

keyCB :: GLFW.KeyCallback
keyCB wnd GLFW.Key'Escape _ _ _ = GLFW.setWindowShouldClose wnd True
keyCB _ _ _ _ _ = return ()
--keyCB _ key scan state mods = return ()

main :: IO ()
main = withWindow setup action cleanup
    where setup wnd = do GLFW.setKeyCallback wnd (Just keyCB)
                         loadObject "teapot.obj"
          action _ teapot = drawObject $
                                camera =: pure (mkTransformationMat (eye3 !!* 0.2) (V3 0 0 0)) <+>
                                objRec =: teapot <+>
                                objXfrm =: (rotation . timeF)
          cleanup teapot = freeObject teapot

rotation :: (Floating a, Epsilon a, Monad m) => Wire s e m a (M44 a)
rotation = arr (\ang -> mkTransformation (axisAngle (V3 0 1 0) ang) zero)