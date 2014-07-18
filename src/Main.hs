{-# LANGUAGE DataKinds, TypeOperators, ScopedTypeVariables, FlexibleContexts #-}
module Main (
	main
) where

import Prelude hiding ((.), id)

import qualified Graphics.UI.GLFW as GLFW
import Linear.Applicative(perspective, rotation)
import Linear
import Data.Vinyl
import Control.Wire hiding ((<+>), Identity)

import Components
import Window
import Scene
import Wavefront
import Util
import Collision
import Texture
import Shader

keyCB :: GLFW.KeyCallback
keyCB wnd GLFW.Key'Escape _ _ _ = GLFW.setWindowShouldClose wnd True
keyCB _ _ _ _ _ = return ()
--keyCB _ key scan state mods = return ()

main :: IO ()
main = withWindow setup scene
    where setup wnd = do GLFW.setKeyCallback wnd (Just keyCB)

scene :: GLFW.Window -> Component '[] '[DrawScene]
scene wnd = arr cast <<< sceneRoot [child object1, obj1Bvh] <<<<
                                    defaultCamera wnd <<<<
                                    transform =:< camLoc
    
    where 
      camLoc = mkTransformationMat eye3 (V3 0 0 (-2))
      --this is the least grungy, but grungy nonetheless
      simpleObject = withBVH <<<< wavefrontObject "assets/capsule.obj" <<<< textureFile "assets/capsule.png"
                      <<<< shader "assets/simple" <<<< id
      obj1Bvh = drawBVHOf (arr cast <<< object1)
      object1 = sceneRoot [child object2] <<<< spinaround <<<< simpleObject
      object2 = sceneRoot [] <<<< spinaround2 <<<< simpleObject
      spinaround = arr move'n'scale <<< rotation (timeF / 2) <<< void
      spinaround2 = arr move'n'scale2 <<< flip mkTransformation 0 <$> axisAngle (V3 1 0 0) <$> 2 * timeF <<< void
      move'n'scale mat = transform =: (mat !*! mkTransformationMat eye3 (V3 0 0 0)
                                           !*! mkTransformationMat (eye3 !!* 0.5) (V3 0 0 0))
      move'n'scale2 mat = transform =: (mat !*! mkTransformationMat eye3 (V3 0 0 2)
                                            !*! mkTransformationMat (eye3 !!* 0.5) (V3 0 0 0))

defaultCamera :: GLFW.Window -> Component '[] '[Camera]
defaultCamera wnd = (camera =:) <$> defaultPerspective <<< pure () --hack
    where defaultPerspective = perspective 0.1 100 (pi/2) . uncurry (/) <$> fbSize wnd