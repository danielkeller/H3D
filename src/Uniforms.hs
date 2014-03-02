{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Uniforms (
) where

import Graphics.GLUtil
import Graphics.Rendering.OpenGL.GL.Shaders.Attribs

--convert 'no instance for' into runtime error
instance AsUniform a where
    asUniform _ loc = error $ "Uniform set to location "
                      ++ show loc ++ " does not have a gl compatible type."
--convert 'no instance for' into runtime error
instance HasVariableType a where
    variableType _ = Int' --error $ "Uniform does not have a gl compatible type."