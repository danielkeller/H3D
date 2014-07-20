{-# LANGUAGE DataKinds, TypeOperators, DeriveDataTypeable, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Shader (
    GL.ShaderProgram,
    Shader, shaderRec,
    shader
) where

import Data.Vinyl

import qualified Graphics as GL
import Loader
import Components

deriving instance Typeable GL.ShaderProgram

type Shader = "shader" ::: GL.ShaderProgram 
shaderRec :: Shader
shaderRec = Field

shader :: FilePath -> Component '[] '[Shader]
shader path = resource descriptor
    where descriptor = Resource {resName = path,
                                 resLoad = const $ GL.simpleShaderProgram (path ++ ".vert") (path ++ ".frag"),
                                 resUnload = GL.deleteObjectName . GL.program}