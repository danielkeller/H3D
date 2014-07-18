{-# LANGUAGE DataKinds, TypeOperators, DeriveDataTypeable, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Shader (
    Shader, shaderRec,
    shader
) where

import Graphics.Rendering.OpenGL.GL.ObjectName (deleteObjectName)
import Graphics.GLUtil.ShaderProgram
import Data.Vinyl
import Data.Typeable

import Loader
import Components

deriving instance Typeable ShaderProgram

type Shader = "shader" ::: ShaderProgram 
shaderRec :: Shader
shaderRec = Field

shader :: FilePath -> Component '[] '[Shader]
shader path = resource descriptor
    where descriptor = Resource {resName = path,
                                 resLoad = const $ simpleShaderProgram (path ++ ".vert") (path ++ ".frag"),
                                 resUnload = deleteObjectName . program}