{-# LANGUAGE DataKinds, TypeOperators, StandaloneDeriving, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Texture (
    Texture, texture,
    textureFile
) where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.GLUtil
import Data.Vinyl
import Control.Applicative
import Data.Typeable (Typeable)

import Components
import Loader
import Util

deriving instance Typeable GL.TextureObject

type Texture = "tex" ::: GL.TextureObject
texture :: Texture
texture = Field

textureFile :: FilePath -> Component '[] '[Texture]
textureFile file = resource descriptor
    where descriptor = Resource {resName = file,
                                 resLoad = const loadIt,
                                 resUnload = GL.deleteObjectName}
          loadIt = do tex <- fromEither "texture" <$> readTexture file
                      generateMipmap' GL.Texture2D --the texture will be active at this point
                      GL.textureFilter GL.Texture2D GL.$= ((GL.Linear', Just GL.Linear'), GL.Linear')
                      return tex