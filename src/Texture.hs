{-# LANGUAGE DataKinds, TypeOperators, StandaloneDeriving, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Texture (
    Texture, texture,
    textureFile
) where

import qualified Graphics as GL
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
          loadIt = do tex <- fromEither "texture" <$> GL.readTexture file
                      GL.generateMipmap' GL.Texture2D --the texture will be active at this point
                      GL.textureFilter GL.Texture2D GL.$= ((GL.Linear', Just GL.Linear'), GL.Linear')
                      return tex