{-# LANGUAGE FlexibleContexts, ScopedTypeVariables,
    DataKinds, TypeOperators, GADTs, ScopedTypeVariables #-}
module Attributes (
    HasAttributes(),
    setAllAttributes,
) where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.GLUtil

import qualified Data.Vector.Storable as V
import qualified Data.Map as M
import Data.Vinyl

setAllAttributes :: HasAttributes r => ShaderProgram -> V.Vector (PlainRec r) -> IO ()
setAllAttributes shdr vector = setAttributes (uniforms shdr) vector

type AttribMap = M.Map String (GL.UniformLocation, GL.VariableType)

class V.Storable (PlainRec r) => HasAttributes r where
    setAttributes :: AttribMap -> V.Vector (PlainRec r) -> IO ()

instance HasAttributes '[] where
    setAttributes attribs _ | M.null attribs = GL.activeTexture $= GL.TextureUnit 0
                        | otherwise = error $ "Attributes " ++ show (M.keys attribs) ++ " not set"

instance (SingI sy, HasAttributes rest)
          => HasAttributes (sy ::: sort ': rest) where
    setAttributes attribs (thing :& rest) = undefined
        where name = show (Field :: sy ::: ())
              realTy = unifType (undefined :: sort)