{-# LANGUAGE FlexibleInstances, OverlappingInstances, Arrows,
    DataKinds, TypeOperators, GADTs, ScopedTypeVariables #-}
module Uniforms (
    HasUniforms(..),
    setAllUniforms,
    Uniform(..)
) where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Data.Vinyl
import Control.Wire hiding ((<+>))
import Graphics.GLUtil
import GHC.TypeLits (SingI)
import qualified Data.Map as M

import Util

newtype Uniform a = Uniform (PlainWire a)

setAllUniforms :: HasUniforms r => ShaderProgram -> PlainRec r -> PlainWire (IO ())
setAllUniforms shdr record = setUniforms (uniforms shdr) record

type UnifMap = M.Map String (GL.UniformLocation, GL.VariableType)

{-
  The texuring setup is kind of a hack. We set the first active texutre in the base case,
  then increment it each time a new txture is set.
-}

class HasUniforms r where
    setUniforms :: UnifMap -> PlainRec r -> PlainWire (IO ())

instance HasUniforms '[] where
    setUniforms unifs _ | M.null unifs = pure $ GL.activeTexture $= GL.TextureUnit 0
                        | otherwise = error $ "Uniforms " ++ show (M.keys unifs) ++ " not set"


unifLoc :: String -> UnifMap -> GL.VariableType -> GL.UniformLocation
unifLoc name unifs realTy = case M.lookup name unifs of
      Nothing -> error $ "Uniform '" ++ name ++ "' not used in shader program "
      Just (loc, ty) | ty == realTy -> loc
      Just (_,   ty) -> error $ "Uniform '" ++ name ++ "' has type '"
                                ++ show realTy ++ "' but shader expects '"
                                ++ show ty ++ "'"

--case for regular uniforms
instance (SingI sy, HasUniforms rest, AsUniform sort, HasVariableType sort)
          => HasUniforms (sy ::: Uniform sort ': rest) where
    setUniforms unifs (Identity (Uniform thing) :& rest) =
        proc () -> do
            val <- thing -< ()
            others <- setUniforms (M.delete name unifs) rest -< ()
            returnA -< others >> asUniform val (unifLoc name unifs realTy)
        where name = show (Field :: sy ::: ())
              realTy = variableType (undefined :: sort)

--case for textures
instance (SingI sy, HasUniforms rest)
          => HasUniforms (sy ::: GL.TextureObject ': rest) where
    setUniforms unifs (Identity tex :& rest) =
        proc () -> do
            others <- setUniforms (M.delete name unifs) rest -< ()
            returnA -< do others
                          GL.textureBinding GL.Texture2D $= Just tex
                          tu@(GL.TextureUnit n) <- GL.get GL.activeTexture
                          GL.uniform (unifLoc name unifs GL.Sampler2D) $= tu
                          GL.activeTexture $= GL.TextureUnit (n + 1)
        where name = show (Field :: sy ::: ())

instance HasUniforms rest => HasUniforms (thing ': rest) where
    setUniforms unifs (_ :& rest) = setUniforms unifs rest