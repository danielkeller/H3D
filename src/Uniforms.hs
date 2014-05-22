{-# LANGUAGE FlexibleInstances, OverlappingInstances, Arrows,
    DataKinds, TypeOperators, GADTs, ScopedTypeVariables #-}
-- | This module is neccesary because vinyl-gl's setAllUniforms
-- barfs if something in the record doesn't have a gl type
module Uniforms (
    HasUniforms(..),
    setAllUniforms,
    Uniform(..),
    UnifSetter, unifSetter
) where

import Prelude hiding (id, (.))

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Data.Vinyl
import Data.Vinyl.Idiom.Identity
import Control.Wire hiding ((<+>), Identity)
import Graphics.GLUtil
import GHC.TypeLits (SingI)
import qualified Data.Map as M

import Util

newtype Uniform a = Uniform {unUnif :: a}

type UnifSetter = "setUnifs" ::: (ShaderProgram -> DrawFun)
unifSetter :: UnifSetter
unifSetter = Field

setAllUniforms :: HasUniforms r => Component r '[UnifSetter]
setAllUniforms = arr ((unifSetter =:) . (.uniforms) --convert the ShaderProgram to a UnifMap
                      . uncurry setUniforms) --just the first two arguments
               . (id &&& delay undefined)

type UnifMap = M.Map String (GL.UniformLocation, GL.VariableType)

{-
  The texuring setup is kind of a hack. We set the first active texutre in the base case,
  then increment it each time a new txture is set.
-}

class HasUniforms r where
    setUniforms :: PlainRec r -- ^ Current uniforms
                -> PlainRec r -- ^ Last uniforms
                -> UnifMap -- ^ Uniform locations
                -> DrawFun

instance HasUniforms '[] where
    setUniforms _ _ unifs _ | M.null unifs = GL.activeTexture $= GL.TextureUnit 0
                            | otherwise = error $ "Uniforms " ++ show (M.keys unifs) ++ " not set"


unifLoc :: String -> UnifMap -> GL.VariableType -> GL.UniformLocation
unifLoc name unifs realTy = case M.lookup name unifs of
      Nothing -> error $ "Uniform '" ++ name ++ "' not used in shader program "
      Just (loc, ty) | ty == realTy -> loc
      Just (_,   ty) -> error $ "Uniform '" ++ name ++ "' has type '"
                                ++ show realTy ++ "' but shader expects '"
                                ++ show ty ++ "'"

--case for regular uniforms
instance (SingI sy, HasUniforms rest, AsUniform sort, Fractional sort, HasVariableType sort)
          => HasUniforms (sy ::: Uniform sort ': rest) where
    setUniforms (Identity (Uniform val) :& rest)
                (Identity (Uniform val') :& rest') unifs alpha = do
          setUniforms rest rest' (M.delete name unifs) alpha
          --FIXME: lerp is wrong for matricies
          let blended = val' * (1 - realToFrac alpha) + val * realToFrac alpha
          asUniform blended (unifLoc name unifs realTy)
        where name = show (Field :: sy ::: ())
              realTy = variableType (undefined :: sort)

--case for textures
instance (SingI sy, HasUniforms rest)
          => HasUniforms (sy ::: GL.TextureObject ': rest) where
    setUniforms (Identity tex :& rest) (_ :& rest') unifs alpha = do
            setUniforms rest rest' (M.delete name unifs) alpha
            GL.textureBinding GL.Texture2D $= Just tex
            tu@(GL.TextureUnit n) <- GL.get GL.activeTexture
            GL.uniform (unifLoc name unifs GL.Sampler2D) $= tu
            GL.activeTexture $= GL.TextureUnit (n + 1)
        where name = show (Field :: sy ::: ())

instance HasUniforms rest => HasUniforms (thing ': rest) where
    setUniforms (_ :& rest) (_ :& rest') unifs =
        setUniforms rest rest' unifs