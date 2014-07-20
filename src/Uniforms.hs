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

import Data.Vinyl.Idiom.Identity
import GHC.TypeLits (SingI)
import qualified Data.Map as M

import Graphics
import Components
import Util

newtype Uniform a = Uniform {unUnif :: a}

type UnifSetter = "setUnifs" ::: (ShaderProgram -> DrawFun)
unifSetter :: UnifSetter
unifSetter = Field

setAllUniforms :: HasUniforms r => Component r '[UnifSetter]
setAllUniforms = arr help . (id &&& delay undefined)
    where help (curr, prev) = unifSetter =: help'
              where help' prgm = setUniforms curr prev (uniforms prgm)

type UnifMap = M.Map String (UniformLocation, VariableType)

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
    setUniforms _ _ unifs _ | M.null unifs = activeTexture $= TextureUnit 0
                            | otherwise = error $ "Uniforms " ++ show (M.keys unifs) ++ " not set"


unifLoc :: String -> UnifMap -> VariableType -> UniformLocation
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
          => HasUniforms (sy ::: TextureObject ': rest) where
    setUniforms (Identity tex :& rest) (_ :& rest') unifs alpha = do
            setUniforms rest rest' (M.delete name unifs) alpha
            textureBinding Texture2D $= Just tex
            tu@(TextureUnit n) <- get activeTexture
            uniform (unifLoc name unifs Sampler2D) $= tu
            activeTexture $= TextureUnit (n + 1)
        where name = show (Field :: sy ::: ())

instance HasUniforms rest => HasUniforms (thing ': rest) where
    setUniforms (_ :& rest) (_ :& rest') unifs =
        setUniforms rest rest' unifs