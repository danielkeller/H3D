{-# LANGUAGE FlexibleInstances, OverlappingInstances, Arrows,
    DataKinds, TypeOperators, GADTs, ScopedTypeVariables #-}
module Uniforms (
    HasUniforms(..),
    setAllUniforms,
    Uniform(..)
) where

import qualified Graphics.Rendering.OpenGL as GL
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

class HasUniforms r where
    setUniforms :: UnifMap -> PlainRec r -> PlainWire (IO ())

instance HasUniforms '[] where
    setUniforms unifs _ | M.null unifs = pure (return ())
                        | otherwise = error $ "Uniforms " ++ show (M.keys unifs) ++ " not set"

instance (SingI sy, HasUniforms rest, AsUniform sort, HasVariableType sort)
          => HasUniforms (sy ::: Uniform sort ': rest) where
    setUniforms unifs (Identity (Uniform thing) :& rest) =
        proc () -> do
            val <- thing -< ()
            others <- setUniforms (M.delete name unifs) rest -< ()
            returnA -< others >> asUniform val unifLoc
        where name = show (Field :: sy ::: ())
              realTy = variableType (undefined :: sort)
              unifLoc = case M.lookup name unifs of
                    Nothing -> error $ "Uniform '" ++ name ++ "' not in shader program "
                    Just (loc, ty) | ty == realTy -> loc
                    Just (_,   ty) -> error $ "Uniform '" ++ name ++ "' has type '"
                                              ++ show realTy ++ "' but shader expects '"
                                              ++ show ty ++ "'"

instance HasUniforms rest => HasUniforms (thing ': rest) where
    setUniforms unifs (_ :& rest) = setUniforms unifs rest