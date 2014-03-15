{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, DefaultSignatures #-}
module Data.Vec.OpenGL (
    VUniform(),
    unifType,
    toUniform,

    Mat4,
    Vec2, Vec3, Vec4
) where

import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.OpenGL.GL
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Utils
import Foreign.Marshal.Array

import qualified Data.Vec as V

class VUniform a where
    unifType :: a -> VariableType
    toUniformV :: GLint -> GLsizei -> Ptr a -> IO ()

    toUniform :: a -> UniformLocation -> IO ()
    default toUniform :: Storable a => a -> UniformLocation -> IO ()
    toUniform val (UniformLocation loc) = with val (toUniformV loc 1)

instance (Storable a, VUniform a) => VUniform [a] where
    unifType _ = unifType (undefined :: a)
    toUniformV = error "VUniform.toUniformV is undefined"
    
    toUniform val (UniformLocation loc) = withArray val $ toUniformV loc $ fromIntegral $ length val

type Mat4 = V.Mat44 GLfloat
type Vec4 = V.Vec4 GLfloat
type Vec3 = V.Vec3 GLfloat
type Vec2 = V.Vec2 GLfloat

instance VUniform Mat4 where
    unifType _ = FloatMat4
    toUniformV loc num val = glUniformMatrix4fv loc num 1 (castPtr val :: Ptr GLfloat)

instance VUniform Vec4 where
    unifType _ = FloatVec4
    toUniformV loc num val = glUniform4fv loc num (castPtr val :: Ptr GLfloat)

instance VUniform Vec3 where
    unifType _ = FloatVec3
    toUniformV loc num val = glUniform3fv loc num (castPtr val :: Ptr GLfloat)

instance VUniform Vec2 where
    unifType _ = FloatVec2
    toUniformV loc num val = glUniform2fv loc num (castPtr val :: Ptr GLfloat)