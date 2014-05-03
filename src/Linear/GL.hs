{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | OpenGL type synonyms
module Linear.GL(
    Vec2, Vec3, Vec4,
    Mat2, Mat3, Mat4,
    CFloat(..)
) where

import qualified Graphics.Rendering.OpenGL as GL
import Foreign.C.Types(CFloat(..))
import Linear (V2(..), V3(..), V4(..), M22, M44, M33)

type Vec2 = V2 GL.GLfloat
type Vec3 = V3 GL.GLfloat
type Vec4 = V4 GL.GLfloat

type Mat2 = M22 GL.GLfloat
type Mat3 = M33 GL.GLfloat
type Mat4 = M44 GL.GLfloat

--Floats are not bounded for some reason
instance Bounded CFloat where
    minBound = CFloat (-(1/0))
    maxBound = CFloat (1/0)

instance Bounded a => Bounded (V2 a) where
    minBound = V2 minBound minBound
    maxBound = V2 maxBound maxBound
instance Bounded a => Bounded (V3 a) where
    minBound = V3 minBound minBound minBound
    maxBound = V3 maxBound maxBound maxBound
instance Bounded a => Bounded (V4 a) where
    minBound = V4 minBound minBound minBound minBound
    maxBound = V4 maxBound maxBound maxBound maxBound