-- | OpenGL type synonyms
module Linear.GL(
    Vec2, Vec3, Vec4,
    Mat2, Mat3, Mat4
) where

import qualified Graphics.Rendering.OpenGL as GL
import Linear (V2(..), V3(..), V4(..), M22, M44, M33)

type Vec2 = V2 GL.GLfloat
type Vec3 = V3 GL.GLfloat
type Vec4 = V4 GL.GLfloat

type Mat2 = M22 GL.GLfloat
type Mat3 = M33 GL.GLfloat
type Mat4 = M44 GL.GLfloat