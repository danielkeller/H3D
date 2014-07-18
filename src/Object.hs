{-# LANGUAGE DataKinds, TypeOperators, DeriveDataTypeable #-}
module Object (
    Object(..),
    Obj, objRec,
) where

import Data.Vinyl((:::)(..))
import qualified Graphics.Rendering.OpenGL as GL (VertexArrayObject)
import Math.Mesh (Mesh)
import Graphics.GLUtil (ShaderProgram)
import Data.Typeable

data Object = Object { objMesh :: Mesh
                     , objVAO :: GL.VertexArrayObject
                     , objDraw :: IO ()
                     , freeObject :: IO ()
                     , objShader :: ShaderProgram
                     }
                     deriving (Typeable)

type Obj = "object" ::: Object 
objRec :: Obj
objRec = Field

