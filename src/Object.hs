{-# LANGUAGE DataKinds, TypeOperators, FlexibleContexts, ConstraintKinds, Arrows, ScopedTypeVariables, StandaloneDeriving #-}
module Object (
    Pos, Mesh(..),
    Object,
    objMesh,
    Obj, objRec,
    draw,
    Uniform(..),
    Draw,
    PrimitiveMode(..),
    drawObject,
    freeObject,
    makeObject,
    makeWireframe,
    child
) where

import Prelude hiding ((.), foldr, id)

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL(PrimitiveMode)
import Graphics.Rendering.OpenGL (($=))
import Graphics.GLUtil
import Data.Vinyl
import Data.Vinyl.Reflect
import Graphics.VinylGL hiding (setAllUniforms)
import qualified Data.Vector.Storable as V
import Linear.GL
import Foreign.Ptr(nullPtr)

import Control.Wire hiding ((<+>))

import Util
import Uniforms
import Scene
import Math.Mesh

--this is getting kind of messy
type Pos = "position" ::: Vec3

data Object = Object { objMesh :: Mesh
                     , objVAO :: GL.VertexArrayObject
                     , objDraw :: IO ()
                     , freeObject :: IO ()
                     , objShader :: ShaderProgram
                     }

type ViableVertex t = (HasFieldNames t, HasFieldSizes t, HasFieldDims t,
                       HasFieldGLTypes t, V.Storable t)

--TODO: make a class for index types that gives the appropriate draw mode

-- | Make a simple wireframe from a line list
makeWireframe :: (ViableVertex (PlainRec rs), BufferSource (V.Vector (PlainRec rs)), Pos `IElem` rs)
              => V.Vector (PlainRec rs) -> ShaderProgram -> IO Object
makeWireframe verts shdr = do
    vertBuf <- bufferVertices verts
    vao <- makeVAO $ do
        enableVertices' shdr vertBuf
        bindVertices vertBuf
    return Object { objMesh = mesh
                  , objVAO = vao
                  , objDraw = GL.drawArrays GL.Lines 0 (fromIntegral (V.length verts))
                  , objShader = shdr
                  , freeObject = do
                        GL.deleteObjectNames [vao]
                        deleteVertices vertBuf
                  }
    where mesh = error "Please do not use this :)"

-- | Make an object with indices and attributes
makeObject :: (ViableVertex (PlainRec rs), BufferSource (V.Vector (PlainRec rs)), Pos `IElem` rs)
              => V.Vector (PlainRec rs) -> V.Vector TriInd -> ShaderProgram -> IO Object
makeObject verts faces shdr = do
    vertBuf <- bufferVertices verts
    indBuf <- bufferIndices faceWords
    vao <- makeVAO $ do
        enableVertices' shdr vertBuf --this doesn't complain about extra attributes
        bindVertices vertBuf
        GL.bindBuffer GL.ElementArrayBuffer $= Just indBuf
    return Object { objMesh = mesh
                  , objVAO = vao
                  , objDraw = GL.drawElements GL.Triangles (fromIntegral (V.length faceWords)) GL.UnsignedInt nullPtr
                  , objShader = shdr
                  , freeObject = do
                        GL.deleteObjectNames [vao]
                        deleteVertices vertBuf
                        GL.deleteObjectNames [indBuf]
                  }
    where mesh = Mesh (V.map (rGet (Field :: Pos)) verts) faces
          faceWords :: V.Vector Word32
          faceWords = V.unsafeCast faces

type Obj = "object" ::: Object 
objRec :: Obj
objRec = Field

drawObject :: Component '[Obj, DrawScene, UnifSetter] '[Draw]
drawObject = arr ((draw =:) . doDraw)
    where doDraw object alpha = do
            rGet drawScene object alpha
            withVAO vao $ do
              GL.currentProgram $= Just (program shdr)
              rGet unifSetter object shdr alpha
              drawIt
            where Object {objVAO = vao, objDraw = drawIt, objShader = shdr} = rGet objRec object

-- | collapse an object's type so it can be put in a list and sent to 'sceneRoot'
child :: (Transform `IElem` atts, Obj `IElem` atts, DrawScene `IElem` atts, HasUniforms atts) => 
          Component '[Camera] atts ->
          Component '[Camera] '[Draw]
child obj = drawObject <<< arr cast <<< polyInline setAllUniforms <<< inline withModelView <<< mergedCamera
    where mergedCamera = id &&& obj >>> arr (uncurry (<+>)) --get rid of this?