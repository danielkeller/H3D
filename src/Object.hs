{-# LANGUAGE DataKinds, TypeOperators, FlexibleContexts, ConstraintKinds #-}
module Object (
    Object,
    objRec,
    objXfrm,
    camera,
    Uniform(..),
    drawObject,
    freeObject,
    makeObject
) where

import Prelude hiding ((.))

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Graphics.GLUtil
import Data.Vinyl
import Data.Vinyl.Reflect
import Graphics.VinylGL hiding (setAllUniforms)
import qualified Data.Vector.Storable as V
import Linear
import Foreign.Ptr(nullPtr)

import Control.Applicative

import Control.Wire hiding ((<+>))

import Util
import Uniforms

data Object = Object { objVAO :: GL.VertexArrayObject
                     , objNumIndices :: GL.GLint
                     , freeObject :: IO ()
                     , objShader :: ShaderProgram
                     }

type ViableVertex t = (HasFieldNames t, HasFieldSizes t, HasFieldDims t,
                       HasFieldGLTypes t, V.Storable t)

makeObject :: (ViableVertex(PlainRec rs), BufferSource (V.Vector (PlainRec rs)))
              => V.Vector (PlainRec rs) -> V.Vector Word32 -> IO Object
makeObject verts faces = do
    vertBuf <- bufferVertices verts
    indBuf <- bufferIndices faces
    shdr <- simpleShaderProgram "simple.vert" "simple.frag"
    vao <- makeVAO $ do
        enableVertices' shdr vertBuf
        bindVertices vertBuf
        GL.bindBuffer GL.ElementArrayBuffer $= Just indBuf
    return Object { objVAO = vao
                  , objNumIndices = fromIntegral (V.length faces)
                  , objShader = shdr
                  , freeObject = do
                        GL.deleteObjectNames [vao]
                        deleteVertices vertBuf
                        GL.deleteObjectNames [indBuf]
                  }

--it appears that record types have to be monomorphic
type FXfrm = "transform" ::: PlainWire (M44 GL.GLfloat)
type FObject = "object" ::: Object
type FCamera = "camera" ::: PlainWire (M44 GL.GLfloat)
objXfrm :: FXfrm
objXfrm = Field
objRec :: FObject
objRec = Field
camera :: FCamera
camera = Field

type Drawable r = (FXfrm `IElem` r, FObject `IElem` r, FCamera `IElem` r)

type ModelView = "modelView" ::: Uniform (M44 GL.GLfloat)
modelView :: ModelView
modelView = Field

withModelView :: (Drawable r) => PlainRec r -> PlainRec (ModelView ': r)
withModelView record = modelView =: Uniform ((!*!) <$> rGet camera record <*> rGet objXfrm record)
                       <+> record

drawObject :: (HasUniforms r, Drawable r) => PlainRec r -> PlainWire ()
drawObject record = 
    mkGen_ (\unifs -> withVAO vao $ do
        GL.currentProgram $= Just (program shdr)
        unifs
        GL.polygonMode $= (GL.Line, GL.Line)
        GL.drawElements GL.Triangles inds GL.UnsignedInt nullPtr
        return (Right ()))
    <<< setAllUniforms shdr (withModelView record)
    where Object {objVAO = vao, objNumIndices = inds, objShader = shdr} = rGet objRec record