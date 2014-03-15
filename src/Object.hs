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
import qualified Data.Vector.Storable as V
import Foreign.Ptr(nullPtr)
--import Foreign.Storable(sizeOf)

import Data.Vec.OpenGL
import Data.Vec.Applicative

import Control.Wire hiding ((<+>))

import Util
import Uniforms
import Attributes

data Object = Object { objVAO :: GL.VertexArrayObject
                     , objNumIndices :: GL.GLint
                     , freeObject :: IO ()
                     , objShader :: ShaderProgram
                     }

makeObject :: (HasAttributes rs) => V.Vector (PlainRec rs) -> V.Vector Word32 -> IO Object
makeObject verts faces = do
    shdr <- simpleShaderProgram "assets/simple.vert" "assets/simple.frag"
    vertBuf <- fromSource GL.ArrayBuffer verts
    indBuf <- bufferIndices faces
    vao <- makeVAO $ do
        enableAttrib shdr "position"
        setAttrib shdr "position" GL.ToFloat
            $ GL.VertexArrayDescriptor 4 GL.Float 0 nullPtr -- (fromIntegral (sizeOf (V.head verts)) - 16) nullPtr
        GL.bindBuffer GL.ElementArrayBuffer $= Just indBuf
    return Object { objVAO = vao
                  , objNumIndices = fromIntegral (V.length faces)
                  , objShader = shdr
                  , freeObject = do
                        GL.deleteObjectNames [vao]
                        GL.deleteObjectNames [vertBuf]
                        GL.deleteObjectNames [indBuf]
                  }

--it appears that record types have to be monomorphic
type FXfrm = "transform" ::: PlainWire Mat4
type FObject = "object" ::: Object
type FCamera = "camera" ::: PlainWire Mat4
objXfrm :: FXfrm
objXfrm = Field
objRec :: FObject
objRec = Field
camera :: FCamera
camera = Field

type Drawable r = (FXfrm `IElem` r, FObject `IElem` r, FCamera `IElem` r)

type ModelView = "modelView" ::: Uniform Mat4
modelView :: ModelView
modelView = Field

withModelView :: (Drawable r) => PlainRec r -> PlainRec (ModelView ': r)
withModelView record = modelView =: Uniform (rGet camera record `multmm` rGet objXfrm record)
                       <+> record

drawObject :: (HasUniforms r, Drawable r) => PlainRec r -> PlainWire ()
drawObject record = 
    mkGen_ (\unifs -> withVAO vao $ do
        GL.currentProgram $= Just (program shdr)
        unifs
        GL.drawElements GL.Triangles inds GL.UnsignedInt nullPtr
        return (Right ()))
    <<< setAllUniforms shdr (withModelView record)
    where Object {objVAO = vao, objNumIndices = inds, objShader = shdr} = rGet objRec record