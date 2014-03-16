{-# LANGUAGE DataKinds, TypeOperators, FlexibleContexts, ConstraintKinds, Arrows #-}
module Object (
    Object,
    objRec,
    objXfrm,
    objChildren,
    child,
    draw,
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
import Linear.Applicative
import Linear.GL
import Foreign.Ptr(nullPtr)

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

makeObject :: (ViableVertex (PlainRec rs), BufferSource (V.Vector (PlainRec rs)))
              => V.Vector (PlainRec rs) -> V.Vector Word32 -> IO Object
makeObject verts faces = do
    vertBuf <- bufferVertices verts
    indBuf <- bufferIndices faces
    shdr <- simpleShaderProgram "assets/simple.vert" "assets/simple.frag"
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
type Xfrm = "transform" ::: PlainWire Mat4
type Obj = "object" ::: Object
type Children = "children" ::: [PlainRec '[Draw]]
objXfrm :: Xfrm
objXfrm = Field
objRec :: Obj
objRec = Field
objChildren :: Children
objChildren = Field

child :: (HasUniforms r, Drawable r) => PlainRec r -> PlainRec '[Draw]
child = cast . drawObject

type Drawable r = (Xfrm `IElem` r, Obj `IElem` r, Children `IElem` r)

type Draw = "draw" ::: (PlainWire Mat4 -> PlainWire ())
draw :: Draw
draw = Field

type ModelView = "modelView" ::: Uniform Mat4
modelView :: ModelView
modelView = Field

withModelView :: (Drawable r) => PlainRec r -> PlainWire Mat4 -> PlainRec (ModelView ': r)
withModelView record camera = modelView =: Uniform (camera !*! rGet objXfrm record)
                       <+> record

drawObject :: (HasUniforms r, Drawable r) => PlainRec r -> PlainRec (Draw ': r)
drawObject record = draw =: getMv <+> record
    where Object {objVAO = vao, objNumIndices = inds, objShader = shdr} = rGet objRec record
          doDraw :: IO () -> IO (Either e ())
          doDraw unifs = withVAO vao $ do
              GL.currentProgram $= Just (program shdr)
              unifs
              GL.drawElements GL.Triangles inds GL.UnsignedInt nullPtr
              return (Right ())
          getMv camera = (mkGen_ doDraw <<< setAllUniforms shdr (withModelView record camera))
                         >>> rest (rGet objChildren record) 
              where rest [] = pure ()
                    rest (c:cs) = (rGet draw c (camera !*! rGet objXfrm record)) >>> rest cs