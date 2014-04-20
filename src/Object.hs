{-# LANGUAGE DataKinds, TypeOperators, FlexibleContexts, ConstraintKinds, Arrows #-}
module Object (
    Object,
    objRec,
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
import Scene

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

type Obj = "object" ::: Object 
objRec :: Obj
objRec = Field

child :: (HasUniforms r, Drawable r) => PlainRec r -> PlainRec '[Draw]
child = cast . drawObject

type Drawable r = (Transform `IElem` r, Obj `IElem` r, Children `IElem` r)

type ModelView = "modelView" ::: Uniform Mat4
modelView :: ModelView
modelView = Field

withModelView :: (Drawable r) => PlainRec r -> PlainWire Mat4 -> PlainRec (ModelView ': r)
withModelView object cam = modelView =: Uniform (cam !*! rGet transform object)
                              <+> object

drawObject :: (HasUniforms r, Drawable r) => PlainRec r -> PlainRec (Draw ': r)
drawObject object = draw =: drawObj <+> object
    where Object {objVAO = vao, objNumIndices = inds, objShader = shdr} = rGet objRec object
          doDraw :: IO () -> IO (Either e ())
          doDraw unifs = withVAO vao $ do
              GL.currentProgram $= Just (program shdr)
              unifs
              GL.drawElements GL.Triangles inds GL.UnsignedInt nullPtr
              return (Right ())
          drawObj cam = proc () -> do
                          unifs <- setAllUniforms shdr (withModelView object cam) -< ()
                          childs <- sceneRoot (camera =: (cam !*! rGet transform object) <+> object) -< ()
                          returnA -< doDraw . unifs >>=& childs