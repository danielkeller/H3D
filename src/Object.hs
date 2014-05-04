{-# LANGUAGE DataKinds, TypeOperators, FlexibleContexts, ConstraintKinds, Arrows #-}
module Object (
    Pos, Mesh(..),
    Object,
    objMesh,
    Obj, objRec,
    child,
    draw,
    Uniform(..),
    Draw,
    PrimitiveMode(..),
    drawObject,
    freeObject,
    makeObject
) where

import Prelude hiding ((.), foldr)

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL(PrimitiveMode)
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
import Math.Mesh

--this is getting kind of messy
type Pos = "position" ::: Vec3

data Object = Object { objMesh :: Mesh
                     , objVAO :: GL.VertexArrayObject
                     , objMode :: GL.PrimitiveMode
                     , objNumIndices :: GL.GLint
                     , freeObject :: IO ()
                     , objShader :: ShaderProgram
                     }

type ViableVertex t = (HasFieldNames t, HasFieldSizes t, HasFieldDims t,
                       HasFieldGLTypes t, V.Storable t)

makeObject :: (ViableVertex (PlainRec rs), BufferSource (V.Vector (PlainRec rs)), Pos `IElem` rs)
              => V.Vector (PlainRec rs) -> V.Vector TriInd -> GL.PrimitiveMode -> IO Object
makeObject verts faces mode = do
    vertBuf <- bufferVertices verts
    indBuf <- bufferIndices faceWords
    shdr <- simpleShaderProgram "assets/simple.vert" "assets/simple.frag"
    vao <- makeVAO $ do
        enableVertices' shdr vertBuf
        bindVertices vertBuf
        GL.bindBuffer GL.ElementArrayBuffer $= Just indBuf
    return Object { objMesh = mesh
                  , objVAO = vao
                  , objMode = mode
                  , objNumIndices = fromIntegral (V.length faceWords)
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
drawObject object = draw =: getMv <+> object
    where Object {objVAO = vao, objMode = mode, objNumIndices = inds, objShader = shdr} = rGet objRec object
          doDraw :: IO () -> IO (Either e ())
          doDraw unifs = withVAO vao $ do
              GL.currentProgram $= Just (program shdr)
              unifs
              GL.drawElements mode inds GL.UnsignedInt nullPtr
              return (Right ())
          getMv cam = proc () -> do
                        unifs <- setAllUniforms shdr (withModelView object cam) -< ()
                        childs <- sceneRoot (camera =: (cam !*! rGet transform object) <+> object) -< ()
                        returnA -< doDraw . unifs >>=& childs