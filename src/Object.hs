{-# LANGUAGE OverloadedStrings, DataKinds, TypeOperators, FlexibleContexts, Arrows #-}
module Object (
    Object(),
    objRec,
    objXfrm,
    camera,
    freeObject,
    loadObject,
    drawObject,
) where

import Prelude hiding ((.))

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Graphics.GLUtil
import Data.Vinyl
import Graphics.VinylGL
import qualified Data.Vector.Storable as V
import Foreign.Ptr(nullPtr)
import Foreign.C.Types(CFloat(..))
import Linear

import GHC.Float (double2Float)
import Data.Either (lefts, rights)
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import Control.Applicative

import Control.Wire

import Util

data Object = Object { objVAO :: GL.VertexArrayObject
                     , objNumIndices :: GL.GLint
                     , freeObject :: IO ()
                     , objShader :: ShaderProgram
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

drawObject :: (FXfrm `IElem` r, FObject `IElem` r, FCamera `IElem` r)
              => PlainRec r -> PlainWire ()
drawObject record = 
    mkGen_ (\(xfrm, cam) -> withVAO vao $ do
        GL.currentProgram $= Just (program shdr)
        setUniforms shdr (modelView =: (xfrm !*! cam))
        GL.polygonMode $= (GL.Line, GL.Line)
        GL.drawElements GL.Triangles inds GL.UnsignedInt nullPtr
        return (Right ()))
    <<<
    rGet objXfrm record &&& rGet camera record
    where Object {objVAO = vao, objNumIndices = inds, objShader = shdr} = rGet objRec record

pos :: "position" ::: V4 GL.GLfloat
pos = Field

type PosRec = PlainRec '["position" ::: V4 GL.GLfloat]

modelView :: "modelView" ::: M44 GL.GLfloat
modelView = Field

loadObject :: FilePath -> IO Object
loadObject file = do
    (verts, faces) <- unEither . parseOnly parseObj <$> B.readFile file
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
    where unEither (Left err) = error err
          unEither (Right res) = res

parseObj :: Parser (V.Vector PosRec, V.Vector Word32)
parseObj = do res <- many $ ((Left <$> parseVert) <|> (Right <$> parseFace))
              return (V.fromList (lefts res), V.concat (rights res))

parseFace :: Parser (V.Vector Word32)
parseFace = "f " .*> thenDec <*> (thenDec <*> (thenDec <*> return V.empty))
    where thenDec = (V.cons . (subtract 1) <$> decimal) <* skipSpace

parseVert :: Parser PosRec
parseVert = "v " .*> ((pos =:) <$> (V4 <$> thenFloat <*> thenFloat <*> thenFloat <*> return 1))
    where thenFloat = CFloat . double2Float <$> (double <* skipSpace)