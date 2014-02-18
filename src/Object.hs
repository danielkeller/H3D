{-# LANGUAGE OverloadedStrings, DataKinds, TypeOperators #-}
module Object (
    Object(),
    freeObject,
    loadObject,
    drawObject,
) where

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

data Object = Object { objVAO :: GL.VertexArrayObject
                     , objNumIndices :: GL.GLint
                     , freeObject :: IO ()
                     , objShader :: ShaderProgram
                     }

drawObject :: Object -> IO ()
drawObject Object {objVAO = vao, objNumIndices = inds, objShader = shdr} =
    withVAO vao $ do
        GL.currentProgram $= Just (program shdr)
        setUniforms shdr (modelView =: mkTransformationMat (eye3 !!* 0.2) (V3 0 0 0))
        GL.polygonMode $= (GL.Line, GL.Line)
        GL.drawElements GL.Triangles inds GL.UnsignedInt nullPtr

pos :: "position" ::: V4 GL.GLfloat
pos = Field

tex :: "tex" ::: GL.GLint
tex = Field

modelView :: "modelView" ::: M44 GL.GLfloat
modelView = Field

loadObject :: FilePath -> IO Object
loadObject file = do
    (verts, faces) <- unEither . parseOnly parseObj <$> B.readFile file
    --let (verts, faces) = (map (pos =:) [V4 1 0 0 1, V4 1 1 0 1, V4 0 1 0 1], V.fromList [0,1,2])
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

--parseObj :: Parser (V.Vector (V4 Float), V.Vector GL.GLint)
parseObj = do res <- many $ ((Left <$> parseVert) <|> (Right <$> parseFace))
              return (V.fromList (lefts res), V.concat (rights res))

--parseFace :: Parser (V.Vector GL.GLint)
parseFace = "f " .*> thenDec <*> (thenDec <*> (thenDec <*> return V.empty))
    where thenDec = (V.cons . (subtract 1) <$> decimal) <* skipSpace

--parseVert :: Parser (PlainRec (V4 Float))
parseVert = "v " .*> ((pos =:) <$> (V4 <$> thenFloat <*> thenFloat <*> thenFloat <*> return 1))
    where thenFloat = CFloat . double2Float <$> (double <* skipSpace)

vertPosnAttrib :: GL.AttribLocation
vertPosnAttrib = GL.AttribLocation 0