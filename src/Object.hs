{-# LANGUAGE OverloadedStrings, DataKinds, TypeOperators, FlexibleContexts,
    Arrows, ConstraintKinds #-}
module Object (
    Object(),
    Obj(..),
    Xfrm(..),
    Camera(..),
    freeObject,
    loadObject,
    drawObject,
) where

import Prelude hiding ((.))

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Graphics.GLUtil
import qualified Data.Vector.Storable as V
import Foreign.Ptr(nullPtr)
import Foreign.C.Types(CFloat(..))
import Linear

import GHC.Float (double2Float)
import Data.Either (lefts, rights)
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import Control.Applicative

import Control.Wire hiding ((<+>))

import Data.Record
import Data.Record.Combinators
import Data.Kind
import Data.TypeFun

import Util

data Object = Object { objVAO :: GL.VertexArrayObject
                     , objNumIndices :: GL.GLint
                     , freeObject :: IO ()
                     , objShader :: ShaderProgram
                     }

data Xfrm = Xfrm deriving Show
data Obj = Obj deriving Show
data Camera = Camera deriving Show
instance Name Xfrm where name = Xfrm
instance Name Obj where name = Obj
instance Name Camera where name = Camera

type Drawable = X :& Xfrm ::: PlainWire (M44 GL.GLfloat) :& Obj ::: Object :& Camera ::: PlainWire (M44 GL.GLfloat)

drawObject :: Convertible r Drawable => r (Id KindStar) -> PlainWire ()
drawObject genRecord =
    mkGen_ (\(xfrm, cam) -> withVAO vao $ do
        GL.currentProgram $= Just (program shdr)
        asUniform (xfrm !*! cam) (getUniform shdr "modelView")
        GL.polygonMode $= (GL.Line, GL.Line)
        GL.drawElements GL.Triangles inds GL.UnsignedInt nullPtr
        return (Right ()))
    <<< (record !!! Xfrm) &&& (record !!! Camera)
    where Object {objVAO = vao, objNumIndices = inds, objShader = shdr} = record !!! Obj
          record :: Drawable (Id KindStar)
          record = convert genRecord

{-
class HasUniforms a where
    setAllUniforms :: ShaderProgram -> a -> IO ()

instance HasUniforms X where
    setAllUniforms _ _ = return ()

instance HasUniforms remain => HasUniforms (name ::: sort :& remain) where
    setAllUniforms shdr (name := thing &: rest) = 
      -}

loadObject :: FilePath -> IO Object
loadObject file = do
    (verts, faces) <- unEither . parseOnly parseObj <$> B.readFile file
    vertBuf <- fromSource GL.ArrayBuffer verts
    indBuf <- bufferIndices faces
    shdr <- simpleShaderProgram "simple.vert" "simple.frag"
    vao <- makeVAO $ do
        GL.bindBuffer GL.ArrayBuffer $= Just vertBuf
        enableAttrib shdr "position"
        setAttrib shdr "position" GL.ToFloat $ GL.VertexArrayDescriptor 4 GL.Float 0 nullPtr
        GL.bindBuffer GL.ElementArrayBuffer $= Just indBuf
    return Object { objVAO = vao
                  , objNumIndices = fromIntegral (V.length faces)
                  , objShader = shdr
                  , freeObject = do
                        GL.deleteObjectNames [vao]
                        GL.deleteObjectNames [vertBuf]
                        GL.deleteObjectNames [indBuf]
                  }
    where unEither (Left err) = error err
          unEither (Right res) = res

parseObj :: Parser (V.Vector (V4 GL.GLfloat), V.Vector Word32)
parseObj = do res <- many $ ((Left <$> parseVert) <|> (Right <$> parseFace))
              return (V.fromList (lefts res), V.concat (rights res))

parseFace :: Parser (V.Vector Word32)
parseFace = "f " .*> thenDec <*> (thenDec <*> (thenDec <*> return V.empty))
    where thenDec = (V.cons . (subtract 1) <$> decimal) <* skipSpace

parseVert :: Parser (V4 GL.GLfloat)
parseVert = "v " .*> (V4 <$> thenFloat <*> thenFloat <*> thenFloat <*> return 1)
    where thenFloat = CFloat . double2Float <$> (double <* skipSpace)