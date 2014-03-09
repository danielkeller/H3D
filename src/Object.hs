{-# LANGUAGE OverloadedStrings, DataKinds, TypeOperators, FlexibleContexts,
    Arrows, ScopedTypeVariables, FlexibleInstances, GADTs, OverlappingInstances #-}
module Object (
    Object(),
    objRec,
    objXfrm,
    camera,
    Uniform(..),
    freeObject,
    loadObject,
    drawObject,
) where

import Prelude hiding ((.))

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Graphics.GLUtil
import Data.Vinyl
import GHC.TypeLits (SingI)
import Graphics.VinylGL hiding (setAllUniforms)
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

type Drawable = PlainRec [FXfrm, FObject, FCamera]

type ModelView = "modelView" ::: Uniform (PlainWire (M44 GL.GLfloat))
modelView :: ModelView
modelView = Field

withModelView :: (PlainRec r <: Drawable) => PlainRec r -> PlainRec (ModelView ': r)
withModelView record = modelView =: Uniform ((!*!) <$> rGet objXfrm rec' <*> rGet camera rec')
                       <+> record
    where rec' :: Drawable
          rec' = cast record

drawObject :: (HasUniforms r, PlainRec r <: Drawable) => PlainRec r -> PlainWire ()
drawObject record = 
    mkGen_ (\unifs -> withVAO vao $ do
        GL.currentProgram $= Just (program shdr)
        unifs
        GL.polygonMode $= (GL.Line, GL.Line)
        GL.drawElements GL.Triangles inds GL.UnsignedInt nullPtr
        return (Right ()))
    <<< setAllUniforms shdr (withModelView record)
    where Object {objVAO = vao, objNumIndices = inds, objShader = shdr} = rGet objRec rec'
          rec' :: Drawable
          rec' = cast record

type PosRec = PlainRec '["position" ::: V4 GL.GLfloat]

newtype Uniform a = Uniform a deriving Show

class HasUniforms a where
    setAllUniforms :: ShaderProgram -> PlainRec a -> PlainWire (IO ())

instance HasUniforms '[] where
    setAllUniforms _ _ = pure (return ())

instance (SingI sy, HasUniforms rest, AsUniform sort)
          => HasUniforms (sy ::: Uniform (PlainWire sort) ': rest) where
    setAllUniforms shdr (Identity (Uniform thing) :& rest) =
        proc () -> do
            val <- thing -< ()
            others <- setAllUniforms shdr rest -< ()
            returnA -< others >> asUniform val (getUniform shdr (show (Field :: sy ::: ())))

instance HasUniforms rest => HasUniforms (thing ': rest) where
    setAllUniforms shdr (_ :& rest) = setAllUniforms shdr rest

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
parseVert = "v " .*> ((Field =:) <$> (V4 <$> thenFloat <*> thenFloat <*> thenFloat <*> return 1))
    where thenFloat = CFloat . double2Float <$> (double <* skipSpace)