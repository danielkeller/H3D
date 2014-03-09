{-# LANGUAGE OverloadedStrings, DataKinds, TypeOperators #-}
module Wavefront (
    loadWavefront,
) where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.GLUtil
import GHC.Float (double2Float)
import Data.Either (lefts, rights)
import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Storable as V
import Foreign.C.Types(CFloat(..))
import Data.Vinyl
import Linear

import Object

type PosRec = PlainRec '["position" ::: V4 GL.GLfloat]

loadWavefront :: FilePath -> IO Object
loadWavefront file = do
    (verts, faces) <- unEither . parseOnly parseObj <$> B.readFile file
    makeObject verts faces
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