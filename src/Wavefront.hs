{-# LANGUAGE OverloadedStrings, DataKinds, TypeOperators #-}
module Wavefront (
    loadWavefront,
) where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.GLUtil
import GHC.Float (double2Float)
import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Storable as V
import Foreign.C.Types(CFloat(..))
import Data.Vinyl
import Linear

import Object

-- the types of information that .obj files support
type PosRec = PlainRec '["position" ::: V4 GL.GLfloat]
type NormRec = PlainRec '["normal" ::: V3 GL.GLfloat]
type TexRec = PlainRec '["texCoord" ::: V2 GL.GLfloat]

-- obj file lines
data WfLine = V PosRec | VN NormRec | VT TexRec | F (V.Vector Word32) | Junk

loadWavefront :: FilePath -> IO Object
loadWavefront file = do
    recs <- unEither . parseOnly parseObj <$> B.readFile file
    let vs = [r | V r <- recs]
        vns = [r | VN r <- recs] ++ repeat (Field =: zero)
        vts = [r | VT r <- recs] ++ repeat (Field =: zero)
        verts = zipWith (<+>) (zipWith (<+>) vs vns) vts
    makeObject (V.fromList verts) $ V.concat [f | F f <- recs]
    where unEither (Left err) = error $ file ++ ": " ++ err
          unEither (Right res) = res

parseObj :: Parser [WfLine]
parseObj =  many ((V <$> parseVert) <|> (F <$> parseFace) <|> (VN <$> parseNorm) <|> (VT <$> parseTex)
                  <|> parseMtl <|> parseUseMtl)
            <* do rest <- takeByteString 
                  if B.null rest then return ()
                                 else fail (show (B.take 20 rest))

     where
        parseFace = "f " .*> thenDec <*> (thenDec <*> (thenDec <*> return V.empty))
            where thenDec = (V.cons . (subtract 1) <$> decimal) <* skipWhile (not . isSpace) <* skipSpace

        thenFloat = CFloat . double2Float <$> (double <* skipSpace)

        parseVert = (Field =:) <$> "v " .*> (V4 <$> thenFloat <*> thenFloat <*> thenFloat <*> return 1)

        parseNorm = (Field =:) <$> "vn " .*> (V3 <$> thenFloat <*> thenFloat <*> thenFloat)

        parseTex = (Field =:) <$> "vt " .*> (V2 <$> thenFloat <*> thenFloat)

        parseMtl = string "mtllib" *> skipWhile (notInClass "\n") *> skipSpace *> return Junk

        parseUseMtl = string "usemtl" *> skipWhile (notInClass "\n") *> skipSpace *> return Junk