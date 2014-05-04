{-# LANGUAGE OverloadedStrings, DataKinds, TypeOperators #-}
module Wavefront (
    loadWavefront,
) where

import GHC.Float (double2Float)
import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Storable as V
import Data.Vinyl
import Linear.Applicative hiding (zero)
import Linear.GL
import Linear (zero)

import qualified Math.Mesh as M
import Object
import Util

-- the types of information that .obj files support
type NormRec = PlainRec '["normal" ::: Vec3]
type TexRec = PlainRec '["texCoord" ::: Vec2]

-- obj file lines
data WfLine = V (PlainRec '[Pos]) | VN NormRec | VT TexRec | F M.TriInd
            -- | MtlLib String | UseMtl String
            | Junk

loadWavefront :: FilePath -> IO Object
loadWavefront file = do
    recs <- fromEither file . parseOnly parseObj <$> B.readFile file
    let vs = [r | V r <- recs]
        vns = [r | VN r <- recs] ++ repeat (Field =: zero)
        vts = [r | VT r <- recs] ++ repeat (Field =: zero)
        verts = zipWith (<+>) (zipWith (<+>) vs vns) vts
    makeObject (V.fromList verts) (V.fromList [f | F f <- recs]) Triangles

parseObj :: Parser [WfLine]
parseObj =  many ((V <$> parseVert) <|> (F <$> parseFace) <|> (VN <$> parseNorm) <|> (VT <$> parseTex)
                  <|> parseMtl <|> parseUseMtl <|> parseComment)
            <* do rest <- takeByteString 
                  if B.null rest then return ()
                                 else fail (show (B.take 20 rest))

     where
        parseFace = "f " .*> (M.TriInd <$> thenDec <*> thenDec <*> thenDec)
            where thenDec = (subtract 1 <$> decimal) <* skipWhile (not . isSpace) <* skipSpace

        thenFloat = CFloat . double2Float <$> (double <* skipSpace)

        parseVert = (Field =:) <$> "v " .*> (vec3 thenFloat thenFloat thenFloat)
        parseNorm = (Field =:) <$> "vn " .*> (vec3 thenFloat thenFloat thenFloat)
        parseTex = (Field =:) <$> "vt " .*> (vec2 thenFloat thenFloat)

        --parseMtl = MtlLib . B.unpack <$> (string "mtllib " *> takeTill isSpace <* skipSpace)
        --parseUseMtl = UseMtl . B.unpack <$> (string "usemtl " *> takeTill isSpace <* skipSpace)
        parseMtl = string "mtllib " *> skipWhile (notInClass "\n") *> skipSpace *> return Junk
        parseUseMtl = string "usemtl " *> skipWhile (notInClass "\n") *> skipSpace *> return Junk

        parseComment = string "#" *> skipWhile (notInClass "\n") *> skipSpace *> return Junk