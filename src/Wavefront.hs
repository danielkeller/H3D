{-# LANGUAGE OverloadedStrings, DataKinds, TypeOperators #-}
module Wavefront (
    loadWavefront,
) where

import Graphics.GLUtil
import GHC.Float (double2Float)
import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Storable as V
import Foreign.C.Types(CFloat(..))
import Data.Vinyl
import Data.Vec.OpenGL
import Data.Vec.Applicative

import Object
import Util

-- the types of information that .obj files support
type PosRec = PlainRec '["position" ::: Vec4]
type NormRec = PlainRec '["normal" ::: Vec3]
type TexRec = PlainRec '["texCoord" ::: Vec2]

-- obj file lines
data WfLine = V PosRec | VN NormRec | VT TexRec | F (V.Vector Word32)
            -- | MtlLib String | UseMtl String
            | Junk

loadWavefront :: FilePath -> IO Object
loadWavefront file = do
    recs <- fromEither file . parseOnly parseObj <$> B.readFile file
    let vs = [r | V r <- recs]
        vns = [r | VN r <- recs] ++ repeat (Field =: 0)
        vts = [r | VT r <- recs] ++ repeat (Field =: 0)
        verts = zipWith (<+>) (zipWith (<+>) vs vns) vts
    makeObject (V.fromList verts) $ V.concat [f | F f <- recs]

parseObj :: Parser [WfLine]
parseObj =  many ((V <$> parseVert) <|> (F <$> parseFace) <|> (VN <$> parseNorm) <|> (VT <$> parseTex)
                  <|> parseMtl <|> parseUseMtl <|> parseComment)
            <* do rest <- takeByteString 
                  if B.null rest then return ()
                                 else fail (show (B.take 20 rest))

     where
        parseFace = "f " .*> thenDec <*> (thenDec <*> (thenDec <*> return V.empty))
            where thenDec = (V.cons . (subtract 1) <$> decimal) <* skipWhile (not . isSpace) <* skipSpace

        thenFloat = CFloat . double2Float <$> (double <* skipSpace)

        parseVert = (Field =:) <$> "v " .*> (thenFloat .*. thenFloat .*. thenFloat .**. return 1)
        parseNorm = (Field =:) <$> "vn " .*> (thenFloat .*. thenFloat .**. thenFloat)
        parseTex = (Field =:) <$> "vt " .*> (thenFloat .**. thenFloat)

        --parseMtl = MtlLib . B.unpack <$> (string "mtllib " *> takeTill isSpace <* skipSpace)
        --parseUseMtl = UseMtl . B.unpack <$> (string "usemtl " *> takeTill isSpace <* skipSpace)
        parseMtl = string "mtllib " *> skipWhile (notInClass "\n") *> skipSpace *> return Junk
        parseUseMtl = string "usemtl " *> skipWhile (notInClass "\n") *> skipSpace *> return Junk

        parseComment = string "#" *> skipWhile (notInClass "\n") *> skipSpace *> return Junk