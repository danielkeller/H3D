{-# LANGUAGE OverloadedStrings, DataKinds, TypeOperators #-}
module Wavefront (
    wavefrontObject,
) where

import GHC.Float (double2Float)
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Storable as V
import Linear.GL
import Linear.Applicative(vec3, vec2)

import qualified Math.Mesh as M (TriInd(..))
import Object
import Util hiding ((.))
import Components
import Loader
import Shader

wavefrontObject :: FilePath -> Component '[Shader] '[Obj]
wavefrontObject object = resource1 descriptor
    where descriptor = Resource {resName = object,
                                 resLoad = loadWavefront object,
                                 resUnload = freeObject}

loadWavefront :: FilePath -> ShaderProgram -> IO Object
loadWavefront file shdr = do
    recs <- fromEither file . parseOnly parseObj <$> B.readFile file
    let vs = [r | V r <- recs]
        vns = [r | VN r <- recs] ++ repeat (singleton 0)
        vts = [r | VT r <- recs] ++ repeat (singleton 0)
        verts = zipWith (<+>) (zipWith (<+>) vs vns) vts
    makeObject (V.fromList verts) (V.fromList [f | F f <- recs]) shdr

-- the types of information that .obj files support
type NormRec = PlainRec '["normal" ::: Vec3]
type TexRec = PlainRec '["texCoord" ::: Vec2]

-- obj file lines
data WfLine = V (PlainRec '[Pos]) | VN NormRec | VT TexRec | F M.TriInd
            -- | MtlLib String | UseMtl String
            | Junk

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

        parseVert = singleton <$> "v " .*> vec3 thenFloat thenFloat thenFloat
        parseNorm = singleton <$> "vn " .*> vec3 thenFloat thenFloat thenFloat
        parseTex = singleton <$> "vt " .*> vec2 thenFloat thenFloat

        --parseMtl = MtlLib . B.unpack <$> (string "mtllib " *> takeTill isSpace <* skipSpace)
        --parseUseMtl = UseMtl . B.unpack <$> (string "usemtl " *> takeTill isSpace <* skipSpace)
        parseMtl = string "mtllib " *> skipWhile (notInClass "\n") *> skipSpace *> return Junk
        parseUseMtl = string "usemtl " *> skipWhile (notInClass "\n") *> skipSpace *> return Junk

        parseComment = string "#" *> skipWhile (notInClass "\n") *> skipSpace *> return Junk