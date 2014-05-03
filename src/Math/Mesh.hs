module Math.Mesh (
    Mesh(..), TriInd(..),
    empty, null, length,
    mapV, foldr, foldl, foldlV, foldM,
    splitAt, span, filter, sortBy,
    minimum, maximum
) where

import qualified Prelude as P
import Prelude hiding (map, length, null, foldr, foldl,
                       span, takeWhile, splitAt,
                       minimum, maximum, filter)

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Algorithms.Intro as V
import Linear.GL
import Linear
import Control.Monad ((>=>))
import Data.Word(Word32)
import Foreign.Storable
import Foreign.Ptr
import Control.Applicative hiding (empty)

import Math.Shapes

data TriInd = TriInd Word32 Word32 Word32

instance Storable TriInd where
    sizeOf _ = 3 * sizeOf (0 :: Word32)
    alignment _ = alignment (0 :: Word32)
    peek p = TriInd <$> peek p' <*> peekElemOff p' 1 <*> peekElemOff p' 2
        where p' = castPtr p
    poke p (TriInd a b c) = poke p' a >> pokeElemOff p' 1 b >> pokeElemOff p' 2 c
        where p' = castPtr p

data Mesh = Mesh { meshVerts :: V.Vector Vec3
                 , meshInds :: V.Vector TriInd
                 }

triAt :: Mesh -> TriInd -> Tri
triAt (Mesh vs _) (TriInd a b c) = Tri (vs V.! fromIntegral a) (vs V.! fromIntegral b) (vs V.! fromIntegral c)

(!) :: Mesh -> Int -> Tri
(!) m@(Mesh _ is) idx = triAt m (is V.! idx)

infixl 9 !

{-# INLINE (!) #-}

empty :: Mesh
empty = Mesh V.empty V.empty

null :: Mesh -> Bool
null = V.null . meshInds

length :: Mesh -> Int
length = V.length . meshInds

mapV :: (Vec3 -> Vec3) -> Mesh -> Mesh
mapV f (Mesh vs is) = Mesh (V.map f vs) is

--TODO: double check the folds

foldr :: (Tri -> a -> a) -> a -> Mesh -> a
foldr f i m = help (length m) i
    where help 0 acc = acc
          help idx acc = help (idx - 1) (m ! idx `f` acc)

foldl :: (a -> Tri -> a) -> a -> Mesh -> a
foldl f i m = help 0 i
    where help idx acc
            | idx == length m = acc
            | otherwise = help (idx + 1) (acc `f` (m ! idx))

foldlV :: (a -> Vec3 -> a) -> a -> Mesh -> a
foldlV f i m = help 0 i
    where help idx acc
            | idx == length m = acc
            | otherwise = help (idx + 1) (acc `f` a `f` b `f` c)
            where ~(Tri a b c) = m ! idx

foldM :: Monad m => (Tri -> a -> m a) -> a -> Mesh -> m a
foldM f i m = help 0 i
    where help idx
            | idx == length m = return
            | otherwise = f (m ! idx) >=> help (idx + 1)

splitAt :: Int -> Mesh -> (Mesh, Mesh)
splitAt i (Mesh vs is) = (Mesh vs l, Mesh vs r)
    where (l, r) = V.splitAt i is

span :: (Tri -> Bool) -> Mesh -> (Mesh, Mesh)
span f m = splitAt (combine (foldM lastIdx 0 m)) m
    where lastIdx tr idx | f tr = Right (succ idx)
                         | otherwise = Left idx
          combine (Left a) = a
          combine (Right a) = a

filter ::  (Tri -> Bool) -> Mesh -> Mesh
filter f m@(Mesh vs is) = Mesh vs (V.filter help is)
    where help ind = f (triAt m ind)

--Tris are not Ord because that wouldn't make sense
sortBy :: (Tri -> Tri -> Ordering) -> Mesh -> Mesh
sortBy f m@(Mesh vs is) = Mesh vs (sortVectorBy is)
    where f' indl indr = f (m `triAt` indl) (m `triAt` indr)
          sortVectorBy = V.modify (V.sortBy f')

minimum :: Mesh -> Vec3
minimum mesh = foldlV help maxBound mesh
    where help (V3 mx my mz) (V3 cx cy cz) = V3 (min mx cx) (min my cy) (min mz cz)

maximum :: Mesh-> Vec3
maximum mesh = foldlV help minBound mesh
    where help (V3 mx my mz) (V3 cx cy cz) = V3 (max mx cx) (max my cy) (max mz cz)