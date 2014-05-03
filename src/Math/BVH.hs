{-# LANGUAGE DataKinds, TypeOperators, FlexibleContexts, ConstraintKinds, Arrows #-}
module Math.BVH (
    buildBVH
) where

import Data.Vinyl
import Linear.GL
import Linear
import Control.Applicative
import Control.Lens.Getter ((^.))

import Util
import Math.Shapes
import qualified Math.Mesh as M

bound :: M.Mesh -> Box
bound m = V2 (M.minimum m) (M.maximum m)

--This would probably be more efficient in a vector
data AABB = Node !Box AABB AABB
          | Leaf !Box M.Mesh

type BVH = "BVH" ::: AABB
bvh :: BVH
bvh = Field

buildBVH :: M.Mesh -> Box -> AABB
buildBVH mesh box
    --base case
    | M.length mesh <= 6 = Leaf box mesh
    --split on mean centroid of longest axis
    | otherwise = undefined