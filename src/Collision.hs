{-# LANGUAGE DataKinds, TypeOperators, FlexibleContexts, ConstraintKinds, Arrows #-}
module Collision (
    BVH, bvh,
    withBVH, drawsBVH
) where

import Data.Vinyl
import Math.BVH
import qualified Data.Vector.Storable as V
import Object

type BVH = "BVH" ::: AABB
bvh :: BVH
bvh = Field

withBVH :: (Obj `IElem` r) => PlainRec r -> PlainRec (BVH ': r)
withBVH object = bvh =: buildBVH m <+> object
    where m = objMesh (rGet objRec object)

{-
type BVHObject = "BVH Opject" ::: Object
bvhObject :: BVHObject
bvhObject = Field
-}

--TODO: how to express dependencies like this?
--Also, how to express that we either supplement or override Draw?
drawsBVH :: (Draw `IElem` r, BVH `IElem` r) => PlainRec r -> PlainRec r
drawsBVH = undefined