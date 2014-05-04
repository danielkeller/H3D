{-# LANGUAGE DataKinds, TypeOperators, FlexibleContexts, ConstraintKinds, Arrows #-}
module Collision (
    BVH, bvh,
    withBVH, drawsBVH
) where

import Data.Vinyl
import Math.BVH
import Object

type BVH = "BVH" ::: AABB
bvh :: BVH
bvh = Field

withBVH :: (Obj `IElem` r) => PlainRec r -> PlainRec (BVH ': r)
withBVH object = bvh =: buildBVH m <+> object
    where m = objMesh (rGet objRec object)

--TODO: how to express dependencies like this?
--Also, how to express that we either supplement or override Draw?
drawsBVH :: (Draw `IElem` r, BVH `IElem` r) => PlainRec r -> PlainRec r
drawsBVH = undefined