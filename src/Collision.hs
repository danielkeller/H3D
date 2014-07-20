{-# LANGUAGE DataKinds, TypeOperators, FlexibleContexts, ConstraintKinds, Arrows #-}
module Collision (
    BVH, bvh,
    withBVH,
    drawBVHOf,
) where

import Prelude hiding (id, (.))
import Math.BVH
import qualified Data.Vector.Storable as V

import Linear
import Linear.GL
import Object
import Components
import Math.Shapes
import Shader
import Scene
import Util
import Loader

type BVH = "BVH" ::: AABB
bvh :: BVH
bvh = Field

--todo: recalculate this less
withBVH :: Component '[Obj] '[BVH]
withBVH = keep <<< attMap (buildBVH . objMesh)

-- | visualize the AABB
bvhWireframe :: AABB -> V.Vector (PlainRec '[Pos, "color" ::: Vec3])
bvhWireframe b = help b 0
    where help aabb depth = case aabb of
            Leaf box _ -> buildVector (boxWf box)
            Node box l r -> help l (depth + 1) V.++ help r (depth + 1) V.++ buildVector (boxWf box)
            where buildVector points = V.fromList $ zipWith (<+>) (map (Field =:) points)
                                                                  (repeat (Field =: rainbow depth 6))
          boxWf (Box (V3 lx ly lz) (V3 hx hy hz)) =
                 (V3  <$> [lx, hx] <*> [ly, hy] <*> [lz, hz])
              ++ (xzy <$> [lx, hx] <*> [lz, hz] <*> [ly, hy])
              ++ (yzx <$> [ly, hy] <*> [lz, hz] <*> [lx, hx])
          -- because of the way applicative works, the lines are in the axis of the last argument
          xzy x z y = V3 x y z
          yzx y z x = V3 x y z

rainbow :: CFloat -> CFloat -> Vec3
rainbow pos end = V3 (sine 0) (sine 2) (sine 4)
    where sine phase = sin (2 * pi * pos / end + phase) / 2 + 0.5

drawBVHOf :: String -> Component '[Camera] '[Transform, BVH, DrawScene] -> Component '[Camera] '[Draw]
drawBVHOf name obj = child $ bvhObject name <<<< obj

-- convert a BVH to an object to draw it

bvhObject :: String -> Component '[BVH] '[Obj]
bvhObject name = resourceN descriptor <<< shader "assets/color" <<<< id
    where descriptor = Resource {resName = name,
                                 resLoad = \deps -> makeWireframe (bvhWireframe (rGet bvh deps)) (rGet shaderRec deps),
                                 resUnload = freeObject}