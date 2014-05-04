module Math.BVH (
    AABB(..),
    buildBVH
) where

import Data.List(maximumBy)
import Linear.GL
import Linear
import Control.Lens.Getter ((^.))
import Control.Lens.Setter (set)

import Math.Shapes
import qualified Math.Mesh as M

bound :: M.Mesh -> Box
bound m = Box (M.minimum m) (M.maximum m)

--This would probably be more efficient in a vector
data AABB = Node !Box AABB AABB
          | Leaf !Box M.Mesh

meanCentroid :: M.Mesh -> Vec3
meanCentroid m = M.foldl addcentr 0 m ^/ fromIntegral (M.length m)
    where addcentr c tr = c ^+^ centroid tr

longestAxis :: Box -> E V3
longestAxis (Box a b) = maximumBy longer [ex, ey, ez]
    where longer (E ax) (E ax') = compare (b^.ax - a^.ax) (b^.ax' - a^.ax')

buildBVH :: M.Mesh -> AABB
buildBVH m = buildBVH' m (bound m)

buildBVH' :: M.Mesh -> Box -> AABB
buildBVH' mesh box@(Box a b)
    --base case
    | M.length mesh <= 6 = Leaf box mesh
    | otherwise = Node box (buildBVH' leftMesh left') (buildBVH' rightMesh right')
        where --split on mean centroid of longest axis
              E splitAx = longestAxis box
              splitter = set splitAx (meanCentroid mesh ^. splitAx)
              --replace the component with that of the split point
              left = Box a (splitter b)
              right = Box (splitter a) b
              --get the relevant meshes (warning, might loop on degernate meshes)
              leftMesh = M.filter (intersects left) mesh
              rightMesh = M.filter (intersects right) mesh
              --chop out uselss space
              left' = boxIntersect left (bound leftMesh)
              right' = boxIntersect right (bound rightMesh)