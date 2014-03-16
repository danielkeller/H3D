module Linear.Applicative (
    V2(), V3(), V4(), M44, M33,
    vec2, vec3, vec4,
    mkTransformationMat,
    eye3,
    zero,
    scale, rotation, perspective,

    (!*!)
) where

import Control.Applicative
import Data.Foldable
import qualified Linear as L
import Linear (V2(..), V3(..), V4(..), M44, M33)

eye3 :: (Applicative f, Num a) => f (M33 a)
eye3 = pure L.eye3

zero :: (Applicative f, L.Additive v, Num a) => f (v a)
zero = pure L.zero

vec2 :: Applicative f => f a -> f a -> f (V2 a)
vec2 = liftA2 V2

vec3 :: Applicative f => f a -> f a -> f a -> f (V3 a)
vec3 = liftA3 V3

vec4 :: Applicative f => f a -> f a -> f a -> f a -> f (V4 a)
vec4 x y z w = V4 <$> x <*> y <*> z <*> w

scale :: (Functor f, Floating a, L.Epsilon a) => f a -> f (M44 a)
scale a = fmap (\amt -> V4 (V4 amt 0 0 0) (V4 0 amt 0 0) (V4 0 0 amt 0) (V4 0 0 0 1)) a

rotation :: (Functor f, Floating a, L.Epsilon a) => f a -> f (M44 a)
rotation a = fmap (\ang -> L.mkTransformation (L.axisAngle (V3 0 1 0) ang) L.zero) a

mkTransformationMat :: (Num a, Applicative f) => f (M33 a) -> f (V3 a) -> f (M44 a)
mkTransformationMat = liftA2 L.mkTransformationMat

perspective :: Floating a => a -> a -> a -> a -> M44 a
perspective near far fovx aspect = V4 (V4 (1/tanHalfFovx) 0 0 0)
                                      (V4 0 (aspect/tanHalfFovx) 0 0)
                                      (V4 0 0 ((far+near)*dst) (2*far*near*dst))
                                      (V4 0 0 (-1) 0)
    where tanHalfFovx = tan (fovx / 2)
          dst = 1/(near - far)

infixl 7 !*!

(!*!) :: (Applicative f, Functor m, Foldable t, L.Additive t, L.Additive n, Num a)
    => f (m (t a)) -> f (t (n a)) -> f (m (n a))
(!*!) = liftA2 (L.!*!)