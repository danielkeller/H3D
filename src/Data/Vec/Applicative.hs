{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Vec.Applicative (
    (.**.), (.*.),

    identity,

    scaling, rotationY,

    multmm
) where

import Control.Applicative
import qualified Data.Vec as V

(.**.) :: Applicative f => f a -> f b -> f (a V.:. (b V.:. ()))
l .**. r = l .*. r .*. (pure ())

(.*.) :: Applicative f => f a -> f b -> f (a V.:. b)
(.*.) = liftA2 (V.:.)

infixr 4 .*.

identity :: (Applicative f, V.Vec n a v, V.Vec n v m, Num v, Num m, V.SetDiagonal v m) => f m
identity = pure V.identity

multmm :: (Applicative f, V.Map v v' m1 m3, V.Map v a b v', V.Transpose m2 b, V.Fold v a, Num v, Num a)
        => f m1 -> f m2 -> f m3
multmm = liftA2 V.multmm

infixl 7 `multmm`

scaling :: (Functor f, Num a) => f (V.Vec3 a) -> f (V.Mat44 a)
scaling = fmap V.scaling

rotationY :: (Functor f, Floating a) => f a -> f (V.Mat44 a)
rotationY = fmap V.rotationY