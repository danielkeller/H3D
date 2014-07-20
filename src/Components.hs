{-# LANGUAGE DataKinds, TypeOperators, FlexibleContexts #-}
module Components (
    Component,
    GameM,
    void, keep,
    (>>>>), (<<<<), (&&&&),
    inline, polyInline,
    attMap, attMapM, (=:<),
) where

import Prelude hiding (id, (.))

import Util
import Loader

type GameM = Loader

-- rename to dependencies >>> outputs ?
type Component dependencies outputs = PlainWire GameM (PlainRec dependencies) (PlainRec outputs)

--it's kind of annoying to call this all the time...

-- | Combines components into bigger ones
inline :: (PlainRec attributes <: PlainRec dependencies) =>
          Component dependencies outputs -> Component attributes (outputs ++ attributes)
inline component = (arr cast >>> component) --feed in desired inputs
                      &&&& id -- pass through everything as well

-- | like 'inline', but for components that can consume anything (like setAllUniforms)
polyInline :: Component ins outs -> Component ins (outs ++ ins)
polyInline component = component &&&& id

-- | component that maps a function over an attribue
attMap :: (a -> b) -> Component '[n ::: a] '[m ::: b]
attMap f = arr (singleton . f . unSingleton)

-- | component that maps a monadic function over an attribue
attMapM :: (a -> Loader b) -> Component '[n ::: a] '[o ::: b]
attMapM f = mkGen_ mapit
    where mapit v = do
            v' <- f (unSingleton v)
            return (Right (singleton v'))

-- | The Angry Chef operator
(=:<) :: (sy ::: t) -> t -> Component '[] '[sy ::: t]
f =:< v = pure (f =: v)

-- | help type deduction for components with no dependencies
void :: Component '[] '[]
void = id

-- | hold the first value forever
keep :: Wire s e m a a
keep = mkPureN $ \a -> (Right a, mkConst (Right a))

--define arrow-like functions

(&&&&) :: Component as bs -> Component as bs' -> Component as (bs ++ bs')
l &&&& r = l &&& r >>> arr (uncurry (<+>))

infixr 3 &&&&

(>>>>) :: (PlainRec outs <: PlainRec ins') =>
    Component ins outs -> Component ins' outs' -> Component ins (outs' ++ outs)
l >>>> r = l >>> inline r

infixr 1 >>>>

(<<<<) :: (PlainRec outs <: PlainRec ins') =>
    Component ins' outs' -> Component ins outs -> Component ins (outs' ++ outs)
(<<<<) = flip (>>>>)

infixr 1 <<<<