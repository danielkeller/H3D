{-# LANGUAGE DataKinds, TypeOperators, FlexibleContexts, GADTs #-}
module Util (
    GameSession,
    PlainWire0,
    PlainWire,
    DrawFun,
    fromEither,
    Identity(..),
    Component,
    void,
    (>>>>), (<<<<), (&&&&),
    inline, polyInline,
    attMap, (=:<)
) where

import Prelude hiding (id, (.))
import Control.Wire hiding ((<+>), Identity)
import Data.Vinyl
import Data.Vinyl.Idiom.Identity(Identity(..))

type GameSession = Timed Float ()
type PlainWire0 b = Wire GameSession () IO () b
type PlainWire a b = Wire GameSession () IO a b

type DrawFun = Float -> IO ()

fromEither :: String -> Either String b -> b
fromEither message (Left err) = error $ message ++ ": " ++ err
fromEither _ (Right res) = res

type Component dependencies outputs = PlainWire (PlainRec dependencies) (PlainRec outputs)

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
attMap :: (a -> a) -> Component '[n ::: a] '[n ::: a]
attMap f = arr mapit
    where mapit (Identity v :& _) = (Identity (f v) :& RNil)

-- | The Angry Chef operator
(=:<) :: (sy ::: t) -> t -> Component '[] '[sy ::: t]
f =:< v = pure (f =: v)

-- | help type deduction for components with no dependencies
void :: Component '[] '[]
void = id

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