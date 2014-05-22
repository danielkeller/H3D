{-# LANGUAGE DataKinds, TypeOperators, FlexibleContexts, GADTs #-}
module Util (
    GameSession,
    PlainWire0,
    PlainWire,
    DrawFun,
    fromEither,
    Identity(..),
    --Entity, 
    Component,
    --augment,
    inline, polyInline,
    attMap, with
) where

import Prelude hiding (id)
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

-- entities might be useless?
type Entity inputs attributes = PlainWire inputs (PlainRec attributes)
type Component dependencies outputs = PlainWire (PlainRec dependencies) (PlainRec outputs)

-- | This function adds a component to an entity, provided its dependencies are met
augment :: (PlainRec attributes <: PlainRec dependencies) =>
              Component dependencies outputs -- ^ component
           -> Entity inputs attributes -- ^ initial entity
           -> Entity inputs (outputs ++ attributes) -- ^ augmented entity
augment component entity = entity >>> inline component

-- | Combines components into bigger ones
inline :: (PlainRec attributes <: PlainRec dependencies) =>
          Component dependencies outputs -> Component attributes (outputs ++ attributes)
inline component = (arr cast >>> component) --feed in desired inputs
                      &&& id -- pass through everything as well
                    >>> arr (uncurry (<+>))  --then recombine

-- | like 'inline', but for components that can consume anything (like setAllUniforms)
polyInline :: Component ins outs -> Component ins (outs ++ ins)
polyInline component = component &&& id >>> arr (uncurry (<+>))

-- | component that maps a function over an attribue
attMap :: (a -> a) -> Component '[n ::: a] '[n ::: a]
attMap f = arr mapit
    where mapit (Identity v :& _) = (Identity (f v) :& RNil)

with :: a -> Component '[] '[n ::: a]
with v = pure (Identity v :& RNil)