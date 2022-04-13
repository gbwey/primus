{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Primus.Rep
Description : representable methods for use with fixed containers
Copyright   : (c) Grant Weyburne, 2022
License     : BSD-3
-}
module Primus.Rep (
  buildRepL,
  buildRepR,
  fillRep,
  toEnumRep,
  izipWithR,
  izipWithRF,
  ipostscanr,
  ipostscanl,
  unfoldlRep,
  unfoldrRep,
) where

import Data.Bool
import Data.Distributive
import Data.Foldable
import Data.Functor.Rep
import qualified Data.List as L
import qualified Data.List.NonEmpty as N
import Primus.Enum
import Primus.Error
import Primus.Fold

-- | builds a representable from the left using past and future inputs
buildRepL ::
  forall f a b.
  (Traversable f, Representable f) =>
  ([Rep f] -> [Rep f] -> b -> Rep f -> (b, a)) ->
  b ->
  (b, f a)
buildRepL f b0 = histMapL f b0 (tabulate id)

-- | same as 'buildRepL' but associates to the right
buildRepR ::
  forall f a b.
  (Traversable f, Representable f) =>
  ([Rep f] -> [Rep f] -> b -> Rep f -> (b, a)) ->
  b ->
  (b, f a)
buildRepR f b0 = histMapR f b0 (tabulate id)

-- | fill a representable container with a foldable
fillRep ::
  forall f a.
  (Representable f, Traversable f) =>
  [a] ->
  Either String ([a], f a)
fillRep = fillTraversable (tabulate id)

-- | load a fixed container with "a"s using the relative position "i"
toEnumRep ::
  forall f a.
  (Traversable f, Representable f, Enum a, Bounded a) =>
  Integer ->
  Either String (f a)
toEnumRep = toEnumTraversable (tabulate id)

-- | 'Data.List.zipWith' with rep index
izipWithR ::
  Representable f =>
  (Rep f -> a -> b -> c) ->
  f a ->
  f b ->
  f c
izipWithR f as bs = tabulate $ \k -> f k (index as k) (index bs k)

-- | 'Control.Monad.zipWithM' with rep index
izipWithRF ::
  (Representable f, Distributive g) =>
  (Rep f -> a -> b -> g c) ->
  f a ->
  f b ->
  g (f c)
izipWithRF f = collect id .@ izipWithR f

{- | like 'Data.List.scanr'
 passes in the 'Rep' index and removes the first element
-}
ipostscanr :: (Representable f, Traversable f) => (Rep f -> a -> b -> b) -> b -> f a -> f b
ipostscanr f c ta =
  frp $ fillTraversableExact ta $ N.init $ N.scanr (uncurry f) c xs
 where
  xs = toList $ imapRep (,) ta

{- | like 'Data.List.scanl'
 passes in the 'Rep' index and removes the last element
-}
ipostscanl :: (Representable f, Traversable f) => (Rep f -> b -> a -> b) -> b -> f a -> f b
ipostscanl f c ta =
  frp $ fillTraversableExact ta $ N.tail $ N.scanl g c xs
 where
  xs = imapRep (,) ta
  g b (i, a) = f i b a

-- | left/right unfold from the right into a Representable
unfoldlRep
  , unfoldrRep ::
    (Representable f, Traversable f) =>
    (Rep f -> s -> (s, a)) ->
    s ->
    (s, f a)
unfoldlRep = unfoldRepImpl False
unfoldrRep = unfoldRepImpl True

unfoldRepImpl :: (Representable f, Traversable f) => Bool -> (Rep f -> s -> (s, a)) -> s -> (s, f a)
unfoldRepImpl isright f s = bool L.mapAccumL L.mapAccumR isright (flip f) s (tabulate id)
