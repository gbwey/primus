{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Primus.Extra
Description : miscellaneous functions
Copyright   : (c) Grant Weyburne, 2022
License     : BSD-3
-}
module Primus.Extra (
  on1,
  on2,
  comparing1,
  (.@),
) where

-- | more flexible version of 'Data.Function.on' that allows differing types for the same container
on1 ::
  forall f a a' b c.
  (b -> b -> c) ->
  (forall x. f x -> b) ->
  f a ->
  f a' ->
  c
on1 f g fa fa' = f (g fa) (g fa')

-- | more flexible version of 'Data.Function.on' that allows differing types for the same container but using two parameters
on2 ::
  forall f a a' a2 a2' b c.
  (b -> b -> c) ->
  (forall x y. f x y -> b) ->
  f a a2 ->
  f a' a2' ->
  c
on2 f g fa fa' = f (g fa) (g fa')

-- | more flexible version of 'compare' that allows differing types for the same container
comparing1 ::
  forall f a a' b.
  Ord b =>
  (forall x. f x -> b) ->
  f a ->
  f a' ->
  Ordering
comparing1 g fa fa' = compare (g fa) (g fa')

-- | compose a two arg function followed by a one arg function
(.@) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.@) = (.) . (.)

infixr 8 .@
