{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : Primus.AsMaybe
Description : methods with termination
Copyright   : (c) Grant Weyburne, 2022
License     : BSD-3
-}
module Primus.AsMaybe (
  -- * AsMaybe
  AsMaybe (..),
  iterateT1,
  unfoldrT,
  pairsT,

  -- * ApThese
  ApThese (..),
  toTheseT,
  toTheseTS,
  partitionEithersT,
  partitionTheseT,
  filterT,
  spanT,
  spanTAlt,
  spanTS,
  takeWhileT,
  takeWhileTS,

  -- * ApTheseF for use with 'Primus.LRHist.LRHist'
  ApTheseF (..),
) where

import Control.Arrow
import Data.Bool
import Data.Functor.Identity
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Semigroup as SG
import Data.These
import Data.These.Combinators
import Primus.Error ((.@))

-- | converts to a 'Maybe' for failure types
class AsMaybe x b | x -> b where
  toMaybe :: x -> Maybe b

instance (b ~ b1) => AsMaybe (These e b) b1 where
  toMaybe = these (const Nothing) Just (const Just)
instance (b ~ b1) => AsMaybe (Either e b) b1 where
  toMaybe = either (const Nothing) Just
instance (b ~ b1) => AsMaybe (Maybe b) b1 where
  toMaybe = id
instance (b1 ~ [b]) => AsMaybe [b] b1 where
  toMaybe = \case
    [] -> Nothing
    as@(_ : _) -> Just as
instance (z ~ SG.Arg b1 y, AsMaybe x b1) => AsMaybe (SG.Arg x y) z where
  toMaybe (SG.Arg x y) = (`SG.Arg` y) <$> toMaybe x
instance (b ~ (b1, b2), AsMaybe x b1, AsMaybe y b2) => AsMaybe (x, y) b where
  toMaybe (x, y) = (,) <$> toMaybe x <*> toMaybe y
instance (b ~ (b1, b2, b3), AsMaybe x b1, AsMaybe y b2, AsMaybe z b3) => AsMaybe (x, y, z) b where
  toMaybe (x, y, z) = (,,) <$> toMaybe x <*> toMaybe y <*> toMaybe z

instance AsMaybe x z => AsMaybe (Identity x) z where
  toMaybe (Identity x) = toMaybe x

-- supports Bool instance so partition can work the same as base [not a requirement but..]

-- | flexible "e" to use with eg 'partitionTheseT': Bool is also valid
class ApThese e a x b | x e a -> b where
  apThese :: a -> x -> These e b

instance (e ~ e1, b ~ b1) => ApThese e1 a (These e b) b1 where
  apThese _ = id
instance (e ~ e1, b ~ b1) => ApThese e1 a (Either e b) b1 where
  apThese _ = either This That
instance (e ~ a, b ~ b1) => ApThese e a (Maybe b) b1 where
  apThese a = maybe (This a) That
instance (e ~ a, b ~ a) => ApThese e a Bool b where
  apThese a = bool (This a) (That a)
instance (e ~ a, b1 ~ [b]) => ApThese e a [b] b1 where
  apThese a = \case
    [] -> This a
    as@(_ : _) -> That as

instance (z ~ SG.Arg b1 y, ApThese e a x b1) => ApThese e a (SG.Arg x y) z where
  apThese a (SG.Arg x y) = (`SG.Arg` y) <$> apThese a x
instance (Semigroup e, b ~ (b1, b2), ApThese e a x b1, ApThese e a y b2) => ApThese e a (x, y) b where
  apThese a (x, y) = (,) <$> apThese a x <*> apThese a y
instance (Semigroup e, b ~ (b1, b2, b3), ApThese e a x b1, ApThese e a y b2, ApThese e a z b3) => ApThese e a (x, y, z) b where
  apThese a (x, y, z) = (,,) <$> apThese a x <*> apThese a y <*> apThese a z

instance ApThese e a x z => ApThese e a (Identity x) z where
  apThese a (Identity x) = apThese a x

-- for LRHist "e" is fixed
-- supports Bool instance for use with LRHist [this is a requirement]

-- | for use with 'Primus.LRHist.LRHist' using a fixed "e"
class ApTheseF e a x b | x e a -> b where
  apTheseF :: a -> x -> These e b

instance (e ~ e1, b ~ b1) => ApTheseF e1 a (These e b) b1 where
  apTheseF _ = id
instance (e ~ e1, b ~ b1) => ApTheseF e1 a (Either e b) b1 where
  apTheseF _ = either This That
instance (Monoid e, b ~ b1) => ApTheseF e a (Maybe b) b1 where
  apTheseF _ = maybe (This mempty) That
instance (Monoid e, b ~ a) => ApTheseF e a Bool b where
  apTheseF a = bool (This mempty) (That a)
instance (Monoid e, b1 ~ [b]) => ApTheseF e a [b] b1 where
  apTheseF _ = \case
    [] -> This mempty
    as@(_ : _) -> That as

instance (z ~ SG.Arg b1 y, ApTheseF e a x b1) => ApTheseF e a (SG.Arg x y) z where
  apTheseF a (SG.Arg x y) = (`SG.Arg` y) <$> apTheseF a x
instance (Semigroup e, b ~ (b1, b2), ApTheseF e a x b1, ApTheseF e a y b2) => ApTheseF e a (x, y) b where
  apTheseF a (x, y) = (,) <$> apTheseF a x <*> apTheseF a y
instance (Semigroup e, b ~ (b1, b2, b3), ApTheseF e a x b1, ApTheseF e a y b2, ApTheseF e a z b3) => ApTheseF e a (x, y, z) b where
  apTheseF a (x, y, z) = (,,) <$> apTheseF a x <*> apTheseF a y <*> apTheseF a z

instance ApTheseF e a x z => ApTheseF e a (Identity x) z where
  apTheseF a (Identity x) = apTheseF a x

-- | similar to 'Data.List.NonEmpty.iterate' but terminate using 'AsMaybe'
iterateT1 ::
  AsMaybe x a =>
  (a -> x) ->
  a ->
  NonEmpty a
iterateT1 f a0 = a0 :| go a0
 where
  go a = case toMaybe (f a) of
    Nothing -> []
    Just x -> x : go x

{- | like 'Data.List.unfoldr' but terminate using 'AsMaybe'

@
>>> unfoldrT (splitAt 2) [1..8]
[[1,2],[3,4],[5,6],[7,8]]

vs

>>> unfoldr (\s -> if null s then Nothing else Just (splitAt 2 s)) [1..8]
[[1,2],[3,4],[5,6],[7,8]]
@
-}
unfoldrT ::
  AsMaybe t t =>
  (t -> (a, t)) ->
  t ->
  [a]
unfoldrT f s0 =
  case toMaybe s0 of
    Nothing -> []
    Just s1 ->
      let (a, s2) = f s1
       in a : unfoldrT f s2

-- | run a functions against each side of a tuple and stitch them together for use with 'unfoldrT' where "s" is a tuple and you want to stop as soon as the either terminates
pairsT :: (x -> (a, x)) -> (y -> (b, y)) -> (x, y) -> ((a, b), (x, y))
pairsT f g (x0, y0) =
  let (a, x) = f x0
      (b, y) = g y0
   in ((a, b), (x, y))

-- | apply a function to a list and convert to a list of 'These'
toTheseT ::
  forall e a x b.
  (ApThese e a x b) =>
  (a -> x) ->
  [a] ->
  [These e b]
toTheseT f = map (\a -> apThese a (f a))

-- | like 'toTheseT' with state
toTheseTS ::
  forall e a x b z.
  (ApThese e a x b) =>
  (z -> a -> (z, x)) ->
  z ->
  [a] ->
  (z, [These e b])
toTheseTS f = L.mapAccumL (\z a -> second (apThese a) (f z a))

-- | like 'partitionEithersT' ignoring the second element of the result
filterT ::
  forall e a b x.
  ApThese e a x b =>
  (a -> x) ->
  [a] ->
  [b]
filterT = catThat .@ toTheseT @e -- minimal type applications required as "e" isnt used here

-- | like 'toTheseT' but use 'partitionHereThere' on the results (swapped version of 'Data.List.partition')
partitionEithersT ::
  forall e a b x.
  ApThese e a x b =>
  (a -> x) ->
  [a] ->
  ([e], [b])
partitionEithersT = partitionHereThere .@ toTheseT

-- | like 'toTheseT' but use 'partitionThese' on the results
partitionTheseT ::
  forall e a b x.
  ApThese e a x b =>
  (a -> x) ->
  [a] ->
  ([e], [b], [(e, b)])
partitionTheseT = partitionThese .@ toTheseT

-- | similar to 'Data.List.span' using 'ApThese' for failure (support Bool and These)
spanT ::
  forall e a x b.
  ApThese e a x b =>
  (a -> x) ->
  [a] ->
  ([b], [a])
spanT f = \case
  [] -> ([], [])
  a : as -> case apThese @e a (f a) of
    This _ -> ([], a : as)
    That b -> first (b :) (spanT @e f as)
    These _ b -> ((b :) *** (a :)) (spanT @e f as) -- put in both buckets and keep going

-- | like 'spanT' but doesn't continue in the 'These' case
spanTAlt ::
  forall e a x b.
  ApThese e a x b =>
  (a -> x) ->
  [a] ->
  ([b], [a])
spanTAlt f = \case
  [] -> ([], [])
  a : as -> case apThese @e a (f a) of
    This _ -> ([], a : as)
    That b -> first (b :) (spanT @e f as)
    These _ b -> ([b], a : as) -- put in both buckets and stop

-- | like 'spanT' with state
spanTS ::
  forall e a x b z.
  ApThese e a x b =>
  (z -> a -> (z, x)) ->
  z ->
  [a] ->
  (z, ([b], [a]))
spanTS f z0 = \case
  [] -> (z0, ([], []))
  a : as ->
    let (z, x) = f z0 a
     in case apThese @e a x of
          This _ -> (z, ([], a : as))
          That b -> second (first (b :)) (spanTS @e f z as)
          These _ b -> second ((b :) *** (a :)) (spanTS @e f z as) -- if these then put in both buckets

-- | like 'takeWhileT' with state
takeWhileTS ::
  forall e a x b z.
  ApThese e a x b =>
  (z -> a -> (z, x)) ->
  z ->
  [a] ->
  (z, [b])
takeWhileTS f = second fst .@ spanTS @e f

-- | like 'spanT' but ignore the second element of the result
takeWhileT ::
  forall e a x b.
  ApThese e a x b =>
  (a -> x) ->
  [a] ->
  [b]
takeWhileT = fst .@ spanT @e
