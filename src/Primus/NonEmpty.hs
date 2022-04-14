{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Primus.NonEmpty
Description : utilities for nonempty lists
Copyright   : (c) Grant Weyburne, 2022
License     : BSD-3
-}
module Primus.NonEmpty (
  MLR (..),

  -- * zip
  zipWithExtras1,
  zipWithExtras,
  mlrOrdering,
  fromList1LR,

  -- * chunking
  chunksOf1,
  chunksRange1,
  chunkNLen,
  chunkNLen1,

  -- * split
  Split1 (..),
  split1Ordering,
  splitAt1,
  splitAt1',
  splitAt1GE,
  splitAts1,
  splits1,
  splits3,

  -- * partition
  partition1,
  --  toThese1,

  -- * span
  spanAdjacent1,
  breakAdjacent1,
  span1,
  break1,

  -- * ascending order methods
  Seq1 (..),
  isSequence1,
  isEnumAscending,
  seq1Ordering,

  -- * isomorphisms
  uncons1,
  unsnoc1,
  consNonEmpty,
  snocNonEmpty,

  -- * positive specific functions
  sumP,
  lengthP,

  -- * fold unfold
  foldMapM1,
  unfoldr1NE,
  unfoldrM1,

  -- * iterators
  iterateMaybe1,
  iterateMaybe1',
  iterateN1,
  replicateP,

  -- * miscellaneous
  appendL1,
  appendR1,
  snoc1,
  updateAt1,
  at1,
  setAt1,
  units1,
  unitsF,
  lengthExact1,
  take1,
  sum1,
  groupByAdjacent1,
  findDupsBy,
  replicate1,
  replicate1M,

  nonemptySnoc,
  nonempty,
  nonempty',
) where

import Control.Arrow
import Control.Monad
import Data.Either
import Data.Foldable
import Data.Function
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as N
import Data.Pos
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.These
import Data.Tuple
import qualified GHC.Exts as GE (IsList (..))
import Primus.Bool
import Primus.Error
import Primus.Fold
import Primus.Lens

-- | zips two nonempty lists together and puts any leftovers into 'MLR'
zipWithExtras1 :: (a -> b -> c) -> NonEmpty a -> NonEmpty b -> (NonEmpty c, MLR a b)
zipWithExtras1 f (a :| as) (b :| bs) = first (f a b :|) $ zipWithExtras f as bs

-- | represents an optional 'Either' ie Maybe (Either (NonEmpty a) (NonEmpty b))
data MLR a b
  = -- | extra values on the left hand side
    MLRLeft !(NonEmpty a)
  | -- | both values have the same length
    MLREqual
  | -- | extra values on the right hand side
    MLRRight !(NonEmpty b)
  deriving stock (Show, Eq, Ord)

-- | 'MLRLeft' predicate
mlrOrdering :: MLR a b -> Ordering
mlrOrdering = \case
  MLRLeft{} -> LT
  MLREqual -> EQ
  MLRRight{} -> GT

-- | zips two lists together and puts any leftovers into 'MLR'
zipWithExtras :: forall a b c. (a -> b -> c) -> [a] -> [b] -> ([c], MLR a b)
zipWithExtras f = go
 where
  go [] [] = ([], MLREqual)
  go (a : as) (b : bs) = let (x, y) = go as bs in (f a b : x, y)
  go (a : as) [] = ([], MLRLeft (a :| as))
  go [] (b : bs) = ([], MLRRight (b :| bs))

-- | conversion from list to a nonempty list
fromList1LR :: [a] -> Either String (NonEmpty a)
fromList1LR =
  \case
    [] -> Left "fromList1LR: empty list"
    n : ns -> Right $ n :| ns

-- | split a nonempty list into a nonempty list of nonempty chunks
chunksOf1 :: Pos -> NonEmpty a -> NonEmpty (NonEmpty a)
chunksOf1 = join chunksRange1

{- | split a nonempty list into a nonempty list of nonempty chunks given a chunk size and how many to skip each iteration
 can decide the size of the chunks and how many elements to skip
-}
chunksRange1 :: Pos -> Pos -> NonEmpty a -> NonEmpty (NonEmpty a)
chunksRange1 n (Pos skip) = unfoldr1NE (take1 n &&& N.drop skip)

{- | creates a nonempty container of length "sz" with chunks of a given size: see 'chunkNLen'
 must fill the container exactly
-}
chunkNLen1 ::
  forall a u.
  Foldable u =>
  Pos ->
  Pos ->
  u a ->
  Either String (NonEmpty (NonEmpty a))
chunkNLen1 sz = chunkNLen (units1 sz)

{- | fills a container "tz" with chunks of size "len"
 must fill the container exactly
-}
chunkNLen ::
  forall t a u z.
  (Traversable t, Foldable u) =>
  t z ->
  Pos ->
  u a ->
  Either String (t (NonEmpty a))
chunkNLen tz len ua = do
  chunkN' f tz (toList ua)
 where
  f :: [a] -> Either String ([a], NonEmpty a)
  f = \case
    [] -> Left "chunkNLen: not enough data"
    x : xs -> swap <$> splitAt1GE len (x :| xs)

{- | unfoldr for a nonempty list

will not terminate if the user keeps returning a larger [s] than received
-}
unfoldr1NE ::
  forall s a.
  (NonEmpty s -> (a, [s])) ->
  NonEmpty s ->
  NonEmpty a
unfoldr1NE f = go
 where
  go :: NonEmpty s -> NonEmpty a
  go ns =
    let (a, ys) = f ns
     in (a :|) $ case ys of
          [] -> []
          x : xs -> N.toList (go (x :| xs))

-- | 'replicate' for a nonempty list
replicate1 :: Pos -> a -> NonEmpty a
replicate1 (Pos n) a = a :| replicate (n - 1) a

-- | 'replicateM' for a nonempty list
replicate1M :: Applicative m => Pos -> m a -> m (NonEmpty a)
replicate1M (Pos n) ma = (:|) <$> ma <*> replicateM (n - 1) ma

-- | 'partitionThese' for a nonempty list
partition1 ::
  Foldable1 t =>
  (a -> Bool) ->
  t a ->
  These (NonEmpty a) (NonEmpty a)
partition1 p =
  sconcat
    . N.map (boolM p (This . pure) (That . pure))
    . toNonEmpty

-- | internal function used by 'span1'
toThese1 ::
  These (NonEmpty a) (NonEmpty b) ->
  ([a], [b]) ->
  These (NonEmpty a) (NonEmpty b)
toThese1 th ns =
  th & case ns of
    ([], []) -> id
    (a : as, []) -> (<> This (a :| as))
    ([], b : bs) -> (<> That (b :| bs))
    (a : as, b : bs) -> (<> These (a :| as) (b :| bs))

-- | 'span' for a nonempty list
span1 ::
  Foldable1 t =>
  (a -> Bool) ->
  t a ->
  These (NonEmpty a) (NonEmpty a)
span1 p (toNonEmpty -> (a :| as)) =
  toThese1
    ( boolM
        p
        (That . pure)
        (This . pure)
        a
    )
    (L.span p as)

-- | 'break' for a nonempty list
break1 ::
  Foldable1 t =>
  (a -> Bool) ->
  t a ->
  These (NonEmpty a) (NonEmpty a)
break1 p = span1 (not . p)

-- | 'sum' for a nonempty list
sum1 :: (Foldable1 t, Num a) => t a -> a
sum1 = L.foldl' (+) 0

-- | predicate for an ascending nonempty list
isSequence1 :: (Foldable1 t, Eq a, Enum a) => t a -> Bool
isSequence1 = (EQ ==) . seq1Ordering . isEnumAscending

-- | possible results for determining if a nonempty list is in ascending order
data Seq1 a
  = -- | generated enumerable sequence is shorter than the original list
    S1Short !(NonEmpty a)
  | -- | first mismatch
    S1Fail !(a, a)
  | -- | both sequences match
    S1Ok
  deriving stock (Show, Eq, Ord, Functor)

-- | predicate for 'S1Short'
seq1Ordering :: Seq1 a -> Ordering
seq1Ordering = \case
  S1Short{} -> LT
  S1Ok{} -> EQ
  S1Fail{} -> GT

-- | shows the first failure or if the length of the enum is too short
isEnumAscending :: forall t a. (Foldable1 t, Eq a, Enum a) => t a -> Seq1 a
isEnumAscending (toNonEmpty -> as@(a :| _)) =
  let (cs, me) = zipWithExtras1 f (a :| drop 1 [a ..]) as
   in case me of
        MLRLeft _ -> either S1Fail (const S1Ok) $ sequenceA cs
        MLREqual -> either S1Fail (const S1Ok) $ sequenceA cs
        MLRRight zs -> S1Short zs
 where
  f :: a -> a -> Either (a, a) ()
  f x y = if x == y then Right () else Left (x, y)

-- | snoc for a nonempty list
snoc1 :: Foldable t => t a -> a -> NonEmpty a
snoc1 as a = foldr (N.<|) (pure a) as

-- | unsnoc for a nonempty list
unsnoc1 :: forall a. NonEmpty a -> ([a], a)
unsnoc1 = uncurry go . uncons1
 where
  go :: a -> [a] -> ([a], a)
  go n [] = ([], n)
  go n (x : xs) = first (n :) (go x xs)

-- | break up a nonempty into cases using cons
nonempty :: (a -> [a] -> b) -> NonEmpty a -> b
nonempty f (a:|as) = f a as

-- | break up a nonempty into cases using cons (argument order is flipped from 'nonempty')
nonempty' :: NonEmpty a -> (a -> [a] -> b) -> b
nonempty' = flip nonempty

-- | break up a nonempty into cases using snoc
nonemptySnoc :: ([a] -> a -> b) -> NonEmpty a -> b
nonemptySnoc f = uncurry f . unsnoc1

-- | uncons for a nonempty list
uncons1 :: forall a. NonEmpty a -> (a, [a])
uncons1 (z :| zs) = (z, zs)

-- | cons iso from 'NonEmpty'
consNonEmpty :: Iso (NonEmpty a) (NonEmpty b) (a, [a]) (b, [b])
consNonEmpty = iso uncons1 (uncurry (:|))

-- | snoc iso from 'NonEmpty'
snocNonEmpty :: Iso (NonEmpty a) (NonEmpty b) ([a], a) ([b], b)
snocNonEmpty = iso unsnoc1 (uncurry snoc1)

-- | 'N.groupBy1' but applies the predicate to adjacent elements
groupByAdjacent1 :: forall a. (a -> a -> Bool) -> NonEmpty a -> NonEmpty (NonEmpty a)
groupByAdjacent1 p (a0 :| as0) =
  let (as, ass) = go a0 as0
   in (a0 :| as) :| ass
 where
  go :: a -> [a] -> ([a], [NonEmpty a])
  go a' = \case
    [] -> ([], [])
    a : as ->
      let (ys, zs) = go a as
       in if p a' a
            then (a : ys, zs)
            else ([], (a :| ys) : zs)

-- | partition duplicates elements together with their positiion
findDupsBy :: forall a c. Ord c => (a -> c) -> [a] -> ([NonEmpty (Int, a)], [(Int, a)])
findDupsBy f =
  partitionEithers
    . map g
    . N.groupAllWith (f . snd)
    . zip [0 ..]
 where
  g :: NonEmpty (Int, a) -> Either (NonEmpty (Int, a)) (Int, a)
  g = \case
    x :| [] -> Right x
    x :| y : ys -> Left (x :| y : ys)

-- | "foldMapM" for nonempty containers: uses Semigroup instead of Monoid
foldMapM1 ::
  forall b m f a.
  (Semigroup b, Monad m, Foldable1 f) =>
  (a -> m b) ->
  f a ->
  m b
foldMapM1 f (toNonEmpty -> n :| ns) = foldr step return ns =<< f n
 where
  step :: a -> (b -> m b) -> b -> m b
  step x r z = f x >>= \y -> r $! z <> y

-- | 'Primus.Fold.unfoldM' for nonempty results
unfoldrM1 :: Monad m => (s -> m (a, Maybe s)) -> s -> m (NonEmpty a)
unfoldrM1 f s = do
  (a, ms) <- f s
  case ms of
    Nothing -> return (a :| [])
    Just s' -> (a N.<|) <$> unfoldrM1 f s'

-- | 'take' for a nonempty list
take1 :: Pos -> NonEmpty a -> NonEmpty a
take1 (Pos i) (a :| as) = a :| take (i - 1) as

-- | 'splitAt' for a nonempty list but doesnt guarantee the number of elements
splitAt1 :: Pos -> NonEmpty a -> (NonEmpty a, [a])
splitAt1 (Pos i) (a :| as) = first (a :|) (splitAt (i - 1) as)

-- | comparator for 'Split1'
split1Ordering :: Split1 a -> Ordering
split1Ordering = \case
  SplitLT{} -> LT
  SplitEQ{} -> EQ
  SplitGT{} -> GT

-- | represents the status of a split a nonempty list
data Split1 a
  = SplitLT !Pos
  | SplitEQ
  | SplitGT !(NonEmpty a)
  deriving stock (Ord, Show, Eq)

-- | split a nonempty list preserving information about the split
splitAt1' :: forall a. Pos -> NonEmpty a -> (NonEmpty a, Split1 a)
splitAt1' = go _1P
 where
  go :: Pos -> Pos -> NonEmpty a -> (NonEmpty a, Split1 a)
  go !i !n (a :| [])
    | i == n = (a :| [], SplitEQ)
    | otherwise = (a :| [], SplitLT i)
  go !i !n (a :| a1 : as)
    | i == n = (a :| [], SplitGT (a1 :| as))
    | otherwise =
        let (ys, y) = go (succP i) n (a1 :| as)
         in (a N.<| ys, y)

-- | split a nonempty list but has to have enough elements else fails
splitAt1GE :: Pos -> NonEmpty a -> Either String (NonEmpty a, [a])
splitAt1GE n as =
  let (ns, z) = splitAt1' n as
   in (ns,) <$> case z of
        SplitLT (Pos len) -> Left $ "not enough elements: expected " ++ show (unP n) ++ " found " ++ show len
        SplitEQ -> pure mempty
        SplitGT ex -> pure (N.toList ex)

-- | repeatedly split a nonempty list
splitAts1 :: Pos -> NonEmpty a -> NonEmpty (NonEmpty a)
splitAts1 i = unfoldr1NE (splitAt1 i) . toNonEmpty

-- | compares the length of a potentially infinite nonempty list with "n" and succeeds if they are the same
lengthExact1 :: Pos -> NonEmpty a -> Either String (NonEmpty a)
lengthExact1 n xs =
  let (as, z) = splitAt1' n xs
   in case z of
        SplitLT (Pos len) -> Left $ "LT: not enough elements: expected " ++ show (unP n) ++ " found " ++ show len
        SplitEQ -> Right as
        SplitGT _ -> Left $ "GT: too many elements: expected " ++ show (unP n)

-- | break up a nonempty list into all possible pairs of nonempty lists
splits1 :: forall a. NonEmpty a -> [(NonEmpty a, NonEmpty a)]
splits1 (n :| ns) = go ([n], ns)
 where
  go :: ([a], [a]) -> [(NonEmpty a, NonEmpty a)]
  go = \case
    ([], _) -> []
    (_, []) -> []
    (a : as, b : bs) -> (a :| as, b :| bs) : go (a : as ++ [b], bs)

-- | like 'Data.List.iterate' but allows termination using Maybe
iterateMaybe1' :: (a -> Maybe a) -> a -> NonEmpty a
iterateMaybe1' f a0 = a0 :| go a0
 where
  go a = case f a of
    Nothing -> []
    Just x -> x : go x

-- | like 'iterateMaybe1'' with 'boolMaybe'
iterateMaybe1 :: (a -> Bool) -> (a -> a) -> a -> NonEmpty a
iterateMaybe1 f g = iterateMaybe1' (boolMaybe f g)

-- | iterate "n" times
iterateN1 :: Pos -> (a -> a) -> a -> NonEmpty a
iterateN1 n = take1 n .@ N.iterate

-- | break up a nonempty list into a nonempty list of three parts
splits3 :: forall a. NonEmpty a -> NonEmpty ([a], a, [a])
splits3 (n :| ns) = N.scanl f ([], n, ns) ns
 where
  f :: forall z. ([a], a, [a]) -> z -> ([a], a, [a])
  f (xs, y, zs') _ = case zs' of
    [] -> programmError "splits3"
    z : zs -> (xs ++ [y], z, zs)

-- | like 'Data.List.span' but applies the predicate to adjacent elements
spanAdjacent1 :: (a -> a -> Bool) -> NonEmpty a -> (NonEmpty a, [a])
spanAdjacent1 p (a0 :| as0) = first (a0 :|) (go a0 as0)
 where
  go a' = \case
    [] -> ([], [])
    a : as
      | p a' a -> first (a :) (go a as)
      | otherwise -> ([], a : as)

-- | like 'Data.List.break' but applies the predicate to adjacent elements
breakAdjacent1 :: (a -> a -> Bool) -> NonEmpty a -> (NonEmpty a, [a])
breakAdjacent1 p = spanAdjacent1 (not .@ p)

-- | append a list with a nonempty list
appendL1 :: [a] -> NonEmpty a -> NonEmpty a
appendL1 as bs = foldr N.cons bs as

-- | append a nonempty list with a list
appendR1 :: NonEmpty a -> [a] -> NonEmpty a
appendR1 (a :| as) bs = a :| (as <> bs)

-- | set a value at an index starting at one
setAt1 :: Pos -> a -> NonEmpty a -> Maybe (NonEmpty a)
setAt1 i = updateAt1 i . const

-- | update a value at an index starting at one
updateAt1 :: Pos -> (a -> a) -> NonEmpty a -> Maybe (NonEmpty a)
updateAt1 (Pos i) f ns =
  case N.splitAt (i - 1) ns of
    ([], b : bs) -> Just (f b :| bs)
    (a : as, b : bs) -> Just (a :| as ++ (f b : bs))
    (_, []) -> Nothing

-- | get a value at an index starting at one
at1 :: Pos -> NonEmpty a -> Maybe a
at1 (Pos i) ns =
  case N.splitAt (i - 1) ns of
    (_, b : _) -> Just b
    (_, []) -> Nothing

-- | generate a repeated nonempty list of values for a fixed size
replicateP :: Pos -> a -> NonEmpty a
replicateP (Pos i) a = a :| replicate (i - 1) a

-- | length of nonempty list
lengthP :: Foldable1 t => t a -> Pos
lengthP = unsafePos "lengthP" . N.length . toNonEmpty
{-# INLINE lengthP #-}

-- | generate a nonempty list of units for a fixed size
units1 :: Pos -> NonEmpty ()
units1 = unitsF

-- | generate a nonempty list of units for a given container of the given size
unitsF :: forall l a. (GE.IsList (l a), GE.Item (l a) ~ ()) => Pos -> l a
unitsF = GE.fromList . flip replicate () . unP

-- | sum of nonempty list of 'Pos' values
sumP :: Foldable1 t => t Pos -> Pos
sumP = L.foldr1 (+!)
{-# INLINE sumP #-}
