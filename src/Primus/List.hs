{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

{- |
Module      : Primus.List
Description : list functions
Copyright   : (c) Grant Weyburne, 2022
License     : BSD-3
-}
module Primus.List (
  -- * partition methods
  partitionEithersL,
  partitionEithersL',
  partitionTheseL,
  partitionTheseL',
  partitionM,

  -- * span methods
  spanMaybe,
  spanMaybe',
  lengthExact,
  zipWithLongest,
  zipLongest,

  -- * chunking
  pairsOf1,
  pairsOf2,
  pairsOf',
  chunksOf,

  -- * split methods
  splitAtLGE,
  splits,
  SplitL (..),
  splitAtL,
  atL,
  atNoteL,
  updateAtL,
  setAtL,

  -- * miscellaneous
  allEqual,
  allEqualBy,
  snocL,
  unsnocL,
  unsnocL',
    list,
    list',
  listSnoc,
) where

import Control.Arrow
import Data.Bool
import Data.Either
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as N
import Data.Pos
import Data.These
import GHC.Stack
import Primus.Bool
import Primus.Error

-- | split a list into overlapping pairs plus overflow
pairsOf1 :: [a] -> ([(a, a)], Maybe a)
pairsOf1 = pairsOf' _1P

-- | split a list into non-overlapping pairs plus overflow
pairsOf2 :: [a] -> ([(a, a)], Maybe a)
pairsOf2 = pairsOf' _2P

-- | split into pairs skipping given number of values
pairsOf' :: forall a. Pos -> [a] -> ([(a, a)], Maybe a)
pairsOf' (Pos i) = go
 where
  go :: [a] -> ([(a, a)], Maybe a)
  go =
    \case
      [] -> ([], Nothing)
      [a] -> ([], Just a)
      [a, a'] -> ([(a, a')], Nothing)
      (a : a' : a'' : as) ->
        let (x, y) = go (drop (i - 1) (a' : a'' : as))
         in ((a, a') : x, y)

-- | simple utility for chunking data but guarantees we make progress
chunksOf :: forall a. Pos -> [a] -> [[a]]
chunksOf (Pos n) = L.unfoldr f
 where
  f :: [a] -> Maybe ([a], [a])
  f = \case
    [] -> Nothing
    xs@(_ : _) -> Just (splitAt n xs)

-- | checks that the list has all the same values
allEqual :: Eq a => [a] -> Either (a, a) ()
allEqual = allEqualBy (==)

-- | checks that the list has all the same values with a predicate
allEqualBy :: (a -> a -> Bool) -> [a] -> Either (a, a) ()
allEqualBy f =
  \case
    [] -> pure ()
    [_] -> pure ()
    x : x' : xs
      | f x x' -> allEqualBy f (x' : xs)
      | otherwise -> Left (x, x')

-- | represents the status of a split on a list
data SplitL a
  = SplitLNeg !Pos
  | SplitLLT !Int
  | SplitLEQ
  | SplitLGT !(NonEmpty a)
  deriving stock (Ord, Show, Eq)

-- | split a list preserving information about the split
splitAtL :: forall a. Int -> [a] -> ([a], SplitL a)
splitAtL n xs
  | n < 0 = (xs, SplitLNeg (unsafePos "splitAtL" (-n)))
  | otherwise = go 0 xs
 where
  go :: Int -> [a] -> ([a], SplitL a)
  go i []
    | i == n = ([], SplitLEQ)
    | otherwise = ([], SplitLLT i)
  go i (a : as)
    | i == n = ([], SplitLGT (a :| as))
    | otherwise = first (a :) (go (i + 1) as)

-- | split a list but has to have enough elements else fails
splitAtLGE :: Int -> [a] -> Either String ([a], [a])
splitAtLGE n as =
  let (ns, z) = splitAtL n as
   in (ns,) <$> case z of
        SplitLNeg (Pos j) -> Left $ "negative index " ++ show j
        SplitLLT len -> Left $ "not enough elements: expected " ++ show n ++ " found " ++ show len
        SplitLEQ -> pure mempty
        SplitLGT ex -> pure (N.toList ex)

-- | set a value at a given index in a list
setAtL :: Int -> a -> [a] -> Maybe [a]
setAtL i0 = updateAtL i0 . const

-- | update a value at a given index in a list
updateAtL :: Int -> (a -> a) -> [a] -> Maybe [a]
updateAtL i f as0 =
  case atLImpl i as0 of
    Right (a, (xs, ys)) -> Just (xs <> (f a : ys))
    Left _ -> Nothing

-- | update a value at a given index in a list
atLImpl :: Int -> [a] -> Either String (a, ([a], [a]))
atLImpl i as0 =
  let (xs, ys) = splitAtL i as0
   in case ys of
        SplitLNeg (Pos j) -> Left $ "negative index " ++ show j
        SplitLLT _ -> Left $ "LT: i=" <> show i <> " out of bounds"
        SplitLEQ -> Left $ "EQ: i=" <> show i <> " out of bounds"
        SplitLGT (a :| as) -> Right (a, (xs, as))

-- | index into a list
atL :: Int -> [a] -> Maybe a
atL = either (const Nothing) (Just . fst) .@ atLImpl

-- | unsafe index into a list
atNoteL :: HasCallStack => String -> [a] -> Int -> a
atNoteL msg = fst . forceRight msg .@ flip atLImpl

-- | compares the length of a potentially infinite list with "n" and succeeds if they are the same
lengthExact :: Int -> [a] -> Either String [a]
lengthExact n xs =
  let (as, z) = splitAtL n xs
   in case z of
        SplitLNeg (Pos j) -> Left $ "negative index " ++ show j
        SplitLLT len -> Left $ "LT: expected " ++ show n ++ " found " ++ show len
        SplitLEQ -> Right as
        SplitLGT _ -> Left $ "GT: too many elements: expected " ++ show n

-- | creates the longest of the two lists: fills with 'This' or 'That'
zipWithLongest :: forall a b c. (These a b -> c) -> [a] -> [b] -> [c]
zipWithLongest f = go .@ (,)
 where
  go = \case
    ([], []) -> []
    (xs@(_ : _), []) -> map (f . This) xs
    ([], ys@(_ : _)) -> map (f . That) ys
    (x : xs, y : ys) -> f (These x y) : go (xs, ys)

-- | 'zipWithLongest' for 'id'
zipLongest :: [a] -> [b] -> [These a b]
zipLongest = zipWithLongest id

-- | break up a list into all possible pairs of nonempty lists: see 'Primus.NonEmpty.splits1'
splits :: forall a. [a] -> [([a], [a])]
splits = \case
  [] -> []
  x : xs -> go ([x], xs)
 where
  go :: ([a], [a]) -> [([a], [a])]
  go = \case
    ([], _) -> []
    (_, []) -> []
    (a : as, b : bs) -> (a : as, b : bs) : go (a : as ++ [b], bs)

-- | like 'Data.List.partition' but allow the user to change the types of "e" and "b" using 'Either'
partitionEithersL' :: Foldable t => (a -> Either e b) -> t a -> ([e], [b])
partitionEithersL' f = partitionEithers . foldr ((:) . f) []

-- | like 'partitionEithersL'' using 'Primus.Bool.boolEither'
partitionEithersL :: Foldable t => (a -> Bool) -> (a -> e) -> (a -> b) -> t a -> ([e], [b])
partitionEithersL p l r = partitionEithers . foldr ((:) . boolEither p l r) []

-- | like 'Data.List.partition' but allow the user to change the types of "e" and "b" using 'These'
partitionTheseL' :: Foldable t => (a -> These e b) -> t a -> ([e], [b], [(e, b)])
partitionTheseL' f = partitionThese . foldr ((:) . f) []

-- | like 'partitionTheseL' using 'Primus.Bool.boolThese'
partitionTheseL :: Foldable t => (a -> Bool) -> (a -> Bool) -> (a -> e) -> (a -> b) -> t a -> ([e], [b], [(e, b)])
partitionTheseL p q l r = partitionThese . foldr ((:) . boolThese p q l r) []

-- | like 'Data.List.span' but allow the user to change the success type using 'Maybe'
spanMaybe' :: (a -> Maybe b) -> [a] -> ([b], [a])
spanMaybe' f = go
 where
  go = \case
    [] -> ([], [])
    a : as -> case f a of
      Nothing -> ([], a : as)
      Just b -> first (b :) (go as)

-- | like 'spanMaybe'' using 'Primus.Bool.boolMaybe'
spanMaybe :: (a -> Bool) -> (a -> b) -> [a] -> ([b], [a])
spanMaybe p r = spanMaybe' (boolMaybe p r)

-- | partition for an applicative
partitionM :: Applicative m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM f = go
 where
  go = \case
    [] -> pure mempty
    a : as -> (\b -> bool first second b (a :)) <$> f a <*> go as

-- | break up a list into cases using cons
list :: b -> (a -> [a] -> b) -> [a] -> b
list z s = \case
  [] -> z
  a:as -> s a as

-- | break up a list into cases using cons (argument order is flipped from 'list')
list' :: [a] -> b -> (a -> [a] -> b) -> b
list' as z s = list z s as

-- | break up a list into cases using snoc
listSnoc :: b -> ([a] -> a -> b) -> [a] -> b
listSnoc z s = maybe z (uncurry s) . unsnocL

-- | snoc for a list
snocL :: [a] -> a -> [a]
snocL as a = as ++ [a]

-- | unsnoc for a list
unsnocL :: [a] -> Maybe ([a], a)
unsnocL = list Nothing (Just .@ unsnocL')

-- | unsnoc for a value and a list
unsnocL' :: a -> [a] -> ([a], a)
unsnocL' a =
  \case
    [] -> ([], a)
    x : xs -> first (a :) (unsnocL' x xs)
