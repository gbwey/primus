{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{- |
Module      : Primus.Enum
Description : methods for safe enumeration and enumeration on containers
Copyright   : (c) Grant Weyburne, 2022
License     : BSD-3
-}
module Primus.Enum (
  -- * enumerations
  universe1,
  universe1R,
  enumFrom1,
  enumFrom1R,
  enumTo1,
  enumFromThen1,
  enumFromTo1,
  enumFromThenTo1,

  -- * converters
  predSafe,
  succSafe,
  integerToEnumSafe,
  integerToIntSafe,

  -- * container enums

  -- ** enumerations
  toEnumList,
  toEnumList1,
  universeTraversable,
  toEnumTraversable,
  -- calcNextEnum,
  -- minMax,
  -- zerolr

  -- ** converters
  succTraversable,
  predTraversable,
  fromEnumFoldable,
  fromEnumFoldable1,

  -- ** capacity
  capacity,
) where

import Control.Arrow
import Data.Foldable
import Data.Function
import Data.Functor
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty (..))
import Data.Ord
import Data.Semigroup.Foldable
import Primus.Error
import Primus.Fold

-- | create a nonempty list of all the values for an 'Enum'
universe1 :: forall a. (Bounded a, Enum a) => NonEmpty a
universe1 = enumFrom1 minBound

-- | create a nonempty list of all the values for an 'Enum' in reverse order
universe1R :: forall a. (Bounded a, Enum a) => NonEmpty a
universe1R = enumFrom1R maxBound

-- | create a nonempty list of values starting at "a"
enumFrom1 :: (Bounded a, Enum a) => a -> NonEmpty a
enumFrom1 a = a :| drop 1 [a .. maxBound]

-- | create a nonempty list of values starting at "a"
enumTo1 :: (Bounded a, Enum a) => a -> NonEmpty a
enumTo1 a = minBound :| drop 1 [minBound .. a]

-- | create a nonempty list of values starting at "a" and skipping "b"
enumFromThen1 :: (Bounded a, Enum a) => a -> a -> NonEmpty a
enumFromThen1 a b =
  case comparing fromEnum a b of
    LT -> a :| drop 1 [a, b .. maxBound]
    EQ -> a :| [] -- diverges from enumFromThen by returning one value instead of cycling
    GT -> a :| drop 1 [a, b .. minBound]

-- | 'enumFromThenTo' for nonempty lists
enumFromThenTo1 :: Enum a => a -> a -> a -> NonEmpty a
enumFromThenTo1 a b c =
  if comparing fromEnum a b == EQ
    then a :| [] -- diverges from enumFromThenTo by returning one value instead of cycling
    else a :| drop 1 [a, b .. c]

-- | create a nonempty list of values starting at "a" and skipping "b"
enumFromTo1 :: Enum a => a -> a -> NonEmpty a
enumFromTo1 a b =
  case comparing fromEnum a b of
    LT -> a :| drop 1 [a .. b]
    EQ -> a :| [] -- diverges from enumFromTo by returning one value instead of cycling
    GT -> a :| drop 1 [a, pred a .. b] -- diverges from enumFromTo by going backwards instead of returning nothing
    -- pred has to exist: a > b => pred a >= b unless float ...

-- | create a nonempty list of "a" in reverse order
enumFrom1R :: forall a. (Bounded a, Enum a) => a -> NonEmpty a
enumFrom1R a
  | Just prv <- predSafe a = a :| drop 1 [a, prv .. minBound]
  | otherwise = a :| []

-- | safe 'pred' for a bounded 'Enum'
predSafe :: (Bounded a, Enum a) => a -> Maybe a
predSafe a
  | on (==) fromEnum a minBound = Nothing
  | otherwise = Just (pred a)

-- | safe 'succ' for a bounded 'Enum'
succSafe :: (Bounded a, Enum a) => a -> Maybe a
succSafe a
  | on (==) fromEnum a maxBound = Nothing
  | otherwise = Just (succ a)

-- | load a given container with "a"s using the relative position "i"
toEnumTraversable ::
  forall a f z.
  (Traversable f, Enum a, Bounded a) =>
  f z ->
  Integer ->
  Either String (f a)
toEnumTraversable tz i = do
  lst <- toEnumList @a i
  z <- zerolr
  c <- capacity @a tz
  lmsg ("cap=" ++ show c) $ padL (z <$ tz) lst

zerolr :: forall a. (Bounded a, Enum a) => Either String a
zerolr = left (const "zerolr: not defined at zero") $ integerToEnumSafe @a 0

-- | calculates the minimum and maximum range of enumerations that can be stored in a container of the given size
capacity :: forall a t z. (Bounded a, Enum a, Foldable t) => t z -> Either String (Integer, Integer)
capacity (length -> len) = do
      let z@(mn, mx) = minMax @a
      lhs <- case compare mn 0 of
            LT -> Right (-(-mn + 1) ^ len + 1)
            EQ -> Right 0
            GT -> Left $ "capacity: unsupported mn > 0: " ++ show z
      rhs <- case compare 0 mx of
            LT -> Right ((mx + 1) ^ len - 1)
            EQ -> Right 0
            GT -> Left $ "capacity: unsupported mx < 0: " ++ show z
      pure (lhs,rhs)

{- | convert toEnum of "a" into a list containing "a"s
   zero is the empty list: see 'toEnumList'
-}
toEnumList :: forall a. (Enum a, Bounded a) => Integer -> Either String [a]
toEnumList i
  | i == 0 = [] <$ zerolr @a
  | otherwise =
      let f :: Integer -> Either String (Maybe (a, Integer))
          f s
            | s == 0 = pure Nothing
            | otherwise =
                calcNextEnum s <&> \(s', a) ->
                  if abs s' < abs s
                    then Just (a, s')
                    else
                      if s' == 0
                        then Nothing
                        else programmError "toEnumList"
       in unfoldlM f i

-- | calculate the next enum
calcNextEnum :: forall a. (Enum a, Bounded a) => Integer -> Either String (Integer, a)
calcNextEnum i = lmsg "calcNextEnum" $
  case compare i 0 of
    GT
      | mx > 0 ->
          let (a, b) = divMod i (mx + 1)
           in case integerToEnumSafe b of
                Left e -> Left $ "out of range(GT): " ++ show i ++ " mod " ++ show (mx + 1) ++ " == " ++ show b ++ "(undefined) e=" ++ e
                Right c -> Right (a, c)
      | otherwise -> Left "not defined for positive numbers"
    EQ -> (0,) <$> zerolr
    LT
      | mn < 0 ->
          let (a, b) = quotRem i (mn - 1)
           in case integerToEnumSafe b of
                Left e -> Left $ "out of range(LT): " ++ show i ++ " mod " ++ show (mn - 1) ++ " == " ++ show b ++ "(undefined) e=" ++ e
                Right c -> Right (-a, c)
      | otherwise -> Left "not defined for negative numbers"
 where
  (mn, mx) = minMax @a

-- | return the min and max of a bounded enum
minMax :: forall a. (Enum a, Bounded a) => (Integer, Integer)
minMax = on (,) (toInteger . fromEnum @a) minBound maxBound

-- | concrete safe conversion of Integer to Int
integerToIntSafe :: Integer -> Either String Int
integerToIntSafe = integerToEnumSafe

-- | safe 'toEnum'
integerToEnumSafe :: forall a. (Enum a, Bounded a) => Integer -> Either String a
integerToEnumSafe i
  | i < mn = Left $ msg "underflow"
  | i > mx = Left $ msg "overflow"
  | otherwise = pure $ toEnum @a $ fromInteger @Int i -- i <= maxBound  && maxBound :: Int so cant fail on fromInteger
 where
  (mn, mx) = minMax @a
  msg s = "integerToEnumSafe:" ++ s ++ " where " ++ show i ++ " not in range [" ++ show mn ++ ".." ++ show mx ++ "]"

-- | convert toEnum of "a" into a nonempty list containing "a"s
toEnumList1 :: forall a. (Enum a, Bounded a) => Integer -> Either String (NonEmpty a)
toEnumList1 i =
  toEnumList i <&> \case
    [] -> minBound :| []
    a : as -> a :| as

-- | reverse of 'toEnumList' [can fail if xs is null and toEnum 0 is not defined]
fromEnumFoldable ::
  forall a t.
  (Foldable t, Enum a, Bounded a) =>
  t a ->
  Either String Integer
fromEnumFoldable xs =
  case toList xs of
    [] -> 0 <$ zerolr @a
    a : as -> pure $ fromEnumFoldable1 (a :| as)

-- | reverse of 'toEnumList1' [cant fail]
fromEnumFoldable1 ::
  forall a t.
  (Foldable1 t, Enum a, Bounded a) =>
  t a ->
  Integer
fromEnumFoldable1 xs =
  let (mn, mx) = minMax @a
      nn, pp :: Maybe Integer
      nn = if mn < 0 then Just (-1) else Nothing
      pp = if mx > 0 then Just 1 else Nothing
      f ::
        a ->
        (Integer, (Maybe Integer, Maybe Integer)) ->
        (Integer, (Maybe Integer, Maybe Integer))
      f a (b, (n, p)) =
        let v = toInteger (fromEnum a)
            w = case (compare v 0, n, p) of
              (LT, Just x, _) -> b - v * x
              (EQ, _, _) -> b
              (GT, _, Just y) -> b + v * y
              o -> programmError $ "fromEnumFoldable1 " ++ show o
         in (w, ((\x -> -(mn - 1) * x) <$> n, (\x -> (mx + 1) * x) <$> p))
   in fst $ foldr f (0, (nn, pp)) xs

-- | 'succ' for a traversable container
succTraversable ::
  forall a t.
  (Traversable t, Enum a, Bounded a) =>
  t a ->
  Either String (t a)
succTraversable xs =
  let f :: Bool -> a -> (Bool, a)
      f b a =
        case (b, succSafe a) of
          (True, Just a') -> (False, a')
          (True, Nothing) -> (True, minBound)
          _o -> (b, a)
      (lft, ret) = L.mapAccumR f True xs
   in if lft
        then Left "succTraversable: over maxbound"
        else Right ret

-- | 'pred' for a traversable container
predTraversable ::
  forall a t.
  (Traversable t, Enum a, Bounded a) =>
  t a ->
  Either String (t a)
predTraversable xs =
  let f :: Bool -> a -> (Bool, a)
      f b a =
        case (b, predSafe a) of
          (True, Just a') -> (False, a')
          (True, Nothing) -> (True, maxBound)
          _o -> (b, a)
      (lft, ret) = L.mapAccumR f True xs
   in if lft
        then Left "predTraversable: below minbound"
        else Right ret

{- | generate all the possible enum combinations for the given container in ascending order

 useful for creating all the valid indices for matrices
-}
universeTraversable ::
  forall f a.
  (Traversable f, Enum a, Bounded a) =>
  f a ->
  Either String (NonEmpty (f a))
universeTraversable ta = do
  (mn, mx) <- capacity @a ta
  traverse (toEnumTraversable ta) (mn :| drop 1 [mn .. mx])
