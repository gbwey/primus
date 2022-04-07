{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Primus.Fold
Description : fold and unfolds
Copyright   : (c) Grant Weyburne, 2022
License     : BSD-3
-}
module Primus.Fold (
  -- * fill a container
  fillTraversable,
  fillTraversableExact,
  traverseLR,

  -- * extended traversals with access to past and future input
  histMapL,
  histMapR,
  histMapL',
  histMapR',

  -- * change inside of a container
  wrapL,
  wrap1,

  -- * fold and unfolds
  pFoldR,
  pFoldL,
  unfoldl,
  unfoldrM,
  unfoldlM,

  -- * zip
  zipExtrasT,
  zipExtrasRight,
  zipWithExact,
  zipExact,
  zipWithT,

  -- * compare container lengths
  CLCount (..),
  compareLength,
  compareLengthBy,
  compareLengths,
  clOrdering,

  -- * pad containers
  padR,
  padL,

  -- * chunking
  chunkN,
  chunkN',

  -- * scan
  postscanl,
  postscanr,

  -- * miscellaneous
  initsT,
  tailsT,
  reverseT,
  sortByT,
  unzipF,
  reverseF,
) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Bool
import Data.Foldable
import Data.Kind
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as N
import Data.Semigroup.Foldable
import Data.These
import Data.These.Combinators
import Primus.Error
import Primus.Extra

data Hist a b = Hist ![a] ![a] !b

getHistZ :: Hist a b -> b
getHistZ (Hist _ _ z) = z

{- | left fold over a list giving the caller access to past and future input and state "z"
 if you want previous "b" values then put it in "z"
-}
histMapImpl ::
  Traversable t =>
  Bool ->
  ([a] -> [a] -> z -> a -> (z, b)) ->
  z ->
  t a ->
  (z, t b)
histMapImpl isright f z0 lst =
  first getHistZ $
    bool
      L.mapAccumL
      L.mapAccumR
      isright
      g
      (Hist [] (bool toList reverseF isright lst) z0)
      lst
 where
  g (Hist ps ft z) a =
    case ft of
      [] -> programmError "histMapImpl: ran out of data!"
      _ : ft0 ->
        let (z', b) = f ps ft0 z a
         in (Hist (a : ps) ft0 z', b)

-- | left/right fold over a list giving the caller access state "z" (for finite containers only)
histMapL
  , histMapR ::
    Traversable t =>
    ([a] -> [a] -> z -> a -> (z, b)) ->
    z ->
    t a ->
    (z, t b)
histMapL = histMapImpl False
histMapR = histMapImpl True

-- | left/right fold that gives access to past input (reverse order) and future input
pFoldL, pFoldR :: forall a b. ([a] -> [a] -> b -> a -> b) -> b -> [a] -> b
pFoldR f n = go []
 where
  go :: [a] -> [a] -> b
  go pres = \case
    [] -> n
    a : as -> f pres as (go (a : pres) as) a
pFoldL f = go []
 where
  go :: [a] -> b -> [a] -> b
  go pres !z = \case
    [] -> z
    a : as -> go (a : pres) (f pres as z a) as

histMapImpl' ::
  forall a b t.
  Traversable t =>
  Bool ->
  ([a] -> [a] -> a -> b) ->
  t a ->
  t b
histMapImpl' isright f = snd . bool histMapL histMapR isright g ()
 where
  g :: [a] -> [a] -> () -> a -> ((), b)
  g ps ft () a = ((), f ps ft a)

-- | same as 'histMapL' or 'histMapR' but skips state
histMapL'
  , histMapR' ::
    forall a b t.
    Traversable t =>
    ([a] -> [a] -> a -> b) ->
    t a ->
    t b
histMapL' = histMapImpl' False
histMapR' = histMapImpl' True

-- | like 'Data.List.unfoldr' but reverses the order of the list
unfoldl :: forall s a. (s -> Maybe (a, s)) -> s -> [a]
unfoldl f = go []
 where
  go :: [a] -> s -> [a]
  go as !s = case f s of
    Nothing -> as
    Just (a, s1) -> go (a : as) s1

-- | monadic unfoldr
unfoldrM :: forall m s a. Monad m => (s -> m (Maybe (a, s))) -> s -> m [a]
unfoldrM f s = do
  mas <- f s
  case mas of
    Nothing -> return []
    Just (a, s') -> (a :) <$> unfoldrM f s'

-- | monadic unfoldl
unfoldlM :: forall m s a. Monad m => (s -> m (Maybe (a, s))) -> s -> m [a]
unfoldlM f = go []
 where
  go :: [a] -> s -> m [a]
  go as s = do
    mas <- f s
    case mas of
      Nothing -> return as
      Just (a, s') -> go (a : as) s'

-- | traverse a container using 'StateLR'
traverseLR ::
  forall t a b c.
  Traversable t =>
  (c -> a -> Either String (c, b)) ->
  c ->
  t a ->
  Either String (c, t b)
traverseLR f c0 ta =
  let g :: a -> StateLR String c b
      g a = StateLR $ \c -> f c a
   in unStateLR (traverse g ta) c0

-- | fill a traversable with a list and fail if not enough data
fillTraversable ::
  forall t a z.
  Traversable t =>
  t z ->
  [a] ->
  Either String ([a], t a)
fillTraversable tz as0 =
  let g :: z -> StateLR String [a] a
      g _ = StateLR $ \case
        [] -> Left "fillTraversable: not enough data"
        d : ds' -> Right (ds', d)
   in unStateLR (traverse g tz) as0

-- | fill a traversable with a list and fail if there are leftovers: see 'fillTraversable'
fillTraversableExact ::
  forall f a z.
  Traversable f =>
  f z ->
  [a] ->
  Either String (f a)
fillTraversableExact = g .@ fillTraversable
 where
  g :: Either String ([a], b) -> Either String b
  g = \case
    Right ([], ret) -> Right ret
    Right (_ : _, _) -> Left "fillTraversableExact: too many elements found"
    Left e -> Left e

-- | run a function against the contents of the 'Foldable1' container as a nonempty list
wrap1 ::
  forall (g :: Type -> Type) a b.
  (Traversable g, Foldable1 g) =>
  (NonEmpty a -> NonEmpty b) ->
  g a ->
  Either String (g b)
wrap1 f gx = fillTraversableExact gx (toList (f (toNonEmpty gx)))

-- | run a function against the contents of the 'Foldable' container as a list
wrapL ::
  forall (g :: Type -> Type) a b.
  (Traversable g) =>
  ([a] -> [b]) ->
  g a ->
  Either String (g b)
wrapL f gx = fillTraversableExact gx (f (toList gx))

-- | pad fill "as" to the right or left with values from "zs"
padR, padL :: forall t a. Traversable t => t a -> [a] -> Either String (t a)
padR = padImpl True
padL = padImpl False

-- | pad fill "as" to the left/right with values from "zs"
padImpl :: forall t a. Traversable t => Bool -> t a -> [a] -> Either String (t a)
padImpl isright as zs =
  let (rs, zz) = bool (L.mapAccumR f (reverseF zs)) (L.mapAccumL f zs) isright as
   in case rs of
        [] -> Right zz
        _ : _ -> Left $ "pad" ++ bool "L" "R" isright ++ ": negative fill: would need to truncate the data"
 where
  f :: [a] -> a -> ([a], a)
  f xs a =
    case xs of
      [] -> ([], a)
      b : bs -> (bs, b)

-- | have to call a second time if the left container is bigger than the right one
zipExtrasT :: forall a b t. Traversable t => t a -> t b -> t (These a b)
zipExtrasT xs ys =
  let (rs, ret) = zipExtrasRight (toList xs) ys
   in case rs of
        [] -> ret
        _ : _ -> swapThese <$> zipExtrasT ys xs

-- | zip a foldable into a traversable container and return any leftovers
zipExtrasRight ::
  forall a b t.
  Traversable t =>
  [a] ->
  t b ->
  ([a], t (These a b))
zipExtrasRight = L.mapAccumL f
 where
  f :: [a] -> b -> ([a], These a b)
  f zs b = case zs of
    [] -> ([], That b)
    a : as -> (as, These a b)

-- | predicate for 'CEQ'
clOrdering :: CLCount b -> Maybe Ordering
clOrdering = \case
  CError{} -> Nothing
  CLT{} -> Just LT
  CEQ -> Just EQ
  CGT -> Just GT

-- | difference between two foldables but quick exit if lhs is larger than rhs
data CLCount b
  = -- | error
    CError !String
  | -- | leftovers from rhs: ie lhs is smaller than rhs
    CLT !(NonEmpty b)
  | -- | same size
    CEQ
  | -- | lhs is larger than rhs
    CGT
  deriving stock (Ord, Show, Eq, Functor, Traversable, Foldable)

-- | compare lengths of foldables
compareLengths :: Foldable t => NonEmpty (t a) -> [CLCount a]
compareLengths (xs :| xss) = map (compareLengthBy mempty xs) xss

-- | compare length where lhs or rhs can be infinite but not both
compareLength ::
  forall t u a b.
  (Foldable t, Foldable u) =>
  t a ->
  u b ->
  CLCount b
compareLength = compareLengthBy mempty

-- | compare length where lhs or rhs can be infinite but not both
compareLengthBy ::
  forall t u a b.
  (Foldable t, Foldable u) =>
  (Int -> a -> b -> Maybe String) ->
  t a ->
  u b ->
  CLCount b
compareLengthBy p xs ys =
  foldr f g xs (0, toList ys)
 where
  g :: (Int, [b]) -> CLCount b
  g (_, zs) = case zs of
    [] -> CEQ
    w : ws -> CLT (w :| ws)
  f :: a -> ((Int, [b]) -> CLCount b) -> (Int, [b]) -> CLCount b
  f a k (i, zs) = case zs of
    [] -> CGT -- quickexit
    b : bs -> case p i a b of
      Nothing -> k (i + 1, bs)
      Just e -> CError e

-- | 'zipWith' with an Applicative result
zipWithT ::
  (Applicative f, Traversable t, Applicative t) =>
  (a -> b -> f c) ->
  t a ->
  t b ->
  f (t c)
zipWithT f = sequenceA .@ liftA2 f

-- | fills a container with chunks using a user supplied unfold function
chunkN ::
  forall t s b z.
  Traversable t =>
  (s -> Either String (s, b)) ->
  t z ->
  s ->
  Either String (s, t b)
chunkN f tz = unStateLR (traverse (const (StateLR f)) tz)

-- | similar to 'chunkN' but "s" is restricted to a foldable: if there is data left then will fail
chunkN' ::
  forall t a u b z.
  (Traversable t, Foldable u) =>
  (u a -> Either String (u a, b)) ->
  t z ->
  u a ->
  Either String (t b)
chunkN' f tz s = do
  (s', ret) <- chunkN g tz s
  if null s'
    then Right ret
    else Left "chunkN': there is still data remaining at eof"
 where
  g s' =
    if null s'
      then Left "chunkN': not enough data"
      else f s'

-- | splits a container "u" into parts of length "len" and fills container "t"
zipWithExact ::
  forall t u a b c.
  (Traversable t, Foldable u) =>
  (a -> b -> c) ->
  t a ->
  u b ->
  Either String (t c)
zipWithExact f ta ub = do
  let g a = StateLR $ \case
        [] -> Left "zipWithExact: lhs has more data"
        b : bs -> Right (bs, f a b)
  (vx, ret) <- unStateLR (traverse g ta) (toList ub)
  if null vx
    then Right ret
    else Left "zipWithExact: lhs has less data"

-- | see 'zipWithExact'
zipExact ::
  forall t u a b.
  (Traversable t, Foldable u) =>
  t a ->
  u b ->
  Either String (t (a, b))
zipExact = zipWithExact (,)

-- | combines state and failure as a monad
newtype StateLR e s a = StateLR {unStateLR :: s -> Either e (s, a)}
  deriving stock (Functor)

instance Applicative (StateLR e s) where
  pure a = StateLR $ \s -> Right (s, a)
  (<*>) = ap

instance Monad (StateLR e s) where
  return = pure
  StateLR sa >>= amb =
    StateLR $ \s -> case sa s of
      Left e -> Left e
      Right (s1, a) -> unStateLR (amb a) s1

-- | 'Data.List.inits' for a traversable container
initsT :: forall a t. Traversable t => t a -> t (NonEmpty a)
initsT ta = case toList ta of
  [] -> fmap pure ta
  i : is -> frp $ fillTraversableExact ta (map (i :|) (L.inits is))

-- | 'Data.List.tails' for a traversable container
tailsT :: forall a t. Traversable t => t a -> t (NonEmpty a)
tailsT ta = forceRight "tailsT" $ do
  (xs, ret) <- traverseLR g (toList ta) ta
  case xs of
    [] -> pure ret
    _ : _ -> Left "extra data at eof"
 where
  g :: [a] -> p -> Either String ([a], NonEmpty a)
  g s _ = case s of
    [] -> Left "ran out of data"
    a : as -> Right (as, a :| as)

-- | 'Data.List.reverse' for a traversable container
reverseT :: forall a t. Traversable t => t a -> t a
reverseT = frp . wrapL reverseF

-- | 'Data.List.sortBy' for a traversable container
sortByT :: forall a t. Traversable t => (a -> a -> Ordering) -> t a -> t a
sortByT f = frp . wrapL (L.sortBy f)

-- | 'N.scanr' for a traversable that drops the last value
postscanr :: Traversable f => (a -> b -> b) -> b -> f a -> f b
postscanr f c = frp . wrapL (N.init . N.scanr f c)

-- | 'N.scanl' for a traversable that drops the first value
postscanl :: Traversable f => (b -> a -> b) -> b -> f a -> f b
postscanl f c = frp . wrapL (N.tail . N.scanl f c)

-- | unzip for a functor of pairs
unzipF :: Functor f => f (a, b) -> (f a, f b)
unzipF = fmap fst &&& fmap snd

-- | reverse a foldable
reverseF :: Foldable t => t a -> [a]
reverseF = foldl' (flip (:)) []
