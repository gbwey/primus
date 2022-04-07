{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- need PolyKinds else could fail for callers using TP type families
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : Primus.LRHist
Description : like 'Either' but keeps history of all successes
Copyright   : (c) Grant Weyburne, 2022
License     : BSD-3

tracks one or more successes and optionally a failure

prefer the smart constructors to enforce correctness or use the apply methods
-}
module Primus.LRHist (
  -- * datatype
  LRHist (..),

  -- * smart constructors
  rhi,
  rh,
  lh,
  lhskip,

  -- * constructors with better type application order
  rhi',
  rh',
  lh',
  lhskip',

  -- * converters
  lhToEitherI,
  lhToEither,
  lhToEitherTuples,

  -- * function application
  lhBool,
  lhMaybe,
  lhMaybe',
  lhEither,
  lhEither',
  appLR,
  appLRS,
  appLRB,

  -- * traversals
  traverseLRHistB,
  traverseLRHist,

  -- * miscellaneous
  eitherToLH,
  maybeToLH,
  validateLRHist,
) where

import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Bool
import Data.Kind
import qualified Data.List as L
import Data.Proxy
import Data.These
import qualified GHC.Read as GR
import Primus.AsMaybe
import Primus.Bool
import Primus.Error (programmError)
import qualified Primus.TypeLevel as TP
import qualified Text.ParserCombinators.ReadPrec as PC
import qualified Text.Read.Lex as TRL

{- | like 'Either' but keeps track of history of all successes
  if there is a failure it wraps the previous successes and stops adding data to 'LRHist'
 "e" the error type
 "as" is the typelevel list in reverse order that tracks all previous "a"s
 "a" is the latest success type
-}
type LRHist :: [Type] -> Type -> Type -> Type
data LRHist as e a where
  -- | wraps an existing error
  LhSkip ::
    LRHist as e a' ->
    LRHist (a' ': as) e a
  -- | wraps previous nested successes with an error
  Lh ::
    e ->
    LRHist as e a' ->
    LRHist (a' ': as) e a
  -- | initial success value
  Rhi ::
    a ->
    LRHist '[] e a
  -- | subsequent success
  Rh ::
    a ->
    LRHist as e a' ->
    LRHist (a' ': as) e a

deriving stock instance Functor (LRHist as e)
deriving stock instance Foldable (LRHist as e)
deriving stock instance Traversable (LRHist as e)
deriving stock instance (Show a, Show e, TP.ApplyConstraints '[Show] as) => Show (LRHist as e a)
deriving stock instance (TP.ApplyConstraints '[Eq, Ord] as, Eq e, Ord e, Ord a) => Ord (LRHist as e a)
deriving stock instance (TP.ApplyConstraints '[Eq] as, Eq e, Eq a) => Eq (LRHist as e a)
instance
  (Semigroup e, Monoid a) =>
  Monoid (LRHist '[] e a)
  where
  mempty = Rhi mempty
instance
  ( Monoid a
  , Monoid e
  , Monoid a'
  , TP.ApplyConstraints '[Semigroup, Monoid] as
  , Monoid (LRHist as e a')
  ) =>
  Monoid (LRHist (a' ': as) e a)
  where
  mempty = Rh mempty mempty

instance
  ( Semigroup a
  , Semigroup e
  , TP.ApplyConstraints '[Semigroup] as
  ) =>
  Semigroup (LRHist as e a)
  where
  x <> y = case (x, y) of
    (Rhi a, Rhi a') -> Rhi (a <> a')
    (Rh a ls, Rh a' ls') -> Rh (a <> a') (ls <> ls')
    (Lh e ls, Lh e' ls') -> Lh (e <> e') (ls <> ls')
    (LhSkip ls, LhSkip ls') -> LhSkip (ls <> ls')
    (z@LhSkip{}, _) -> z
    (_, z@LhSkip{}) -> z
    (z@Lh{}, _) -> z
    (_, z@Lh{}) -> z

-- | constructor for 'Rhi' with more convenient type application order
rhi' :: forall e a. a -> LRHist '[] e a
rhi' = Rhi

-- | constructor for 'Rh' with more convenient type application order
rh' :: forall e a a' as. a -> LRHist as e a' -> LRHist (a' : as) e a
rh' = Rh

-- | constructor for 'Lh' with more convenient type application order
lh' :: forall a e a' as. e -> LRHist as e a' -> LRHist (a' : as) e a
lh' = Lh

-- | constructor for 'LhSkip' with more convenient type application order
lhskip' :: forall a e a' as. LRHist as e a' -> LRHist (a' : as) e a
lhskip' = LhSkip

-- | smart constructor for 'Rhi'
rhi :: forall e a. a -> (Proxy 'True, LRHist '[] e a)
rhi a = (Proxy, Rhi a)

-- | smart constructor for 'Rh'
rh ::
  forall e a a' as proxy.
  a ->
  (proxy 'True, LRHist as e a') ->
  (proxy 'True, LRHist (a' : as) e a)
rh a = second (Rh a)

-- | smart constructor for 'Lh'
lh ::
  forall a e a' as proxy.
  e ->
  (proxy 'True, LRHist as e a') ->
  (Proxy 'False, LRHist (a' : as) e a)
lh e (_p, z) = (Proxy, Lh e z)

-- | smart constructor for 'LhSkip'
lhskip ::
  forall a e a' as proxy.
  (proxy 'False, LRHist as e a') ->
  (proxy 'False, LRHist (a' : as) e a)
lhskip = second LhSkip

-- | initialise 'LRHist' with an 'Either' by wrapping a unit
eitherToLH :: Either e a -> LRHist '[()] e a
eitherToLH lr = either Lh Rh lr (Rhi ())

-- | initialise 'LRHist' with an 'Maybe' by wrapping a unit
maybeToLH :: Monoid e => Maybe a -> LRHist '[()] e a
maybeToLH = eitherToLH . maybe (Left mempty) Right

-- | returns an inductive tuple on success and Either for failure
lhToEitherI ::
  forall e a as.
  RHistC as =>
  LRHist as e a ->
  Either e (RHistT a as)
lhToEitherI = rhist

-- | convert 'LRHist' to an 'Either'
lhToEither :: forall e a as. LRHist as e a -> Either e a
lhToEither = \case
  Rhi a -> Right a
  Rh a _ -> Right a
  Lh e _ -> Left e
  z@LhSkip{} -> Left $ go z
 where
  go :: forall as' a'. LRHist as' e a' -> e
  go = \case
    Rhi{} -> programmError "malformed LRHist: LhSkip expects inner LhSkip or Lh but found Rhi"
    Rh{} -> programmError "malformed LRHist: LhSkip expects LhSkip or Lh but found Rh"
    Lh e _ -> e
    LhSkip ls -> go ls

-- | extract the initial type for 'LRHist'
type OrgAT :: [Type] -> Type -> Type
type family OrgAT as a where
  OrgAT '[] a' = a'
  OrgAT (a ': as) _ = OrgAT as a

-- | extracts the initial value from 'LRHist'
type OrgAC :: [Type] -> Type -> Constraint
class OrgAC as a where
  orgA :: LRHist as e a -> OrgAT as a

instance OrgAC '[] a' where
  orgA = \case
    Rhi a -> a
instance OrgAC as a => OrgAC (a ': as) a' where
  orgA = \case
    Rh _ ls -> orgA ls
    Lh _ ls -> orgA ls
    LhSkip ls -> orgA ls

-- | returns flattened n-tuple with all the history of successes on success and Either for failure
lhToEitherTuples ::
  forall e a as tp.
  ( TP.ITupleC tp
  , RHistC as
  , TP.ToITupleT tp ~ RHistT a as
  ) =>
  LRHist as e a ->
  Either e tp
lhToEitherTuples = fmap TP.fromITupleC . rhist

-- | type family for creating an inductive tuple
type RHistT :: Type -> [Type] -> Type
type family RHistT a as where
  RHistT a '[] = (a, ())
  RHistT a (a' ': as) = (a, RHistT a' as)

-- | return an inductive tuple on success
type RHistC :: [Type] -> Constraint
class RHistC as where
  rhist :: LRHist as e a -> Either e (RHistT a as)

instance RHistC '[] where
  rhist = \case
    Rhi a -> Right (a, ())
instance RHistC as => RHistC (a ': as) where
  rhist = \case
    Rh a ls -> (a,) <$> rhist ls
    Lh e _ -> Left e
    LhSkip ls -> case rhist ls of
      Left e -> Left e
      Right _a -> programmError "malformed LRHist: LhSkip wrapping Rh or Rhi"

-- | validate that the composition of constructors for 'LRHist' is valid
validateLRHist :: forall e a as. LRHist as e a -> Either String ()
validateLRHist =
  \case
    Rhi{} -> Right ()
    Rh _ ls -> case ls of
      Lh{} -> Left "Rh cannot wrap Lh"
      LhSkip{} -> Left "Rh cannot wrap LhSkip"
      Rhi{} -> validateLRHist ls
      Rh{} -> validateLRHist ls
    Lh _ ls -> case ls of
      Lh{} -> Left "Lh cannot wrap Lh"
      LhSkip{} -> Left "Lh cannot wrap LhSkip"
      Rhi{} -> validateLRHist ls
      Rh{} -> validateLRHist ls
    LhSkip ls -> case ls of
      Lh{} -> validateLRHist ls
      LhSkip{} -> validateLRHist ls
      Rhi{} -> Left "LhSkip cannot wrap Rhi"
      Rh{} -> Left "LhSkip cannot wrap Rh"

-- | base case for 'LRHist' Read instance for '[] so only supports 'Rhi' constructor
instance
  (Read a, Read e) =>
  Read (LRHist '[] e a)
  where
  readPrec =
    GR.parens
      ( PC.prec
          10
          ( do
              GR.expectP (TRL.Ident "Rhi")
              a <- PC.step GR.readPrec
              return (Rhi a)
          )
      )

-- | successor case for 'LRHist' Read instance (a' ': as) so supports 'Rh', 'Lh', 'LhSkip' constructors
instance
  ( Read a
  , Read e
  , Read a'
  , Read (LRHist as e a')
  , TP.ApplyConstraints '[Read] as
  ) =>
  Read (LRHist (a' ': as) e a)
  where
  readPrec =
    GR.parens
      ( PC.prec
          10
          ( do
              GR.expectP (TRL.Ident "LhSkip")
              rst <- PC.step GR.readPrec
              return (LhSkip rst)
          )
          PC.+++ PC.prec
            10
            ( do
                GR.expectP (TRL.Ident "Lh")
                e <- PC.step GR.readPrec
                rst <- PC.step GR.readPrec
                return (Lh e rst)
            )
          PC.+++ PC.prec
            10
            ( do
                GR.expectP (TRL.Ident "Rh")
                a <- PC.step GR.readPrec
                rst <- PC.step GR.readPrec
                return (Rh a rst)
            )
      )

instance Bifunctor (LRHist as) where
  bimap f g = \case
    Rhi a -> Rhi (g a)
    Rh a ls -> Rh (g a) (first f ls)
    Lh e ls -> Lh (f e) (first f ls)
    LhSkip ls -> LhSkip (first f ls)

instance Bifoldable (LRHist as) where
  bifoldMap f g = \case
    Rhi a -> g a
    Rh a ls -> g a <> bifoldMap f (const mempty) ls
    Lh e ls -> f e <> bifoldMap f (const mempty) ls
    LhSkip ls -> bifoldMap f (const mempty) ls

instance Bitraversable (LRHist as) where
  bitraverse f g = \case
    Rhi a -> Rhi <$> g a
    Rh a ls -> Rh <$> g a <*> bitraverse f pure ls
    Lh e ls -> Lh <$> f e <*> bitraverse f pure ls
    LhSkip ls -> LhSkip <$> bitraverse f pure ls

-- | uses a boolean predicate to determine success or failure
lhBool ::
  forall e a a' as.
  (a ~ a', Monoid e) =>
  (a' -> Bool) ->
  LRHist as e a' ->
  LRHist (a' ': as) e a
lhBool f w =
  case w of
    Rhi a -> k a
    Rh a _ls -> k a
    Lh{} -> LhSkip w
    LhSkip{} -> LhSkip w
 where
  k a = bool (Lh mempty) (Rh a) (f a) w

-- | uses a maybe function to determine success or failure and also allow change of type "a"
lhMaybe ::
  forall e a a' as.
  Monoid e =>
  (a' -> Maybe a) ->
  LRHist as e a' ->
  LRHist (a' ': as) e a
lhMaybe f w =
  case w of
    Rhi a -> k a
    Rh a _ls -> k a
    Lh{} -> LhSkip w
    LhSkip{} -> LhSkip w
 where
  k a = maybe (Lh mempty) Rh (f a) w

-- | similar to 'lhMaybe' leveraging 'boolMaybe'
lhMaybe' ::
  forall e a a' as.
  Monoid e =>
  (a' -> Bool) ->
  (a' -> a) ->
  LRHist as e a' ->
  LRHist (a' ': as) e a
lhMaybe' p f w =
  case w of
    Rhi a -> k a
    Rh a _ls -> k a
    Lh{} -> LhSkip w
    LhSkip{} -> LhSkip w
 where
  k a = maybe (Lh mempty) Rh (boolMaybe p f a) w

-- | uses an either function to determine success or failure and also allow change of type "a"
lhEither ::
  forall e a a' as.
  (a' -> Either e a) ->
  LRHist as e a' ->
  LRHist (a' ': as) e a
lhEither f w =
  case w of
    Rhi a -> either Lh Rh (f a) w
    Rh a _ls -> either Lh Rh (f a) w
    Lh{} -> LhSkip w
    LhSkip{} -> LhSkip w

-- | similar to 'lhEither' leveraging 'boolEither'
lhEither' ::
  forall e a a' as.
  (a' -> Bool) ->
  (a' -> e) ->
  (a' -> a) ->
  LRHist as e a' ->
  LRHist (a' ': as) e a
lhEither' p l r w =
  case w of
    Rhi a -> k a
    Rh a _ls -> k a
    Lh{} -> LhSkip w
    LhSkip{} -> LhSkip w
 where
  k a = either Lh Rh (boolEither p l r a) w

-- | apply a function to 'LRHist' using 'ApTheseF'
appLR ::
  forall e a a' as x.
  (ApTheseF e a' x a) =>
  (a' -> x) ->
  LRHist as e a' ->
  LRHist (a' ': as) e a
appLR f w =
  case w of
    Rhi a -> k a
    Rh a _ls -> k a
    Lh{} -> LhSkip w
    LhSkip{} -> LhSkip w
 where
  k a =
    let th = apTheseF a (f a)
     in these Lh Rh (const Rh) th w

-- | similar to 'appLR' with state
appLRS ::
  forall e a' x a as z.
  (ApTheseF e a' x a) =>
  (z -> a' -> (z, x)) ->
  z ->
  LRHist as e a' ->
  (z, LRHist (a' ': as) e a)
appLRS f z w =
  case w of
    Rhi a -> k a
    Rh a _ls -> k a
    Lh{} -> (z, LhSkip w)
    LhSkip{} -> (z, LhSkip w)
 where
  k a =
    let (z1, th) = second (apTheseF a) (f z a)
     in (z1, these Lh Rh (const Rh) th w)

-- | apply a function to a 'LRHist' via 'boolEither'
appLRB ::
  forall e a a' as.
  (a' -> Bool) ->
  (a' -> e) ->
  (a' -> a) ->
  LRHist as e a' ->
  LRHist (a' ': as) e a
appLRB p l r = appLR (boolEither p l r)

-- | convenience method to apply 'appLR' to a container of 'LRHist' with state
traverseLRHist ::
  forall e a t a' as z.
  Traversable t =>
  (z -> a' -> (z, Either e a)) ->
  z ->
  t (LRHist as e a') ->
  (z, t (LRHist (a' ': as) e a))
traverseLRHist f = L.mapAccumL (appLRS f)

-- | convenience method to apply 'appLRB' to a container of 'LRHist'
traverseLRHistB ::
  forall e a t a' as.
  Functor t =>
  (a' -> Bool) ->
  (a' -> e) ->
  (a' -> a) ->
  t (LRHist as e a') ->
  t (LRHist (a' ': as) e a)
traverseLRHistB p l r = fmap (appLRB p l r)
