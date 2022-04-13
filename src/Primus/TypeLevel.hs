{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : Primus.TypeLevel
Description : commonly used type families
Copyright   : (c) Grant Weyburne, 2022
License     : BSD-3
-}
module Primus.TypeLevel (
  pnat,
  FailUnless,
  Fst,
  Snd,
  Fsts,
  Snds,
  LengthT,
  NotEqTC,
  Cons1T,
  SnocT,
  InitT,
  LastT,
  ApplyConstraints1,
  ApplyConstraint,
  ApplyConstraints,
  UnsnocT,
  FirstConsT,
  ToITupleT,
  FromITupleT,
  ITupleC (..),
  type (++),
  type (:=>),
) where

import Data.Kind
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy
import GHC.TypeLits
import qualified GHC.TypeLits as GL
import qualified GHC.TypeNats as GN
import Primus.One

-- | fail with error message if "b" is 'False
type FailUnless :: Bool -> ErrorMessage -> Constraint
type family FailUnless b err where
  FailUnless 'False err = TypeError ( 'Text "FailUnless: " ':<>: err)
  FailUnless 'True _ = ()

-- | extract an int from a 'Nat'
pnat :: forall n. KnownNat n => Int
pnat = fromEnum (GN.natVal (Proxy @n))

-- | type level boolean implication
type (:=>) :: Bool -> Bool -> Bool
type family x :=> y where
  'False :=> _ = 'True
  'True :=> x = x
  _ :=> 'False = 'True
  _ :=> 'True = 'True
  x :=> x = 'True

-- | "fst" at the typelevel
type Fst :: forall a b. (a, b) -> a
type family Fst tp where
  Fst '(a, _) = a

-- | "snd" at the typelevel
type Snd :: forall a b. (a, b) -> b
type family Snd tp where
  Snd '(_, b) = b

-- | "map fst" at the typelevel
type Fsts :: forall a b. [(a, b)] -> [a]
type family Fsts rs where
  Fsts '[] = '[]
  Fsts ('(a, _) ': rs) = a ': Fsts rs

-- | "map snd" at the typelevel
type Snds :: forall a b. [(a, b)] -> [b]
type family Snds rs where
  Snds '[] = '[]
  Snds ('(_, b) ': rs) = b ': Snds rs

-- | 'length' at the typelevel
type LengthT :: forall k. [k] -> Nat
type family LengthT rs where
  LengthT '[] = 0
  LengthT '[_] = 1
  LengthT '[_, _] = 2
  LengthT '[_, _, _] = 3
  LengthT '[_, _, _, _] = 4
  LengthT (_ ': _ ': _ ': _ ': _ ': rs) = 5 + LengthT rs

-- | ensure that two types are not equal
type NotEqTC :: forall k k1. k -> k1 -> Constraint
type family NotEqTC a b where
  NotEqTC a a = TypeError ( 'Text "NotEqTC: found equal")
  NotEqTC _ _ = ()

-- sometimes you can avoid using Cons1T: check first (expand/inline at the callsite)

-- | cons a type to a nonempty list at the type level
type Cons1T :: forall k. k -> NonEmpty k -> NonEmpty k
type family Cons1T a ys = result | result -> a ys where
  Cons1T a (b ':| bs) = a ':| b ': bs

-- | snoc a type list to a type
type SnocT :: forall k. [k] -> k -> [k]
type family SnocT as b where
  SnocT '[] b = '[b]
  SnocT (a ': as) b = a ': SnocT as b

-- | create a constraint from a type and list of constraints taking a type
type ApplyConstraints1 :: forall k. [k -> Constraint] -> k -> Constraint
type family ApplyConstraints1 xs x where
  ApplyConstraints1 '[] _ = ()
  ApplyConstraints1 (c ': cs) x = (c x, ApplyConstraints1 cs x)

-- | create a constraint from a list of types and a constraint that take a type
type ApplyConstraint :: (k -> Constraint) -> [k] -> Constraint
type family ApplyConstraint c xs where
  ApplyConstraint _ '[] = ()
  ApplyConstraint c (x ': xs) = (c x, ApplyConstraint c xs)

-- | create a constraint from a list of types and list of constraints that take a type
type ApplyConstraints :: [k -> Constraint] -> [k] -> Constraint
type family ApplyConstraints cs xs where
  ApplyConstraints '[] _ = ()
  ApplyConstraints (c ': cs) xs = (ApplyConstraint c xs, ApplyConstraints cs xs)

-- | unsnoc a type level nonempty list
type UnsnocT :: forall k. [k] -> ([k], k)
type family UnsnocT ns where
  UnsnocT '[] = GL.TypeError ( 'GL.Text "UnsnocT: undefined for empty indices")
  UnsnocT '[a] = '( '[], a)
  UnsnocT (a ': a1 ': as) = FirstConsT a (UnsnocT (a1 ': as))

-- | cons a type to the first element in a tuple
type FirstConsT :: forall k k1. k -> ([k], k1) -> ([k], k1)
type family FirstConsT a b = result | result -> a b where
  FirstConsT a '(as, c) = '(a ': as, c)

-- putStrLn $ genITupleAll _10P

-- | convert a flat tuple type to an inductive tuple
type ToITupleT :: Type -> Type
type family ToITupleT x = result | result -> x where
  ToITupleT (One a1) = (a1, ())
  ToITupleT (a1, a2) = (a1, (a2, ()))
  ToITupleT (a1, a2, a3) = (a1, (a2, (a3, ())))
  ToITupleT (a1, a2, a3, a4) = (a1, (a2, (a3, (a4, ()))))
  ToITupleT (a1, a2, a3, a4, a5) = (a1, (a2, (a3, (a4, (a5, ())))))
  ToITupleT (a1, a2, a3, a4, a5, a6) = (a1, (a2, (a3, (a4, (a5, (a6, ()))))))
  ToITupleT (a1, a2, a3, a4, a5, a6, a7) = (a1, (a2, (a3, (a4, (a5, (a6, (a7, ())))))))
  ToITupleT (a1, a2, a3, a4, a5, a6, a7, a8) = (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, ()))))))))
  ToITupleT (a1, a2, a3, a4, a5, a6, a7, a8, a9) = (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, ())))))))))
  ToITupleT (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, ()))))))))))

-- | convert an inductive tuple to a flat tuple type
type FromITupleT :: Type -> Type
type family FromITupleT x = result | result -> x where
  FromITupleT (a1, ()) = One a1
  FromITupleT (a1, (a2, ())) = (a1, a2)
  FromITupleT (a1, (a2, (a3, ()))) = (a1, a2, a3)
  FromITupleT (a1, (a2, (a3, (a4, ())))) = (a1, a2, a3, a4)
  FromITupleT (a1, (a2, (a3, (a4, (a5, ()))))) = (a1, a2, a3, a4, a5)
  FromITupleT (a1, (a2, (a3, (a4, (a5, (a6, ())))))) = (a1, a2, a3, a4, a5, a6)
  FromITupleT (a1, (a2, (a3, (a4, (a5, (a6, (a7, ()))))))) = (a1, a2, a3, a4, a5, a6, a7)
  FromITupleT (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, ())))))))) = (a1, a2, a3, a4, a5, a6, a7, a8)
  FromITupleT (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, ()))))))))) = (a1, a2, a3, a4, a5, a6, a7, a8, a9)
  FromITupleT (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, ())))))))))) = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)

-- | conversions to and from an inductive tuple and a flat tuple
type ITupleC :: Type -> Constraint
class ITupleC x where
  toITupleC :: x -> ToITupleT x
  fromITupleC :: ToITupleT x -> x

instance ITupleC (One a1) where
  toITupleC (One a1) = (a1, ())
  fromITupleC (a1, ()) = One a1
instance ITupleC (a1, a2) where
  toITupleC (a1, a2) = (a1, (a2, ()))
  fromITupleC (a1, (a2, ())) = (a1, a2)
instance ITupleC (a1, a2, a3) where
  toITupleC (a1, a2, a3) = (a1, (a2, (a3, ())))
  fromITupleC (a1, (a2, (a3, ()))) = (a1, a2, a3)
instance ITupleC (a1, a2, a3, a4) where
  toITupleC (a1, a2, a3, a4) = (a1, (a2, (a3, (a4, ()))))
  fromITupleC (a1, (a2, (a3, (a4, ())))) = (a1, a2, a3, a4)
instance ITupleC (a1, a2, a3, a4, a5) where
  toITupleC (a1, a2, a3, a4, a5) = (a1, (a2, (a3, (a4, (a5, ())))))
  fromITupleC (a1, (a2, (a3, (a4, (a5, ()))))) = (a1, a2, a3, a4, a5)
instance ITupleC (a1, a2, a3, a4, a5, a6) where
  toITupleC (a1, a2, a3, a4, a5, a6) = (a1, (a2, (a3, (a4, (a5, (a6, ()))))))
  fromITupleC (a1, (a2, (a3, (a4, (a5, (a6, ())))))) = (a1, a2, a3, a4, a5, a6)
instance ITupleC (a1, a2, a3, a4, a5, a6, a7) where
  toITupleC (a1, a2, a3, a4, a5, a6, a7) = (a1, (a2, (a3, (a4, (a5, (a6, (a7, ())))))))
  fromITupleC (a1, (a2, (a3, (a4, (a5, (a6, (a7, ()))))))) = (a1, a2, a3, a4, a5, a6, a7)
instance ITupleC (a1, a2, a3, a4, a5, a6, a7, a8) where
  toITupleC (a1, a2, a3, a4, a5, a6, a7, a8) = (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, ()))))))))
  fromITupleC (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, ())))))))) = (a1, a2, a3, a4, a5, a6, a7, a8)
instance ITupleC (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
  toITupleC (a1, a2, a3, a4, a5, a6, a7, a8, a9) = (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, ())))))))))
  fromITupleC (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, ()))))))))) = (a1, a2, a3, a4, a5, a6, a7, a8, a9)
instance ITupleC (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) where
  toITupleC (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, ()))))))))))
  fromITupleC (a1, (a2, (a3, (a4, (a5, (a6, (a7, (a8, (a9, (a10, ())))))))))) = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)

-- | append two type level lists
type (++) :: forall a. [a] -> [a] -> [a]
type family (++) xs ys where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': xs ++ ys

infixr 5 ++

-- | get the init of a list
type InitT :: forall a. [a] -> [a]
type family InitT xs where
  InitT '[] = GL.TypeError ( 'GL.Text "InitT: undefined for empty indices")
  InitT '[_] = '[]
  InitT (n ': m ': ns) = n ': InitT (m ': ns)

-- | peel off the bottom-most index in the matrix
type LastT :: forall k. [k] -> k
type family LastT ns where
  LastT '[a] = a
  LastT (_ ': a1 : as) = LastT (a1 ': as)
