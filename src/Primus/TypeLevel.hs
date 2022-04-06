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
  Length,
  Len1T,
  NotEqTC,
  Cons1T,
  Snoc1T,
  Snoc1LT,
  SnocT,
  InitT,
  Init1T,
  Last1T,
  Head1T,
  App1T,
  ApplyConstraints1,
  ApplyConstraint,
  ApplyConstraints,
  UnconsT,
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
type Length :: forall k. [k] -> Nat
type family Length rs where
  Length '[] = 0
  Length (_ ': '[]) = 1
  Length (_ ': _ ': '[]) = 2
  Length (_ ': _ ': _ ': '[]) = 3
  Length (_ ': _ ': _ ': _ ': '[]) = 4
  Length (_ ': _ ': _ ': _ ': _ ': rs) = 5 + Length rs

-- | get the length of a type level nonempty list
type Len1T :: forall k. NonEmpty k -> k
type family Len1T ns where
  Len1T (_ ':| ns) = 1 GN.+ Length ns

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

-- | snoc a nonempty list type to a type
type Snoc1T :: forall k. NonEmpty k -> k -> NonEmpty k
type family Snoc1T as b where
  Snoc1T (a ':| as) b = a ':| SnocT as b

-- | snoc a type list to a type
type SnocT :: forall k. [k] -> k -> [k]
type family SnocT as b where
  SnocT '[] b = '[b]
  SnocT (a ': as) b = a ': SnocT as b

-- | snoc a type list to a type
type Snoc1LT :: forall k. [k] -> k -> NonEmpty k
type family Snoc1LT as b where
  Snoc1LT '[] b = b ':| '[]
  Snoc1LT (a ': as) b = Cons1T a (Snoc1LT as b)

-- | append two nonempty lists at the type level
type App1T :: forall k. NonEmpty k -> NonEmpty k -> NonEmpty k
type family App1T x y where
  App1T (a ':| '[]) y = Cons1T a y
  App1T (a ':| a1 ': as) y = Cons1T a (App1T (a1 ':| as) y)

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

-- | uncons a type level nonempty list
type UnconsT :: forall k. NonEmpty k -> (k, [k])
type family UnconsT ns = result | result -> ns where
  UnconsT (a ':| as) = '(a, as)

-- | unsnoc a type level nonempty list
type UnsnocT :: forall k. NonEmpty k -> ([k], k)
type family UnsnocT ns where
  UnsnocT (a ':| '[]) = '( '[], a)
  UnsnocT (a ':| a1 ': as) = FirstConsT a (UnsnocT (a1 ':| as))

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
  InitT '[] = GL.TypeError ( 'GL.Text "InitT: undefined for 1d")
  InitT (_ ': '[]) = '[]
  InitT (n ': m ': ns) = n ': InitT (m ': ns)

-- | get the init of a nonempty list
type Init1T :: forall a. NonEmpty a -> NonEmpty a
type family Init1T ns where
  Init1T (n ':| ns) = n ':| InitT ns

-- | peel off the bottom-most index in the matrix
type Last1T :: forall k. NonEmpty k -> k
type family Last1T ns where
  Last1T (a ':| '[]) = a
  Last1T (_ ':| (a1 : as)) = Last1T (a1 ':| as)

-- | get the head of a nonempty list
type Head1T :: forall k. NonEmpty k -> k
type family Head1T ns where
  Head1T (a ':| _) = a
