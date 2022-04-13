{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Primus.Lens
Description : minimal lens interfaces
-}
module Primus.Lens (
  Lens,
  Lens',
  lens,
  Iso,
  Iso',
  iso,
  Traversal,
  _Fst,
  _Snd,
) where

import Data.Profunctor

-- | lens type synonym
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

-- | simpler lens type synonym
type Lens' s a = Lens s s a a

-- | create a lens
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt afb s = sbt s <$> afb (sa s)
{-# INLINE lens #-}

-- | isomorphism type synonym
type Iso s t a b = forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t)

-- | simpler isomorphism type synonym
type Iso' s a = Iso s s a a

-- | create an isomoprhism
iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso sa bt = dimap sa (fmap bt)
{-# INLINE iso #-}

-- | traversal type synonym
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

-- | lens for accessing the first value in a tuple
_Fst :: forall a x a'. Lens (a, x) (a', x) a a'
_Fst = lens fst (\(_, x) a' -> (a', x))
{-# INLINE _Fst #-}

-- | lens for accessing the second value in a tuple
_Snd :: forall x b b'. Lens (x, b) (x, b') b b'
_Snd = lens snd (\(x, _) b' -> (x, b'))
{-# INLINE _Snd #-}
