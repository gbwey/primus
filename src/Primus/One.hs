{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |
Module      : Primus.One
Description : holds a singleton value
Copyright   : (c) Grant Weyburne, 2016
License     : BSD-3

handles a tuple of size one. this is a special type that distinguishes a singleton value from a ntuple
will be replaced by Solo when ghc 9.2 is standard and generics-sop is updated to support Solo
-}
module Primus.One (
  One (..),
  unOne,
) where

import Control.DeepSeq
import Data.Coerce
import Data.Data
import qualified Data.Functor.Apply as Apply
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable
import GHC.Generics (Generic, Generic1)

-- | unwrap 'One'
unOne :: One a -> a
unOne = coerce

-- | One holds a single value. To use wprint we need a SOP Generics instance
newtype One a = One a
  deriving stock (Data, Generic, Generic1, Show, Eq, Ord, Traversable, Read, Functor, Foldable)
  deriving newtype (Semigroup, Monoid, NFData)
  deriving anyclass (NFData1, Foldable1)

instance Applicative One where
  pure = coerce
  (<*>) = coerce
instance Apply.Apply One where
  (<.>) = coerce
instance Monad One where
  return = pure
  One a >>= amb = amb a
instance Traversable1 One where
  traverse1 afb = fmap One . afb . unOne
