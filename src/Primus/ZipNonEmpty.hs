{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Primus.ZipNonEmpty
Description : 'Control.Applicative.ZipList' version for 'NonEmpty'
Copyright   : (c) Grant Weyburne, 2022
License     : BSD-3
-}
module Primus.ZipNonEmpty (
  ZipNonEmpty (..),
  _Zip1,
  unZipNonEmpty,
) where

import Control.Applicative
import Control.DeepSeq
import Data.Coerce
import Data.Data
import qualified Data.Functor.Apply as Apply
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as N
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable
import qualified GHC.Exts as GE (IsList (..))
import GHC.Generics (Generic, Generic1)
import Primus.Error (normalError)
import Primus.Lens

-- | unwrap 'ZipNonEmpty'
unZipNonEmpty :: ZipNonEmpty a -> NonEmpty a
unZipNonEmpty = coerce

-- | zippable version of 'NonEmpty'
newtype ZipNonEmpty a = ZipNonEmpty (NonEmpty a)
  deriving stock (Data, Generic, Generic1, Show, Eq, Ord, Traversable, Read)
  deriving (NFData) via (NonEmpty a)
  deriving (NFData1, Foldable, Foldable1, Functor) via NonEmpty

-- checkers hangs
instance Monoid a => Monoid (ZipNonEmpty a) where
  mempty = pure mempty

instance Semigroup a => Semigroup (ZipNonEmpty a) where
  (<>) = liftA2 (<>)

instance Applicative ZipNonEmpty where
  pure = ZipNonEmpty . N.repeat
  liftA2 f (ZipNonEmpty xs) (ZipNonEmpty ys) = ZipNonEmpty (N.zipWith f xs ys)

instance Apply.Apply ZipNonEmpty where
  (<.>) = (<*>)

instance Traversable1 ZipNonEmpty where
  traverse1 afb = fmap ZipNonEmpty . traverse1 afb . unZipNonEmpty

instance GE.IsList (ZipNonEmpty a) where
  type Item (ZipNonEmpty a) = a
  fromList =
    \case
      [] -> normalError "IsList: fromList: need at least one element"
      x : xs -> ZipNonEmpty (x :| xs)
  toList = N.toList . coerce

-- | iso for the zipnonempty constructor
_Zip1 :: Iso (ZipNonEmpty a) (ZipNonEmpty b) (NonEmpty a) (NonEmpty b)
_Zip1 = iso coerce coerce
