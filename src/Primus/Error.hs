{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Primus.Error
Description : error methods
Copyright   : (c) Grant Weyburne, 2022
License     : BSD-3
-}
module Primus.Error (
  -- * force conversion from an Either
  forceRight,
  forceRightP,
  fr,
  frp,

  -- * force conversion from a nonempty list
  fromList1,
  fromList1P,
  ne,
  nep,

  -- * error types
  programmError,
  normalError,
  compileError,

  -- * decorate an error
  lmsg,
) where

import Control.Arrow
import Data.List.NonEmpty (NonEmpty (..))
import GHC.Stack

-- | indicates a programmer error
programmError :: HasCallStack => String -> a
programmError s = withFrozenCallStack $ error $ "programm error:" ++ s

-- | indicates a user error
normalError :: HasCallStack => String -> a
normalError s = withFrozenCallStack $ error s

-- | indicates a compiler error
compileError :: HasCallStack => String -> a
compileError s = withFrozenCallStack $ error $ "should be a compile error (check the constraints):" ++ s

-- | unsafe force an error if 'Left'
forceRight :: HasCallStack => String -> Either String a -> a
forceRight s = \case
  Left e -> withFrozenCallStack $ error $ "forceRight:" ++ s ++ " e=" ++ e
  Right a -> a

-- | unsafe force an error if 'Left'
forceRightP :: HasCallStack => String -> Either String a -> a
forceRightP s = \case
  Left e -> withFrozenCallStack $ error $ "programmer error:" ++ s ++ " e=" ++ e
  Right a -> a

-- | unsafe force an error if 'Left'
fr :: HasCallStack => Either String a -> a
fr = forceRight ""

-- | unsafe force an error if 'Left'
frp :: HasCallStack => Either String a -> a
frp = forceRightP ""

-- | prepend an error message
lmsg :: String -> Either String a -> Either String a
lmsg s =
  left
    ( \e -> case s of
        [] -> e
        _ : _ -> s <> ":" <> e
    )

-- | unsafe conversion from list to a nonempty list
ne :: HasCallStack => [a] -> NonEmpty a
ne =
  \case
    [] -> normalError "ne:list is empty"
    x : xs -> x :| xs

-- | unsafe conversion from list to a nonempty list
nep :: HasCallStack => [a] -> NonEmpty a
nep =
  \case
    [] -> programmError "nep:list is empty"
    x : xs -> x :| xs

-- | unsafe conversion from list to a nonempty list
fromList1 :: HasCallStack => String -> [a] -> NonEmpty a
fromList1 msg =
  \case
    [] -> normalError $ "fromList1:" ++ msg
    x : xs -> x :| xs

-- | unsafe conversion from list to a nonempty list
fromList1P :: HasCallStack => String -> [a] -> NonEmpty a
fromList1P msg =
  \case
    [] -> programmError $ "fromList1P:" ++ msg
    x : xs -> x :| xs
