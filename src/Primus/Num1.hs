{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Primus.Num1
Description : similar to 'Num' class but with failure handling
Copyright   : (c) Grant Weyburne, 2022
License     : BSD-3
-}
module Primus.Num1 (
  Num1 (..),
  withOp,
  withOp2,
  withOp3,
  withOp4,
) where

import Control.Applicative
import Control.Monad
import Data.Int
import Data.Kind
import Data.Pos
import Data.Word
import GHC.Natural
import Primus.Enum
import Primus.Error
import Primus.Extra

-- | run a function of one integer against the underlying 'Num1' type
withOp :: Num1 a => (Integer -> Integer) -> a -> Either String a
withOp f a = fromInteger1 a (f (toInteger1 a))

-- | run a function of two integers against the underlying 'Num1' types
withOp2 :: Num1 a => (Integer -> Integer -> Integer) -> a -> a -> Either String a
withOp2 f a b = fromInteger1 a (f (toInteger1 a) (toInteger1 b))

-- | run a function of three integers against the underlying 'Num1' types
withOp3 :: Num1 a => (Integer -> Integer -> Integer -> Integer) -> a -> a -> a -> Either String a
withOp3 f a b c =
  fromInteger1 a (f (toInteger1 a) (toInteger1 b) (toInteger1 c))

-- | run a function of four integers against the underlying 'Num1' types
withOp4 :: Num1 a => (Integer -> Integer -> Integer -> Integer -> Integer) -> a -> a -> a -> a -> Either String a
withOp4 f a b c d =
  fromInteger1 a (f (toInteger1 a) (toInteger1 b) (toInteger1 c) (toInteger1 d))

{- | lifted version of Num class for handling failure
 minimal definition requires 'toInteger1' and 'fromInteger1' unless leveraging default signatures
-}
type Num1 :: Type -> Constraint
class Num1 a where
  -- | required method for converting from "a" to an 'Integer'
  toInteger1 :: a -> Integer
  default toInteger1 :: Enum a => a -> Integer
  toInteger1 = toInteger . fromEnum @a

  -- | required method for trying to convert from an 'Integer' to "a"
  fromInteger1 :: a -> Integer -> Either String a
  default fromInteger1 :: (Bounded a, Enum a) => a -> Integer -> Either String a
  fromInteger1 = const integerToEnumSafe

  (.+)
    , (.-)
    , (.*) ::
      Either String a ->
      Either String a ->
      Either String a
  (.+) = join .@ liftA2 (lmsg "(.+)" .@ withOp2 (+))
  (.-) = join .@ liftA2 (lmsg "(.-)" .@ withOp2 (-))
  (.*) = join .@ liftA2 (lmsg "(.*)" .@ withOp2 (*))
  negate1
    , abs1
    , signum1
    , succ1
    , pred1 ::
      Either String a ->
      Either String a
  negate1 = (=<<) (lmsg "negate1" . withOp negate)
  signum1 = (=<<) (lmsg "signum1" . withOp signum)
  abs1 = (=<<) (lmsg "abs1" . withOp abs)
  succ1 = (=<<) (lmsg "succ1" . withOp succ)
  pred1 = (=<<) (lmsg "pred1" . withOp pred)

infixl 7 .*
infixl 6 .+
infixl 6 .-

instance Num1 Natural where
  fromInteger1 _ i
    | i < 0 = Left $ "Natural: undefined for negative numbers " ++ show i
    | otherwise = Right $ naturalFromInteger i

instance Num1 Pos

instance Num1 Word8
instance Num1 Word16
instance Num1 Word32
instance Num1 Word64

instance Num1 Int

instance Num1 Int8
instance Num1 Int16
instance Num1 Int32
instance Num1 Int64
