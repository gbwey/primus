{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module TestNum1 where

import Data.Pos
import Data.Word
import GHC.Natural
import Primus.Num1
import Test.Tasty
import Test.Tasty.HUnit

doit :: IO ()
doit = defaultMain suite

suite :: TestTree
suite =
  testGroup
    "TestNum1"
    [ testCase "Num1" $
        fromInteger1 @Natural 0 (-100)
          @?= Left "Natural: undefined for negative numbers -100"
    , testCase "Num1" $ withOp2 @Natural (*) 4 5 @?= Right 20
    , testCase "Num1" $ withOp2 @Natural (*) 9 0 @?= Right 0
    , testCase "Num1" $ withOp2 @Natural (-) 7 5 @?= Right 2
    , testCase "Num1" $ withOp2 @Natural (-) 5 5 @?= Right 0
    , testCase "Num1" $
        withOp2 @Natural (-) 4 5
          @?= Left "Natural: undefined for negative numbers -1"
    , testCase "pred1" $
        pred1 @Natural (pure 0)
          @?= Left "pred1:Natural: undefined for negative numbers -1"
    , testCase "pred1" $
        pred1 @Natural (pure 2)
          @?= Right 1
    , testCase "succ1" $
        succ1 @Natural (pure 1)
          @?= Right 2
    , testCase "withOp" $
        withOp @Natural (subtract 9) 2
          @?= Left "Natural: undefined for negative numbers -7"
    , testCase "withOp" $
        withOp @Natural (subtract 9) 14
          @?= Right 5
    , testCase "fromInteger1" $
        fromInteger1 _10P 0
          @?= Left "integerToEnumSafe:underflow where 0 not in range [1..9223372036854775807]"
    , testCase "withOp2" $ withOp2 (*) _1P _5P @?= Right _5P
    , testCase "withOp2" $ withOp2 (*) _1P _1P @?= Right _1P
    , testCase "withOp2" $ withOp2 (*) _1P _2P @?= Right _2P
    , testCase "withOp2" $ withOp2 ((-) . (+ 1)) _5P _5P @?= Right _1P
    , testCase "withOp2" $ withOp2 ((-) . (+ 1)) _5P _5P @?= Right _1P
    , testCase "withOp2" $ withOp2 (-) _7P _5P @?= Right _2P
    , testCase "withOp2" $
        withOp2 (-) _5P _5P
          @?= Left "integerToEnumSafe:underflow where 0 not in range [1..9223372036854775807]"
    , testCase "withOp2" $
        withOp2 (-) _5P _13P
          @?= Left "integerToEnumSafe:underflow where -8 not in range [1..9223372036854775807]"
    , testCase "withOp2" $
        withOp2 (-) (_P @333) (_P @1234)
          @?= Left "integerToEnumSafe:underflow where -901 not in range [1..9223372036854775807]"
    , testCase "pred1" $
        pred1 (pure _1P)
          @?= Left "pred1:integerToEnumSafe:underflow where 0 not in range [1..9223372036854775807]"
    , testCase "pred1" $
        pred1 (pure _2P)
          @?= Right _1P
    , testCase "succ1" $
        succ1 (pure _1P)
          @?= Right _2P
    , testCase "withOp" $
        withOp (subtract 9) _2P
          @?= Left "integerToEnumSafe:underflow where -7 not in range [1..9223372036854775807]"
    , testCase "withOp" $
        withOp (subtract 9) _14P
          @?= Right _5P
    , testCase "withOp2" $
        withOp2 ((-) . (+ 1)) (100 :: Word8) (113 :: Word8)
          @?= Left "integerToEnumSafe:underflow where -12 not in range [0..255]"
    , testCase "withOp2" $
        fromInteger1 (100 :: Word8) 2000
          @?= Left "integerToEnumSafe:overflow where 2000 not in range [0..255]"
    , testCase "withOp2" $
        fromInteger1 (100 :: Word8) 122
          @?= Right @_ @Word8 122
    ]
