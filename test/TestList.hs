{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module TestList where

import Data.Char
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as N
import Data.Pos
import Data.These
import Primus.List
import Test.Tasty
import Test.Tasty.HUnit

suite :: TestTree
suite =
  testGroup
    "TestList"
    [ testCase "pairsOf1" $ pairsOf1 @Int [1, 2, 3] @?= ([(1, 2), (2, 3)], Nothing)
    , testCase "pairsOf1" $ pairsOf1 @Int [1, 2] @?= ([(1, 2)], Nothing)
    , testCase "pairsOf1" $ pairsOf1 @Int [1] @?= ([], Just 1)
    , testCase "pairsOf1" $ pairsOf1 @Int [] @?= ([], Nothing)
    , testCase "pairsOf2" $ pairsOf2 @Int [1, 2, 3, 4] @?= ([(1, 2), (3, 4)], Nothing)
    , testCase "pairsOf2" $ pairsOf2 @Int [1, 2, 3] @?= ([(1, 2)], Just 3)
    , testCase "pairsOf2" $ pairsOf2 @Int [1, 2] @?= ([(1, 2)], Nothing)
    , testCase "pairsOf2" $ pairsOf2 @Int [1] @?= ([], Just 1)
    , testCase "pairsOf2" $ pairsOf2 @Int [] @?= ([], Nothing)
    , testCase "chunksOf" $ chunksOf @Int _4P [1 .. 13] @?= [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12], [13]]
    , testCase "chunksOf" $ chunksOf @Int _4P [1, 2, 3, 4, 5] @?= [[1, 2, 3, 4], [5]]
    , testCase "chunksOf" $ chunksOf @Int _4P [1, 2, 3, 4] @?= [[1, 2, 3, 4]]
    , testCase "chunksOf" $ chunksOf @Int _4P [1, 2, 3] @?= [[1, 2, 3]]
    , testCase "chunksOf" $ chunksOf @Int _4P [1, 2] @?= [[1, 2]]
    , testCase "chunksOf" $ chunksOf @Int _4P [1] @?= [[1]]
    , testCase "chunksOf" $ chunksOf @Int _4P [] @?= []
    , testCase "atL" $ atL 0 ['a', 'c', 'e'] @?= Just 'a'
    , testCase "atL" $ atL 1 ['a', 'c', 'e'] @?= Just 'c'
    , testCase "atL" $ atL 2 ['a', 'c', 'e'] @?= Just 'e'
    , testCase "atL" $ atL 3 ['a', 'c', 'e'] @?= Nothing
    , testCase "atL" $ atL 0 ([] :: [Int]) @?= Nothing
    , testCase "updateAtL" $ updateAtL 0 toUpper "ace" @?= Just "Ace"
    , testCase "updateAtL" $ updateAtL 1 toUpper "ace" @?= Just "aCe"
    , testCase "updateAtL" $ updateAtL 2 toUpper "ace" @?= Just "acE"
    , testCase "updateAtL" $ updateAtL 3 toUpper "ace" @?= Nothing
    , testCase "updateAtL" $ updateAtL 0 toUpper "" @?= Nothing
    , testCase "splitAtLGE" $ splitAtLGE @Int 0 [1, 2, 3, 4, 5] @?= Right ([], [1, 2, 3, 4, 5])
    , testCase "splitAtLGE" $ splitAtLGE @Int 1 [1, 2, 3, 4, 5] @?= Right ([1], [2, 3, 4, 5])
    , testCase "splitAtLGE" $ splitAtLGE @Int 5 [1, 2, 3, 4, 5] @?= Right ([1, 2, 3, 4, 5], [])
    , testCase "splitAtLGE" $ splitAtLGE @Int 6 [1, 2, 3, 4, 5] @?= Left "not enough elements: expected 6 found 5"
    , testCase "splitAtLGE" $ splitAtLGE @Int (-44) [1, 2, 3, 4, 5] @?= Left "negative index 44"
    , testCase "splitAtLGE" $ splitAtLGE @Int 0 [] @?= Right ([], [])
    , testCase "splitAtL" $ splitAtL @Int 0 [] @?= ([], SplitLEQ)
    , testCase "splitAtL" $ splitAtL @Int 0 [1] @?= ([], SplitLGT (1 :| []))
    , testCase "splitAtL" $ splitAtL @Int 1 [] @?= ([], SplitLLT 0)
    , testCase "splitAtL" $ splitAtL @Int (-4) [1, 2, 3, 4, 5] @?= ([1, 2, 3, 4, 5], SplitLNeg _4P)
    , testCase "splitAtL" $ splitAtL @Int 0 [1, 2, 3, 4, 5] @?= ([], SplitLGT (1 :| [2, 3, 4, 5]))
    , testCase "splitAtL" $ splitAtL @Int 1 [1, 2, 3, 4, 5] @?= ([1], SplitLGT (2 :| [3, 4, 5]))
    , testCase "splitAtL" $ splitAtL @Int 2 [1, 2, 3, 4, 5] @?= ([1, 2], SplitLGT (3 :| [4, 5]))
    , testCase "splitAtL" $ splitAtL @Int 5 [1, 2, 3, 4, 5] @?= ([1, 2, 3, 4, 5], SplitLEQ)
    , testCase "splitAtL" $ splitAtL @Int 6 [1, 2, 3, 4, 5] @?= ([1, 2, 3, 4, 5], SplitLLT 5)
    , testCase "splitAtL" $ splitAtL @Int (-1) [1, 2, 3, 4, 5] @?= ([1, 2, 3, 4, 5], SplitLNeg _1P)
    , testCase "splitAtL infinite" $
        let (xs, ss) = splitAtL @Int 4 [1 ..]
         in case ss of
              SplitLGT ns -> (xs, N.take 10 ns) @?= ([1 .. 4], [5 .. 14])
              o -> assertFailure $ "expected SplitLGT but found " ++ show o
    , testCase "zipLongest" $ zipLongest [1 :: Int .. 5] "Abc" @?= [These 1 'A', These 2 'b', These 3 'c', This 4, This 5]
    , testCase "zipLongest" $ zipLongest "Abc" [1 :: Int .. 5] @?= [These 'A' 1, These 'b' 2, These 'c' 3, That 4, That 5]
    , testCase "lengthExact" $ lengthExact 2 "abc" @?= Left "GT: too many elements: expected 2"
    , testCase "lengthExact" $ lengthExact 3 "abc" @?= Right "abc"
    , testCase "lengthExact" $ lengthExact 4 "abc" @?= Left "LT: expected 4 found 3"
    , testCase "lengthExact" $ lengthExact 0 "abc" @?= Left "GT: too many elements: expected 0"
    , testCase "lengthExact" $ lengthExact 0 "" @?= Right ""
    , testCase "partitionTheseL" $
        partitionTheseL even (> 5) id (chr . (+ 50)) [1 :: Int .. 10]
          @?= ([1, 3, 5, 7, 9], "46", [(6, '8'), (8, ':'), (10, '<')])
    , testCase "partitionEithersL" $
        partitionEithersL even id (chr . (+ 50)) [1 :: Int .. 10]
          @?= ([1, 3, 5, 7, 9], "468:<")
    , testCase "partitionTheseL" $
        partitionTheseL (> 4) even id id [1 :: Int .. 10]
          @?= ([1, 2, 3, 4], [5, 7, 9], [(6, 6), (8, 8), (10, 10)])
    , testCase "spanMaybe" $
        spanMaybe even show [2, 4, 6, 8, 1, 2, 3, 4 :: Int]
          @?= (["2", "4", "6", "8"], [1, 2, 3, 4])
    , testCase "spanMaybe even simpler" $
        spanMaybe even (chr . (+ 50)) [2 :: Int, 4, 6, 8, 9, 11, 13]
          @?= ( "468:"
              ,
              [ 9
              , 11
              , 13
              ]
              )
    ]
