{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module TestNonEmpty where

import Control.Lens
import Data.Char
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as N
import Data.These
import Data.Pos
import Primus.Bool
import Primus.Enum
import Primus.NonEmpty
import Test.Tasty
import Test.Tasty.HUnit

suite :: TestTree
suite =
  testGroup
    "TestNonEmpty"
    [ testCase "splits1" $ splits1 ('a' :| "bcd") @?= [('a' :| "", 'b' :| "cd"), ('a' :| "b", 'c' :| "d"), ('a' :| "bc", 'd' :| "")]
    , testCase "splits1" $ splits1 ('a' :| "b") @?= [('a' :| "", 'b' :| "")]
    , testCase "splits1" $ splits1 ('a' :| "") @?= []
    , testCase "groupByAdjacent1" $ groupByAdjacent1 @Int (<) (1 :| [2, 5, 7, 10, 11]) @?= (1 :| [2, 5, 7, 10, 11]) :| []
    , testCase "groupByAdjacent1" $ groupByAdjacent1 @Int (<) (1 :| [2, 5, 7, 3, 10, 11]) @?= (1 :| [2, 5, 7]) :| [3 :| [10, 11]]
    , testCase "groupByAdjacent1" $ groupByAdjacent1 @Int (<) ((1 :: Int) :| []) @?= (1 :| []) :| []
    , testCase "groupByAdjacent1" $ groupByAdjacent1 @Int (<) (10 :| [9, 8, 7, 6, 5]) @?= (10 :| []) :| [9 :| [], 8 :| [], 7 :| [], 6 :| [], 5 :| []]
    , testCase "groupByAdjacent1" $ groupByAdjacent1 @Int ((==) . (+ 1)) (1 :| [4, 5, 7, 10, 11]) @?= (1 :| []) :| [4 :| [5], 7 :| [], 10 :| [11]]
    , testCase "groupByAdjacent1" $ groupByAdjacent1 @Int ((==) . (+ 1)) (1 :| [2, 3, 4]) @?= (1 :| [2, 3, 4]) :| []
    , testCase "groupByAdjacent1" $ groupByAdjacent1 @Int ((==) . (+ 1)) (10 :| [9, 8]) @?= (10 :| []) :| [9 :| [], 8 :| []]
    , testCase "groupByAdjacent1" $ groupByAdjacent1 @Int ((==) . (+ 1)) (10 :| [8, 8, 9]) @?= (10 :| []) :| [8 :| [], 8 :| [9]]
    , testCase "groupByAdjacent1" $ groupByAdjacent1 @Int ((==) . (+ 1)) (1 :| [2]) @?= (1 :| [2]) :| []
    , testCase "groupByAdjacent1" $ groupByAdjacent1 @Int ((==) . (+ 1)) (1 :| [3]) @?= (1 :| []) :| [3 :| []]
    , testCase "groupByAdjacent1" $ groupByAdjacent1 @Int ((==) . (+ 1)) (1 :| [0]) @?= (1 :| []) :| [0 :| []]
    , testCase "findDupsBy" $ findDupsBy (even . ord) "abcd" @?= ([(0, 'a') :| [(2, 'c')], (1, 'b') :| [(3, 'd')]], [])
    , testCase "findDupsBy" $ findDupsBy id "abcd" @?= ([], [(0, 'a'), (1, 'b'), (2, 'c'), (3, 'd')])
    , testCase "findDupsBy" $ findDupsBy (even . ord) "aceg" @?= ([(0, 'a') :| [(1, 'c'), (2, 'e'), (3, 'g')]], [])
    , testCase "findDupsBy" $ findDupsBy (even . ord) "acegzyz" @?= ([(0, 'a') :| [(1, 'c'), (2, 'e'), (3, 'g'), (5, 'y')], (4, 'z') :| [(6, 'z')]], [])
    , testCase "findDupsBy" $ findDupsBy id "aaaaabccdc" @?= ([(0, 'a') :| [(1, 'a'), (2, 'a'), (3, 'a'), (4, 'a')], (6, 'c') :| [(7, 'c'), (9, 'c')]], [(5, 'b'), (8, 'd')])
    , testCase "findDupsBy" $ findDupsBy id "aaa" @?= ([(0, 'a') :| [(1, 'a'), (2, 'a')]], [])
    , testCase "findDupsBy" $ findDupsBy id "axaya" @?= ([(0, 'a') :| [(2, 'a'), (4, 'a')]], [(1, 'x'), (3, 'y')])
    , testCase "consNonEmpty" $ (1 :| [2, 3, 4 :: Int]) ^. consNonEmpty @?= (1, [2, 3, 4])
    , testCase "consNonEmpty" $ (1, [2, 3, 4 :: Int]) ^. from consNonEmpty @?= (1 :| [2, 3, 4])
    , testCase "splitAt1'" $ splitAt1' @Int _1P (2 :| [3 .. 6]) @?= (2 :| [], SplitGT (3 :| [4, 5, 6]))
    , testCase "splitAt1'" $ splitAt1' @Int _2P (2 :| [3 .. 6]) @?= (2 :| [3], SplitGT (4 :| [5, 6]))
    , testCase "splitAt1'" $ splitAt1' @Int _3P (2 :| [3 .. 6]) @?= (2 :| [3, 4], SplitGT (5 :| [6]))
    , testCase "splitAt1'" $ splitAt1' @Int _3P (2 :| []) @?= (2 :| [], SplitLT _1P)
    , testCase "splitAt1'" $ splitAt1' @Int _1P (2 :| []) @?= (2 :| [], SplitEQ)
    , testCase "chunksRange1" $ chunksRange1 @Int _1P _2P (1 :| [2 .. 5]) @?= (1 :| []) :| [3 :| [], 5 :| []]
    , testCase "chunksRange1" $ chunksRange1 @Int _1P _2P (1 :| [2 .. 10]) @?= (1 :| []) :| [3 :| [], 5 :| [], 7 :| [], 9 :| []]
    , testCase "chunksRange1" $ chunksRange1 @Int _3P _2P (1 :| [2 .. 10]) @?= (1 :| [2, 3]) :| [3 :| [4, 5], 5 :| [6, 7], 7 :| [8, 9], 9 :| [10]]
    , testCase "chunksRange1" $ chunksRange1 @Int _3P _1P (1 :| [2 .. 10]) @?= (1 :| [2, 3]) :| [2 :| [3, 4], 3 :| [4, 5], 4 :| [5, 6], 5 :| [6, 7], 6 :| [7, 8], 7 :| [8, 9], 8 :| [9, 10], 9 :| [10], 10 :| []]
    , testCase "chunksRange1" $ chunksRange1 @Int _3P _1P (1 :| []) @?= (1 :| []) :| []
    , testCase "chunksRange1" $ chunksRange1 @Int _1P _1P (1 :| []) @?= (1 :| []) :| []
    , testCase "unfoldr1NE" $
        unfoldr1NE (\ns -> ((length ns, N.head ns), N.drop 1 ns)) (N.fromList "abcdef")
          @?= ((6, 'a') :| [(5, 'b'), (4, 'c'), (3, 'd'), (2, 'e'), (1, 'f')])
    , testCase "isEnumAscending" $ isEnumAscending @_ @Int (1 :| [2, 3, 4]) @?= S1Ok
    , testCase "isEnumAscending" $ isEnumAscending @_ @Int (1 :| [2, 3, 4, 3]) @?= S1Fail (5, 3)
    , testCase "isEnumAscending" $ isEnumAscending (LT :| [EQ, GT]) @?= S1Ok
    , testCase "isEnumAscending" $ isEnumAscending (LT :| [EQ, GT, EQ]) @?= S1Short (EQ :| [])
    , testCase "isEnumAscending" $ isEnumAscending @_ @Int (1 :| [2, 3, 4, 98, 9, 3]) @?= S1Fail (5, 98)
    , testCase "splits3" $
        splits3 (1 :| [2 :: Int .. 5])
          @?= (([], 1, [2, 3, 4, 5]) :| [([1], 2, [3, 4, 5]), ([1, 2], 3, [4, 5]), ([1, 2, 3], 4, [5]), ([1, 2, 3, 4], 5, [])])
    , testCase "splits3" $
        splits3 ((1 :: Int) :| [])
          @?= (([], 1, []) :| [])
    , testCase "consNonEmpty" $
        (consNonEmpty # (from consNonEmpty # (1 :| [2 :: Int .. 5])))
          @?= (1 :| [2, 3, 4, 5])
    , testCase "chunkNLen nonempty" $
        chunkNLen (enumTo1 _3P) _4P [1 :: Int .. 12]
          @?= Right ((1 :| [2, 3, 4]) :| [5 :| [6, 7, 8], 9 :| [10, 11, 12]])
    , testCase "chunkNLen nonempty" $
        chunkNLen (enumTo1 _3P) _4P [1 :: Int .. 13]
          @?= Left "chunkN': there is still data remaining at eof"
    , testCase "chunkNLen nonempty" $
        chunkNLen (enumTo1 _3P) _4P [1 :: Int .. 11]
          @?= Left "not enough elements: expected 4 found 3"
    , testCase "chunkNLen list" $
        chunkNLen (replicate 3 ()) _4P [1 :: Int .. 12]
          @?= Right [1 :| [2, 3, 4], 5 :| [6, 7, 8], 9 :| [10, 11, 12]]
    , testCase "chunkNLen list" $
        chunkNLen (replicate 3 ()) _4P [1 :: Int .. 13] @?= Left "chunkN': there is still data remaining at eof"
    , testCase "chunkNLen list" $
        chunkNLen (replicate 3 ()) _4P [1 :: Int .. 11]
          @?= Left "not enough elements: expected 4 found 3"
    , testCase "appendL1" $
        appendL1 [1 :: Int .. 4] (101 :| [102])
          @?= (1 :| [2, 3, 4, 101, 102])
    , testCase "appendR1" $
        appendR1 (101 :| [102]) [1 :: Int .. 4]
          @?= (101 :| [102, 1, 2, 3, 4])
    , testCase "appendL1" $
        appendL1 [] (101 :| [102 :: Int])
          @?= (101 :| [102])
    , testCase "appendR1" $
        appendR1 (101 :| [102 :: Int]) []
          @?= (101 :| [102])
    , testCase "iterateMaybe1 even simpler" $
        iterateMaybe1 (> 0) pred (5 :: Int)
          @?= (5 :| [4, 3, 2, 1, 0])
    , testCase "snoc1" $
        snoc1 [1, 2, 3, 4 :: Int] 999 @?= 1 :| [2, 3, 4, 999]
    , testCase "snoc1" $
        snoc1 ([] :: [Int]) 999 @?= 999 :| []
    , testCase "unsnoc1" $
        unsnoc1 (1 :| [2, 3, 4, 999]) @?= ([1, 2, 3, 4 :: Int], 999)
    , testCase "unsnoc1" $
        unsnoc1 ((999 :: Int) :| []) @?= ([], 999)
    , testCase "take1" $
        take1 _1P ('a' :| []) @?= 'a' :| ""
    , testCase "take1" $
        take1 _2P ('a' :| []) @?= 'a' :| ""
    , testCase "take1" $
        take1 _2P ('a' :| "bc") @?= 'a' :| "b"
    , testCase "take1" $
        take1 _3P ('a' :| "bc") @?= 'a' :| "bc"
    , testCase "splitAts1" $
        splitAts1 _2P ('a' :| "bcde") @?= ('a' :| "b") :| ['c' :| "d", 'e' :| ""]
    , testCase "splitAts1" $
        splitAts1 _2P ('a' :| "") @?= ('a' :| "") :| []
    , testCase "splitAts1" $
        splitAts1 _2P ('a' :| "b") @?= ('a' :| "b") :| []
    , testCase "splitAts1" $
        splitAts1 _1P ('a' :| "b") @?= ('a' :| "") :| ['b' :| ""]
    , testCase "lengthExact1" $
        lengthExact1 _1P ('a' :| "bcdef")
          @?= Left "GT: too many elements: expected 1"
    , testCase "lengthExact1" $
        lengthExact1 _6P ('a' :| "bcdef")
          @?= Right ('a' :| "bcdef")
    , testCase "lengthExact1" $
        lengthExact1 _6P ('a' :| "bcde")
          @?= Left "LT: not enough elements: expected 6 found 5"
    , testCase "updateAt1" $
        updateAt1 _3P (+ 100) (1 :| [2 :: Int .. 5])
          @?= Just (1 :| [2, 103, 4, 5])
    , testCase "updateAt1" $
        updateAt1 _1P (+ 100) (1 :| [2 :: Int .. 5])
          @?= Just (101 :| [2, 3, 4, 5])
    , testCase "updateAt1" $
        updateAt1 _5P (+ 100) (1 :| [2 :: Int .. 5])
          @?= Just (1 :| [2, 3, 4, 105])
    , testCase "updateAt1" $
        updateAt1 _6P (+ 100) (1 :| [2 :: Int .. 5])
          @?= Nothing
    , testCase "updateAt1" $
        updateAt1 _7P (+ 100) (1 :| [2 :: Int .. 5])
          @?= Nothing
    , testCase "spanAdjacent1" $
        spanAdjacent1 (<) (1 :| [2 :: Int, 4, 5, 1, 9, 2, 3])
          @?= (1 :| [2, 4, 5], [1, 9, 2, 3])
    , testCase "spanAdjacent1" $
        spanAdjacent1 (<) (9 :| [8 :: Int, 7, 1])
          @?= (9 :| [], [8, 7, 1])
    , testCase "spanAdjacent1" $
        spanAdjacent1 (<) (9 :| [8 :: Int, 7, 1, 9])
          @?= (9 :| [], [8, 7, 1, 9])
    , testCase "spanAdjacent1" $
        spanAdjacent1 (<) ((9 :: Int) :| [])
          @?= (9 :| [], [])
    , testCase "spanAdjacent1" $
        spanAdjacent1 (<) (9 :| [10 :: Int, 11])
          @?= (9 :| [10, 11], [])
    , testCase "spanAdjacent1" $
        spanAdjacent1 ((==) . (+ 1)) (1 :| [2 :: Int, 3, 4, 5, 6])
          @?= (1 :| [2, 3, 4, 5, 6], [])
    , testCase "spanAdjacent1" $
        spanAdjacent1 ((==) . (+ 1)) (1 :| [2 :: Int, 3, 4, 5, 6, 8])
          @?= (1 :| [2, 3, 4, 5, 6], [8])
    , testCase "breakAdjacent1" $
        breakAdjacent1 (<) (1 :| [2 :: Int, 4, 5, 1, 9, 2, 3])
          @?= (1 :| [], [2, 4, 5, 1, 9, 2, 3])
    , testCase "breakAdjacent1" $
        breakAdjacent1 (<) (9 :| [8 :: Int, 7, 1])
          @?= (9 :| [8, 7, 1], [])
    , testCase "breakAdjacent1" $
        breakAdjacent1 (<) (9 :| [8 :: Int, 7, 1, 9])
          @?= (9 :| [8, 7, 1], [9])
    , testCase "breakAdjacent1" $
        breakAdjacent1 (<) ((9 :: Int) :| [])
          @?= (9 :| [], [])
    , testCase "breakAdjacent1" $
        breakAdjacent1 ((==) . (+ 1)) (1 :| [2 :: Int, 3, 4, 5, 6, 8])
          @?= (1 :| [], [2, 3, 4, 5, 6, 8])
    , testCase "breakAdjacent1" $
        breakAdjacent1 ((==) . (+ 1)) (1 :| [3 :: Int, 5])
          @?= (1 :| [3, 5], [])
    , testCase "breakAdjacent1" $
        breakAdjacent1 ((==) . (+ 1)) (1 :| [3 :: Int, 5, 6])
          @?= (1 :| [3, 5], [6])
    , testCase "breakAdjacent1" $
        splitAt1GE _1P (1 :| [2 :: Int, 3, 4])
          @?= Right (1 :| [], [2, 3, 4])
    , testCase "breakAdjacent1" $
        splitAt1GE _2P (1 :| [2 :: Int, 3, 4])
          @?= Right (1 :| [2], [3, 4])
    , testCase "breakAdjacent1" $
        splitAt1GE _4P (1 :| [2 :: Int, 3, 4])
          @?= Right (1 :| [2, 3, 4], [])
    , testCase "breakAdjacent1" $
        splitAt1GE _5P (1 :| [2 :: Int, 3, 4])
          @?= Left "not enough elements: expected 5 found 4"
    , testCase "breakAdjacent1" $
        splitAt1GE _5P ((1 :: Int) :| [])
          @?= Left "not enough elements: expected 5 found 1"
    , testCase "breakAdjacent1" $
        splitAt1GE _1P ((1 :: Int) :| [])
          @?= Right (1 :| [], [])
    , testCase "zipWithExtras" $
        zipWithExtras (,) ([] :: [Int]) "abcd"
          @?= ([], MLRRight ('a' :| "bcd"))
    , testCase "zipWithExtras" $
        zipWithExtras (,) ("abcd" :: String) ([] :: [Int])
          @?= ([], MLRLeft ('a' :| "bcd"))
    , testCase "zipWithExtras" $
        zipWithExtras (,) ("abcd" :: String) [1, 2, 3, 4 :: Int]
          @?= ([('a', 1), ('b', 2), ('c', 3), ('d', 4)], MLREqual)
    , testCase "zipWithExtras" $
        zipWithExtras (,) ("abcd" :: String) [1, 2, 3 :: Int]
          @?= ([('a', 1), ('b', 2), ('c', 3)], MLRLeft ('d' :| ""))
    , testCase "zipWithExtras" $
        zipWithExtras (,) ("abcd" :: String) [1, 2, 3, 4, 5 :: Int]
          @?= ([('a', 1), ('b', 2), ('c', 3), ('d', 4)], MLRRight (5 :| []))
    , testCase "zipWithExtras" $
        zipWithExtras (,) ([] :: [Int]) ([] :: [()])
          @?= ([], MLREqual)
    , testCase "zipWithExtras1" $
        zipWithExtras1 (,) ((1 :: Int) :| []) ('a' :| "bcd")
          @?= ((1, 'a') :| [], MLRRight ('b' :| "cd"))
    , testCase "zipWithExtras1" $
        zipWithExtras1 (,) ('a' :| "bcd") ((1 :: Int) :| [])
          @?= (('a', 1) :| [], MLRLeft ('b' :| "cd"))
    , testCase "zipWithExtras1" $
        zipWithExtras1 (,) ('a' :| "bcd") (1 :| [2, 3, 4 :: Int])
          @?= (('a', 1) :| [('b', 2), ('c', 3), ('d', 4)], MLREqual)
    , testCase "zipWithExtras1" $
        zipWithExtras1 (,) ('a' :| "bcd") (1 :| [2, 3 :: Int])
          @?= (('a', 1) :| [('b', 2), ('c', 3)], MLRLeft ('d' :| ""))
    , testCase "zipWithExtras1" $
        zipWithExtras1 (,) ('a' :| "bcd") (1 :| [2, 3, 4, 5 :: Int])
          @?= (('a', 1) :| [('b', 2), ('c', 3), ('d', 4)], MLRRight (5 :| []))
    , testCase "zipWithExtras1" $
        zipWithExtras1 (,) ((1 :: Int) :| []) (() :| [])
          @?= ((1, ()) :| [], MLREqual)
    , testCase "unfoldrM1" $
        unfoldrM1 (\s -> Just (take 3 s, boolMaybe (not . null) id (drop 3 s))) [1 :: Int .. 11]
          @?= Just ([1, 2, 3] :| [[4, 5, 6], [7, 8, 9], [10, 11]])
    , testCase "span1" $
        span1 even (2 :| [4 :: Int, 6, 8, 1, 210])
          @?= These (2 :| [4, 6, 8]) (1 :| [210])
    , testCase "span1" $
        span1 even (1 :| [2 :: Int, 4, 6, 8, 1, 210])
          @?= These (2 :| [4, 6, 8]) (1 :| [1, 210])
    , testCase "span1" $
        span1 even (2 :| [4 :: Int, 6, 8])
          @?= This (2 :| [4, 6, 8])
    , testCase "span1" $
        span1 even ((1 :: Int) :| [])
          @?= That (1 :| [])
    , testCase "span1" $
        span1 even (1 :| [3 :: Int, 5])
          @?= That (1 :| [3, 5])
    , testCase "partition1" $
        partition1 even (2 :| [4 :: Int, 6, 8, 1, 210])
          @?= These (1 :| []) (2 :| [4, 6, 8, 210])
    , testCase "partition1" $
        partition1 even (1 :| [2 :: Int, 4, 6, 8, 1, 210])
          @?= These (1 :| [1]) (2 :| [4, 6, 8, 210])
    , testCase "partition1" $
        partition1 even (1 :| [2 :: Int, 3, 4, 5, 6, 7, 8])
          @?= These (1 :| [3, 5, 7]) (2 :| [4, 6, 8])
    , testCase "partition1" $
        partition1 even (2 :| [4 :: Int, 6, 8])
          @?= That (2 :| [4, 6, 8])
    , testCase "partition1" $
        partition1 even ((1 :: Int) :| [])
          @?= This (1 :| [])
    , testCase "partition1" $
        partition1 even (1 :| [3 :: Int, 5])
          @?= This (1 :| [3, 5])
    , testCase "at1" $ at1 _1P ('a' :| "ce") @?= Just 'a'
    , testCase "at1" $ at1 _2P ('a' :| "ce") @?= Just 'c'
    , testCase "at1" $ at1 _3P ('a' :| "ce") @?= Just 'e'
    , testCase "at1" $ at1 _4P ('a' :| "ce") @?= Nothing
    , testCase "at1" $ at1 _1P ('a' :| []) @?= Just 'a'
    , testCase "updateAt1" $ updateAt1 _1P toUpper ('a' :| "ce") @?= Just ('A' :| "ce")
    , testCase "updateAt1" $ updateAt1 _2P toUpper ('a' :| "ce") @?= Just ('a' :| "Ce")
    , testCase "updateAt1" $ updateAt1 _3P toUpper ('a' :| "ce") @?= Just ('a' :| "cE")
    , testCase "updateAt1" $ updateAt1 _4P toUpper ('a' :| "ce") @?= Nothing
    , testCase "updateAt1" $ updateAt1 _1P toUpper ('a' :| []) @?= Just ('A' :| [])
    , testCase "updateAt1" $ updateAt1 _2P toUpper ('a' :| []) @?= Nothing
    , testCase "lengthP" $ lengthP (2 :| [1 :: Int .. 5]) @?= _6P
    , testCase "unitsF" $ unitsF @[] _4P @?= [(), (), (), ()]
    , testCase "unitsF" $ unitsF @NonEmpty _4P @?= (() :| [(), (), ()])
    ]
