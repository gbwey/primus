{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module TestFold where

import Data.Bool
import Data.Char
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as N
import Data.Ord
import Data.Pos
import Data.These
import Primus.Fold
import Primus.NonEmpty
import Test.Tasty
import Test.Tasty.HUnit

doit :: IO ()
doit = defaultMain suite

suite :: TestTree
suite =
  testGroup
    "TestFold"
    [ testCase "histMapL" $
        histMapL (\ps ft z a -> ((length ft, length ps) : z, show a)) [(999, 998)] [1 .. 5 :: Int]
          @?= ([(0, 4), (1, 3), (2, 2), (3, 1), (4, 0), (999, 998)], ["1", "2", "3", "4", "5"])
    , testCase "histMapL" $
        histMapL (\_ps ft z a -> (ft : z, show a)) [] [1 .. 5 :: Int]
          @?= ([[], [5], [4, 5], [3, 4, 5], [2, 3, 4, 5]], ["1", "2", "3", "4", "5"])
    , testCase "histMapR" $
        histMapR (\_ps ft z a -> (ft : z, show a)) [] [1 .. 5 :: Int]
          @?= ([[], [1], [2, 1], [3, 2, 1], [4, 3, 2, 1]], ["1", "2", "3", "4", "5"])
    , testCase "histMapL" $
        histMapL (\ps ft z a -> (z - 1, (z, toUpper a, ps, ft))) (100 :: Int) ['a' .. 'f']
          @?= (94, [(100, 'A', "", "bcdef"), (99, 'B', "a", "cdef"), (98, 'C', "ba", "def"), (97, 'D', "cba", "ef"), (96, 'E', "dcba", "f"), (95, 'F', "edcba", "")])
    , testCase "histMapR" $
        histMapR (\ps ft z a -> (z - 1, (z, toUpper a, ps, ft))) (100 :: Int) ['a' .. 'f']
          @?= (94, [(95, 'A', "bcdef", ""), (96, 'B', "cdef", "a"), (97, 'C', "def", "ba"), (98, 'D', "ef", "cba"), (99, 'E', "f", "dcba"), (100, 'F', "", "edcba")])
    , testCase "histMapL'" $ histMapL' (\p f a -> (a, p, f)) ['a' .. 'f'] @?= [('a', "", "bcdef"), ('b', "a", "cdef"), ('c', "ba", "def"), ('d', "cba", "ef"), ('e', "dcba", "f"), ('f', "edcba", "")]
    , testCase "fillTraversable" $ fillTraversable [1 :: Int, 2, 3] ("abcdef" :: String) @?= Right ("def", "abc")
    , testCase "fillTraversableExact" $ fillTraversableExact [1 :: Int, 2, 3] ("abc" :: String) @?= Right "abc"
    , testCase "fillTraversableExact" $ fillTraversableExact [1 :: Int, 2, 3] ("abcd" :: String) @?= Left "fillTraversableExact: too many elements found"
    , testCase "wrapL" $ wrapL reverse ("abcdef" :: String) @?= Right "fedcba"
    , testCase "wrapL" $ wrapL (map succ) (Just (123 :: Int)) @?= Right (Just 124)
    , testCase "wrapL" $ wrapL (map succ) (Nothing :: Maybe Int) @?= Right Nothing
    , testCase "padL" $ padL (replicate1 @Int _10P 1) [9999] @?= Right (1 :| [1, 1, 1, 1, 1, 1, 1, 1, 9999])
    , testCase "padR" $ padR (replicate1 @Int _10P 1) [9999] @?= Right (9999 :| [1, 1, 1, 1, 1, 1, 1, 1, 1])
    , testCase "padR" $ padR (replicate1 @Int _1P 4) [9999] @?= Right (9999 :| [])
    , testCase "padR fail" $ padR (replicate1 @Int _1P 4) [9999, 123] @?= Left "padR: negative fill: would need to truncate the data"
    , testCase "padR" $ padR ("a" :: String) [] @?= Right "a"
    , testCase "padR fail" $ padR ("a" :: String) ("bc" :: String) @?= Left "padR: negative fill: would need to truncate the data"
    , testCase "padL fail" $ padL ("a" :: String) ("bc" :: String) @?= Left "padL: negative fill: would need to truncate the data"
    , testCase "padR" $ padR (replicate 5 'a') ("bc" :: String) @?= Right "bcaaa"
    , testCase "padR" $ padR ("aa" :: String) ("bc" :: String) @?= Right "bc"
    , testCase "padR" $ padR [1 .. 10 :: Int] [101 .. 104] @?= Right [101, 102, 103, 104, 5, 6, 7, 8, 9, 10]
    , testCase "padL" $ padL [1 .. 10 :: Int] [101 .. 104] @?= Right [1, 2, 3, 4, 5, 6, 101, 102, 103, 104]
    , testCase "padR" $ padR [1 .. 4 :: Int] [101 .. 104] @?= Right [101, 102, 103, 104]
    , testCase "padL" $ padL (1 :| [2 .. 4 :: Int]) [101 .. 104] @?= Right (101 :| [102, 103, 104])
    , testCase "padR" $ padR (1 :| [2 .. 10 :: Int]) [101 .. 104] @?= Right (101 :| [102, 103, 104, 5, 6, 7, 8, 9, 10])
    , testCase "padL" $ padL (1 :| [2 .. 10 :: Int]) [101 .. 104] @?= Right (1 :| [2, 3, 4, 5, 6, 101, 102, 103, 104])
    , testCase "padR" $ padR (1 :| [2 .. 4 :: Int]) [101 .. 104] @?= Right (101 :| [102, 103, 104])
    , testCase "padL" $ padL (1 :| [2 .. 4 :: Int]) [101 .. 104] @?= Right (101 :| [102, 103, 104])
    , testCase "zipExtrasT" $ zipExtrasT [1 :: Int .. 5] ['a' .. 'f'] @?= [These 1 'a', These 2 'b', These 3 'c', These 4 'd', These 5 'e', That 'f']
    , testCase "zipExtrasT" $ zipExtrasT [1 :: Int .. 5] ['a' .. 'e'] @?= [These 1 'a', These 2 'b', These 3 'c', These 4 'd', These 5 'e']
    , testCase "zipExtrasT" $ zipExtrasT [1 :: Int .. 4] ['a' .. 'e'] @?= [These 1 'a', These 2 'b', These 3 'c', These 4 'd', That 'e']
    , testCase "zipExtrasT" $ zipExtrasT [1 :: Int .. 4] ([] @()) @?= [This 1, This 2, This 3, This 4]
    , testCase "zipExtrasT" $ zipExtrasT ([] @()) [1 :: Int .. 4] @?= [That 1, That 2, That 3, That 4]
    , testCase "compareLength" $ compareLength [1 :: Int .. 5] ("abc" :: String) @?= CGT
    , testCase "compareLength" $ compareLength [1 :: Int .. 5] ("abcde" :: String) @?= CEQ
    , testCase "compareLength" $ compareLength [1 :: Int .. 5] ("abcdefg" :: String) @?= CLT ('f' :| "g")
    , testCase "compareLength" $ compareLength [1 :: Int .. 5] ("" :: String) @?= CGT
    , testCase "compareLength" $ compareLength ([] :: [Int]) ("" :: String) @?= CEQ
    , testCase "compareLength" $ compareLength (These () 'x') ('a' :| "bc") @?= CLT ('b' :| "c")
    , testCase "compareLength" $ compareLength (These True 'x') ('a' :| "") @?= CEQ
    , testCase "compareLength" $
        compareLength ([] :: [Int]) ([] @Char)
          @?= CEQ
    , testCase "compareLength" $
        compareLength ('a' :| ['b' .. 'g']) [100 :: Int .. 104]
          @?= CGT
    , testCase "compareLength" $
        compareLength ('a' :| ['b' .. 'g']) [100 :: Int .. 105]
          @?= CGT
    , testCase "compareLength" $
        compareLength ('a' :| ['b' .. 'g']) [100 :: Int .. 112]
          @?= CLT (107 :| [108, 109, 110, 111, 112])
    , testCase "compareLength" $
        compareLength ('a' :| ['b' .. 'g']) [100 :: Int .. 108]
          @?= CLT (107 :| [108])
    , testCase "compareLength" $
        compareLength ('a' :| ['b' .. 'g']) [100 :: Int .. 106]
          @?= CEQ
    , testCase "compareLengths" $
        compareLengths @Int @Int [1 .. 5] [[101 .. 110], [201 .. 205], [301 .. 302], [], [1001 .. 1020]]
          @?= [ CLT (106 :| [107, 108, 109, 110])
              , CEQ
              , CGT
              , CGT
              , CLT (1006 :| [1007, 1008, 1009, 1010, 1011, 1012, 1013, 1014, 1015, 1016, 1017, 1018, 1019, 1020])
              ]
    , testCase "compareLengths" $
        compareLengths @Char @Int ['a' .. 'z'] [[101 .. 110], [201 .. 205], [301 .. 302], [], [1001 .. 1020]]
          @?= [CGT,CGT,CGT,CGT,CGT]
    , testCase "compareLengths" $
        compareLengths @Char @Int ['a' .. 'd'] [[101 .. 104], [201 .. 204], [301 .. 302], [], [1001 .. 1010]]
          @?= [ CEQ
              , CEQ
              , CGT
              , CGT
              , CLT (1005 :| [1006,1007,1008,1009,1010])
              ]

    , testCase "compareLengths" $
        compareLengths @Char @Int ['a' .. 'd'] [[101 .. 110], [201 .. 205], [301 .. 302], [], [1001 .. 1010]]
          @?=
             [ CLT (105 :| [106,107,108,109,110])
             , CLT (205 :| [])
             , CGT
             , CGT
             , CLT (1005 :| [1006,1007,1008,1009,1010])
             ]

    , testCase "compareLengths" $
        compareLengths @Int @Int [1 .. 5] [[101 .. 110]]
          @?= [CLT (106 :| [107 .. 110])]
    , testCase "compareLengths" $
        compareLengths @Int @Int @_ @[] [1 .. 5] []
          @?= []
    , testCase "compareLength infinite lhs" $
        compareLength [1 :: Int ..] ['a' .. 'f']
          @?= CGT
    , testCase "compareLength infinite rhs" $
        case compareLength ['a' .. 'f'] [1 :: Int ..] of
          CLT ns -> N.take 10 ns @?= [7 .. 16]
          o -> assertFailure $ "expected CLT but found " ++ show o
    , testCase "zipWithExact" $
        zipWithExact (,) (1 :| [2 :: Int .. 5]) ['a' .. 'e']
          @?= Right ((1, 'a') :| [(2, 'b'), (3, 'c'), (4, 'd'), (5, 'e')])
    , testCase "zipWithExact" $
        zipWithExact (,) (1 :| [2 :: Int .. 5]) ['a' .. 'f']
          @?= Left "zipWithExact: lhs has less data"
    , testCase "zipWithExact" $
        zipWithExact (,) (1 :| [2 :: Int .. 5]) ['a' .. 'd']
          @?= Left "zipWithExact: lhs has more data"
    , testCase "tailsT" $
        tailsT (1 :| [2 :: Int, 3, 4, 5, 6, 7, 8, 9, 10])
          @?= (1 :| [2, 3, 4, 5, 6, 7, 8, 9, 10])
          :| [ 2 :| [3, 4, 5, 6, 7, 8, 9, 10]
             , 3 :| [4, 5, 6, 7, 8, 9, 10]
             , 4 :| [5, 6, 7, 8, 9, 10]
             , 5 :| [6, 7, 8, 9, 10]
             , 6 :| [7, 8, 9, 10]
             , 7 :| [8, 9, 10]
             , 8 :| [9, 10]
             , 9 :| [10]
             , 10 :| []
             ]
    , testCase "initsT" $
        initsT (1 :| [2 :: Int, 3, 4, 5, 6, 7, 8, 9, 10])
          @?= (1 :| [])
          :| [ 1 :| [2]
             , 1 :| [2, 3]
             , 1 :| [2, 3, 4]
             , 1 :| [2, 3, 4, 5]
             , 1 :| [2, 3, 4, 5, 6]
             , 1 :| [2, 3, 4, 5, 6, 7]
             , 1 :| [2, 3, 4, 5, 6, 7, 8]
             , 1 :| [2, 3, 4, 5, 6, 7, 8, 9]
             , 1 :| [2, 3, 4, 5, 6, 7, 8, 9, 10]
             ]
    , testCase "reverseT" $
        reverseT (1 :| [2 :: Int, 3, 4, 5, 6, 7, 8, 9, 10])
          @?= 10 :| [9, 8, 7, 6, 5, 4, 3, 2, 1]
    , testCase "sortByT" $
        sortByT (comparing (\x -> Down (x `mod` 5, x))) [1 :: Int .. 10]
          @?= [9, 4, 8, 3, 7, 2, 6, 1, 10, 5]
    , testCase "pFoldL" $
        pFoldL (\xs ys z a -> (xs, ys, a) : z) [([77], [88], 999)] [1 :: Int .. 5]
          @?= [ ([4, 3, 2, 1], [], 5)
              , ([3, 2, 1], [5], 4)
              , ([2, 1], [4, 5], 3)
              , ([1], [3, 4, 5], 2)
              , ([], [2, 3, 4, 5], 1)
              , ([77], [88], 999)
              ]
    , testCase "pFoldR" $
        pFoldR (\xs ys z a -> (xs, ys, a) : z) [([77], [88], 999)] [1 :: Int .. 5]
          @?= [ ([], [2, 3, 4, 5], 1)
              , ([1], [3, 4, 5], 2)
              , ([2, 1], [4, 5], 3)
              , ([3, 2, 1], [5], 4)
              , ([4, 3, 2, 1], [], 5)
              , ([77], [88], 999)
              ]
    , testCase "pFoldL" $
        pFoldL (\as bs b a -> (as, bs, a) : b) [] [1 :: Int .. 4]
          @?= [([3, 2, 1], [], 4), ([2, 1], [4], 3), ([1], [3, 4], 2), ([], [2, 3, 4], 1)]
    , testCase "pFoldR" $
        pFoldR (\as bs b a -> (as, bs, a) : b) [] [1 :: Int .. 4]
          @?= [([], [2, 3, 4], 1), ([1], [3, 4], 2), ([2, 1], [4], 3), ([3, 2, 1], [], 4)]
    , testCase "unfoldl" $
        unfoldl (\s -> if null s then Nothing else Just (splitAt 2 s)) "abcdef"
          @?= ["ef", "cd", "ab"]
    , testCase "unfoldl" $
        unfoldl (\s -> if null s then Nothing else Just (splitAt 2 s)) ""
          @?= []
    , testCase "unfoldrM" $
        unfoldrM (\s -> Right @() (if null s then Nothing else Just (splitAt 2 s))) ("" :: String)
          @?= Right []
    , testCase "unfoldlM" $
        unfoldlM (\s -> Right @() (if null s then Nothing else Just (splitAt 2 s))) "abcdefg"
          @?= Right ["g", "ef", "cd", "ab"]
    , testCase "unfoldrM" $
        unfoldrM (\s -> Right @() (if null s then Nothing else Just (splitAt 2 s))) "abcdefg"
          @?= Right ["ab", "cd", "ef", "g"]
    , testCase "unfoldrM" $
        unfoldrM (\s -> if length s == 4 then Left ("xx" :: String) else Right (if null s then Nothing else Just (splitAt 2 s))) "abcd"
          @?= Left "xx"
    , testCase "unfoldrM" $
        unfoldrM (\s -> if length s == 3 then Left ("xx" :: String) else Right (if null s then Nothing else Just (splitAt 2 s))) "abcdefg"
          @?= Left "xx"
    , testCase "unfoldlM" $
        unfoldlM (\s -> if null s then [Nothing] else [Just $ splitAt 3 s, Just $ splitAt 5 s]) [1 :: Int .. 10]
          @?= [ [[10], [7, 8, 9], [4, 5, 6], [1, 2, 3]]
              , [[10], [7, 8, 9], [4, 5, 6], [1, 2, 3]]
              , [[7, 8, 9, 10], [4, 5, 6], [1, 2, 3]]
              , [[9, 10], [4, 5, 6, 7, 8], [1, 2, 3]]
              , [[9, 10], [4, 5, 6, 7, 8], [1, 2, 3]]
              , [[9, 10], [6, 7, 8], [1, 2, 3, 4, 5]]
              , [[9, 10], [6, 7, 8], [1, 2, 3, 4, 5]]
              , [[6, 7, 8, 9, 10], [1, 2, 3, 4, 5]]
              ]
    , testCase "unfoldrM" $
        unfoldrM (\s -> if null s then [Nothing] else [Just $ splitAt 3 s, Just $ splitAt 5 s]) [1 :: Int .. 10]
          @?= [ [[1, 2, 3], [4, 5, 6], [7, 8, 9], [10]]
              , [[1, 2, 3], [4, 5, 6], [7, 8, 9], [10]]
              , [[1, 2, 3], [4, 5, 6], [7, 8, 9, 10]]
              , [[1, 2, 3], [4, 5, 6, 7, 8], [9, 10]]
              , [[1, 2, 3], [4, 5, 6, 7, 8], [9, 10]]
              , [[1, 2, 3, 4, 5], [6, 7, 8], [9, 10]]
              , [[1, 2, 3, 4, 5], [6, 7, 8], [9, 10]]
              , [[1, 2, 3, 4, 5], [6, 7, 8, 9, 10]]
              ]
    , testCase "unfoldlM" $
        unfoldlM (\s -> if null s then [Nothing] else map Just [splitAt 2 s, splitAt 3 s]) "abcdef"
          @?= [ ["ef", "cd", "ab"]
              , ["ef", "cd", "ab"]
              , ["f", "cde", "ab"]
              , ["f", "cde", "ab"]
              , ["f", "de", "abc"]
              , ["f", "de", "abc"]
              , ["def", "abc"]
              ]
    , testCase "unfoldrM" $
        unfoldrM (\s -> if null s then [Nothing] else map Just [splitAt 2 s, splitAt 3 s]) "abcdef"
          @?= [ ["ab", "cd", "ef"]
              , ["ab", "cd", "ef"]
              , ["ab", "cde", "f"]
              , ["ab", "cde", "f"]
              , ["abc", "de", "f"]
              , ["abc", "de", "f"]
              , ["abc", "def"]
              ]
    , testCase "unfoldrM1" $
        unfoldrM1 (\s -> let (a, b) = splitAt 3 s; (c, d) = splitAt 5 s in bool [(a, Just b)] [(a, Nothing)] (null b) <> bool [(c, Just d)] [(c, Nothing)] (null d)) "abcdefghi"
          @?= [ "abc" :| ["def", "ghi"]
              , "abc" :| ["def", "ghi"]
              , "abc" :| ["defgh", "i"]
              , "abc" :| ["defgh", "i"]
              , "abcde" :| ["fgh", "i"]
              , "abcde" :| ["fgh", "i"]
              , "abcde" :| ["fghi"]
              ]
    , testCase "unfoldrM" $
        unfoldrM (\s -> if null s then [Nothing] else map Just [splitAt 3 s, splitAt 5 s]) "abcdefghi"
          @?= [ ["abc", "def", "ghi"]
              , ["abc", "def", "ghi"]
              , ["abc", "defgh", "i"]
              , ["abc", "defgh", "i"]
              , ["abcde", "fgh", "i"]
              , ["abcde", "fgh", "i"]
              , ["abcde", "fghi"]
              ]
    ]
