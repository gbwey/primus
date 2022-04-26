{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module TestAsMaybe where

import Control.Arrow
import Data.Char
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Semigroup as SG
import Data.These
import Primus.AsMaybe
import Primus.Bool
import Primus.Enum
import Test.Tasty
import Test.Tasty.HUnit

suite :: TestTree
suite =
  testGroup
    "TestAsMaybe"
    [ testCase "iterateT1" $
        iterateT1 (\i -> if i > 0 then Just (i - 1) else Nothing) (5 :: Int)
          @?= (5 :| [4, 3, 2, 1, 0])
    , testCase "iterateT1 simpler" $
        iterateT1 (boolMaybe (> 0) pred) (5 :: Int)
          @?= (5 :| [4, 3, 2, 1, 0])
    , testCase "iterateT1" $
        iterateT1 succSafe LT
          @?= (LT :| [EQ, GT])
    , testCase "iterateT1" $
        iterateT1 succSafe GT
          @?= (GT :| [])
    , testCase "iterateT1" $
        iterateT1 predSafe GT
          @?= (GT :| [EQ, LT])
    , testCase "iterateT1" $
        iterateT1 (\x -> if length x > 5 then Left @String "asdf" else Right (length x : x)) [0 :: Int]
          @?= ([0] :| [[1, 0], [2, 1, 0], [3, 2, 1, 0], [4, 3, 2, 1, 0], [5, 4, 3, 2, 1, 0]])
    , testCase "iterateT1 simpler" $
        iterateT1 (boolEither ((<= 5) . length) (const @String "asdf") (\x -> length x : x)) [0 :: Int]
          @?= ([0] :| [[1, 0], [2, 1, 0], [3, 2, 1, 0], [4, 3, 2, 1, 0], [5, 4, 3, 2, 1, 0]])
    , testCase "iterateT1" $
        iterateT1 (\x -> if length x > 5 then Nothing else Just (length x : x)) [0 :: Int]
          @?= ([0] :| [[1, 0], [2, 1, 0], [3, 2, 1, 0], [4, 3, 2, 1, 0], [5, 4, 3, 2, 1, 0]])
    , testCase "iterateT1" $
        iterateT1 (\x -> if length x > 5 then [] else length x : x) [0 :: Int]
          @?= ([0] :| [[1, 0], [2, 1, 0], [3, 2, 1, 0], [4, 3, 2, 1, 0], [5, 4, 3, 2, 1, 0]])
    , testCase "iterateT1" $
        iterateT1 (\(x, y) -> (if length x > 5 then [] else length x : x, Just y)) ([0 :: Int], 1 :: Int)
          @?= (([0], 1) :| [([1, 0], 1), ([2, 1, 0], 1), ([3, 2, 1, 0], 1), ([4, 3, 2, 1, 0], 1), ([5, 4, 3, 2, 1, 0], 1)])
    , testCase "takeWhileTS" $
        takeWhileTS (\b a -> (b + 1, Just (b, a))) (100 :: Int) "abcdef"
          @?= (106, [(100, 'a'), (101, 'b'), (102, 'c'), (103, 'd'), (104, 'e'), (105, 'f')])
    , testCase "takeWhileT" $
        takeWhileT (\a -> if a > 'f' then Nothing else Just (a, ord a)) ['a' .. 'm']
          @?= [('a', 97), ('b', 98), ('c', 99), ('d', 100), ('e', 101), ('f', 102)]
    , testCase "takeWhileT simpler" $
        takeWhileT (boolMaybe (<= 'f') (id &&& ord)) ['a' .. 'm']
          @?= [('a', 97), ('b', 98), ('c', 99), ('d', 100), ('e', 101), ('f', 102)]
    , testCase "filterT" $
        filterT (\a -> if even a then Nothing else Just (show a)) [1 :: Int .. 10]
          @?= ["1", "3", "5", "7", "9"]
    , testCase "filterT simpler" $
        filterT (boolMaybe odd show) [1 :: Int .. 10]
          @?= ["1", "3", "5", "7", "9"]
    , testCase "filterT" $
        filterT (\a -> replicate (7 - a) 'x') [1 :: Int .. 10]
          @?= ["xxxxxx", "xxxxx", "xxxx", "xxx", "xx", "x"]
    , testCase "takeWhileT" $
        takeWhileT (\a -> if even a then Nothing else Just (a, a + 1)) [1 :: Int .. 10]
          @?= [(1, 2)]
    , testCase "takeWhileT" $
        takeWhileT (\a -> if a == 5 then Nothing else Just (show a)) [1 :: Int .. 10]
          @?= ["1", "2", "3", "4"]
    , testCase "takeWhileT" $
        takeWhileT (\a -> replicate (7 - a) 'x') [1 :: Int .. 10]
          @?= ["xxxxxx", "xxxxx", "xxxx", "xxx", "xx", "x"]
    , testCase "partitionEithersT" $
        partitionEithersT (\a -> if odd a then Just (show a) else Nothing) [1 .. 10 :: Int]
          @?= ([2, 4, 6, 8, 10], ["1", "3", "5", "7", "9"])
    , testCase "partitionEithersT" $
        partitionEithersT (\a -> if odd a then Right (show a) else Left a) [1 .. 10 :: Int]
          @?= ([2, 4, 6, 8, 10], ["1", "3", "5", "7", "9"])
    , testCase "partitionEithersT" $
        partitionEithersT (\a -> replicate (7 - a) 'x') [1 .. 10 :: Int]
          @?= ([7, 8, 9, 10], ["xxxxxx", "xxxxx", "xxxx", "xxx", "xx", "x"])
    , testCase "toTheseT warped" $
        toTheseT
          ( \a ->
              let w = chr (a + 50)
               in if even a
                    then That w
                    else
                      if a > 4
                        then These ("These=" ++ show a) w
                        else This ("This=" ++ show a)
          )
          [1 .. 10 :: Int]
          @?= [ This "This=1"
              , That '4'
              , This "This=3"
              , That '6'
              , These "These=5" '7'
              , That '8'
              , These "These=7" '9'
              , That ':'
              , These "These=9" ';'
              , That '<'
              ]
    , testCase "toTheseT ok" $
        toTheseT
          ( \a ->
              let w = chr (a + 50)
               in if even a
                    then if a > 4 then These ("These=" ++ show a) w else That w
                    else This ("This=" ++ show a)
          )
          [1 .. 10 :: Int]
          @?= [This "This=1", That '4', This "This=3", That '6', This "This=5", These "These=6" '8', This "This=7", These "These=8" ':', This "This=9", These "These=10" '<']
    , testCase "toTheseT boolThese simpler" $ -- not the same as the above as "That" should not be opposite of "These"!!
        toTheseT (boolThese even (> 4) (chr . (+ 50)) show) [1 .. 10]
          @?= [This '3', That "2", This '5', That "4", This '7', These '8' "6", This '9', These ':' "8", This ';', These '<' "10"]
    , testCase "unfoldrT" $
        unfoldrT (splitAt 2) [1 :: Int .. 8]
          @?= [[1, 2], [3, 4], [5, 6], [7, 8]]
    , testCase "unfoldrT" $
        unfoldrT (splitAt 2) [1 :: Int .. 7]
          @?= [[1, 2], [3, 4], [5, 6], [7]]
    , testCase "unfoldrT" $
        unfoldrT (splitAt 2) [1 :: Int]
          @?= [[1]]
    , testCase "unfoldrT" $
        unfoldrT (splitAt 2) ([] :: [Int])
          @?= []
    , testCase "unfoldrT" $
        unfoldrT (first sum . splitAt 3) [1 :: Int .. 10]
          @?= [6, 15, 24, 10]
    , testCase "iterateT1" $
        iterateT1 succSafe LT
          @?= (LT :| [EQ, GT])
    , testCase "iterateT1" $
        iterateT1 succSafe GT
          @?= (GT :| [])
    , testCase "iterateT1" $
        iterateT1 predSafe GT
          @?= (GT :| [EQ, LT])
    , testCase "spanT" $
        spanT (\x -> SG.Arg (if x > 3 then "" else "xx" :: String) (x, x * 1000)) [1 .. 5 :: Int]
          @?= (
                [ SG.Arg "xx" (1, 1000)
                , SG.Arg "xx" (2, 2000)
                , SG.Arg "xx" (3, 3000)
                ]
              , [4, 5]
              )
    , testCase "takeWhileT" $
        takeWhileT (\x -> SG.Arg (if x > 3 then "" else "xx" :: String) (x, x * 1000)) [1 .. 5 :: Int]
          @?= [ SG.Arg "xx" (1, 1000)
              , SG.Arg "xx" (2, 2000)
              , SG.Arg "xx" (3, 3000)
              ]
    , testCase "takeWhileT" $
        takeWhileT (\x -> SG.Arg (if x > 3 then Nothing else Just @String "xx") (x, x * 1000)) [1 .. 5 :: Int]
          @?= [ SG.Arg "xx" (1, 1000)
              , SG.Arg "xx" (2, 2000)
              , SG.Arg "xx" (3, 3000)
              ]
    , testCase "spanT" $
        spanT (\i -> if i < 4 then These "thesedata" (show i) else if i < 8 then That (show i) else This @String "thosedata") [1 :: Int .. 10]
          @?= (["1", "2", "3", "4", "5", "6", "7"], [1, 2, 3, 8, 9, 10])
    , testCase "partitionEithersT" $
        partitionEithersT (\a -> if even a then Left @String "1" else Right (a, True)) [1 :: Int .. 4]
          @?= (
                [ "1"
                , "1"
                ]
              ,
                [
                  ( 1
                  , True
                  )
                ,
                  ( 3
                  , True
                  )
                ]
              )
    , testCase "partitionEithersT" $
        partitionEithersT even [1 :: Int .. 5]
          @?= (
                [ 1
                , 3
                , 5
                ]
              ,
                [ 2
                , 4
                ]
              )
    , testCase "L.span" $
        L.span even [2 :: Int, 4, 6, 8, 9, 11, 13]
          @?= (
                [ 2
                , 4
                , 6
                , 8
                ]
              ,
                [ 9
                , 11
                , 13
                ]
              )
    , testCase "spanT" $
        spanT (\a -> if even a then Just (chr (a + 50)) else Nothing) [2 :: Int, 4, 6, 8, 9, 11, 13]
          @?= ( "468:"
              ,
                [ 9
                , 11
                , 13
                ]
              )
    , testCase "spanT simpler" $
        spanT (boolMaybe even (chr . (+ 50))) [2 :: Int, 4, 6, 8, 9, 11, 13]
          @?= ( "468:"
              ,
                [ 9
                , 11
                , 13
                ]
              )
    , testCase "apThese" $ apThese 'x' (Just @Int 1) @?= That 1
    , testCase "apThese" $ apThese 'x' True @?= That 'x'
    , testCase "apThese" $ apThese 'x' False @?= This 'x'
    , testCase "apThese" $ apThese 'x' (Nothing @Double) @?= This 'x'
    , testCase "partitionEithersT" $
        partitionEithersT (\a -> if even a then Right (chr (a + 50)) else Left (show a ++ "oops")) [1 .. 10]
          @?= (["1oops", "3oops", "5oops", "7oops", "9oops"], "468:<")
    , testCase "toTheseTS" $
        toTheseTS (\z a -> (z + 1, if even a then Right (z, a) else Left (a, z, "oops" :: String))) (100 :: Integer) [2, 4, 6, 7, 8, 9 :: Int]
          @?= ( 106
              ,
                [ That
                    ( 100
                    , 2
                    )
                , That
                    ( 101
                    , 4
                    )
                , That
                    ( 102
                    , 6
                    )
                , This
                    ( 7
                    , 103
                    , "oops"
                    )
                , That
                    ( 104
                    , 8
                    )
                , This
                    ( 9
                    , 105
                    , "oops"
                    )
                ]
              )
    , testCase "partitionEithersT" $
        partitionEithersT (\a -> if even a then Right (chr (50 + a)) else Left (a, "oops" :: String)) [2, 4, 6, 7, 8, 9 :: Int]
          @?= (
                [
                  ( 7
                  , "oops"
                  )
                ,
                  ( 9
                  , "oops"
                  )
                ]
              , "468:"
              )
    , testCase "spanTS" $
        spanTS (\z a -> (z + 1, if even a then Right (z, chr (50 + a)) else Left (z, a, "oops" :: String))) (100 :: Int) [2, 4, 6, 7, 8, 9 :: Int]
          @?= ( 104
              ,
                (
                  [
                    ( 100
                    , '4'
                    )
                  ,
                    ( 101
                    , '6'
                    )
                  ,
                    ( 102
                    , '8'
                    )
                  ]
                ,
                  [ 7
                  , 8
                  , 9
                  ]
                )
              )
    , testCase "L.span" $ L.span even [2, 4, 6, 5, 3, 1, 2, 3 :: Int] @?= ([2, 4, 6], [5, 3, 1, 2, 3])
    , testCase "toMaybe" $ toMaybe (Left @String @Int "asdf", Nothing @()) @?= Nothing
    , testCase "toMaybe" $ toMaybe (Right @Double @String "asdf", "" :: String) @?= Nothing
    , testCase "toMaybe" $ toMaybe (Right @() @Int 11, "abc" :: String) @?= Just (11, "abc")
    , testCase "toMaybe" $ toMaybe (Right @() @Int 11) @?= Just 11
    , testCase "toMaybe" $ toMaybe (Just 'x') @?= Just 'x'
    , testCase "toMaybe" $ toMaybe (Nothing @Char) @?= Nothing
    , testCase "toMaybe" $ toMaybe (SG.Arg @String @Int "" 2) @?= Nothing
    , testCase "toMaybe" $ toMaybe (SG.Arg @String @Int "xyz" 2) @?= Just (SG.Arg "xyz" 2)
    , testCase "toMaybe" $ toMaybe (SG.Arg (This @Int @Bool 123) 'x') @?= Nothing
    , testCase "toMaybe" $ toMaybe (SG.Arg (That @Double @Int 123) 'x') @?= Just (SG.Arg 123 'x')
    , testCase "partitionTheseT" $
        partitionTheseT (boolThese even (>= 5) show (chr . (65 +))) [1 :: Int .. 10]
          @?= (
                [ "1"
                , "3"
                , "5"
                , "7"
                , "9"
                ]
              , "CE"
              ,
                [
                  ( "6"
                  , 'G'
                  )
                ,
                  ( "8"
                  , 'I'
                  )
                ,
                  ( "10"
                  , 'K'
                  )
                ]
              )
    , testCase "toTheseT" $
        toTheseT even [1 :: Int .. 10]
          @?= [This 1, That 2, This 3, That 4, This 5, That 6, This 7, That 8, This 9, That 10]
    , testCase "toTheseT'" $
        toTheseT (\a -> if even a then Just (a, show a) else Nothing) [1 :: Int .. 10]
          @?= [This 1, That (2, "2"), This 3, That (4, "4"), This 5, That (6, "6"), This 7, That (8, "8"), This 9, That (10, "10")]
    , testCase "unfoldrT" $
        unfoldrT (\(a, b) -> let (x, y) = splitAt 1 a; (z, w) = splitAt 2 b in ((x, z), (y, w))) ("abc" :: String, [1 :: Int .. 10])
          @?= [
                ( "a"
                ,
                  [ 1
                  , 2
                  ]
                )
              ,
                ( "b"
                ,
                  [ 3
                  , 4
                  ]
                )
              ,
                ( "c"
                ,
                  [ 5
                  , 6
                  ]
                )
              ]
    , testCase "unfoldrT" $
        unfoldrT (pairsT (splitAt 1) (splitAt 2)) ("asdf", [1 :: Int .. 6])
          @?= [("a", [1, 2]), ("s", [3, 4]), ("d", [5, 6])]
    , testCase "spanT" $
        spanT Just [2 :: Int, 4, 6, 1, 3, 5, 2, 3]
          @?= ([2, 4, 6, 1, 3, 5, 2, 3], [])
    , testCase "spanT" $
        spanT (boolThese even (> 4) show (chr . (+ 65))) [2 :: Int, 4, 6, 1, 3, 5, 2, 3]
          @?= ("CEG", [6, 1, 3, 5, 2, 3])
    , testCase "spanT" $
        spanT (boolThese even (< 10) id show) [2, 4 .. 20 :: Int]
          @?= (["2", "4", "6", "8", "10", "12", "14", "16", "18", "20"], [2, 4, 6, 8])
    ]
