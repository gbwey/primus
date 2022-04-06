{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module TestBool where

import Data.Char
import Data.These
import Primus.Bool
import Test.Tasty
import Test.Tasty.HUnit

doit :: IO ()
doit = defaultMain suite

suite :: TestTree
suite =
  testGroup
    "TestBool"
    [ testCase "boolThese" $
        map (boolThese even (> 5) show (chr . (+ 66))) [1 :: Int .. 10]
          @?= [ This "1"
              , That 'D'
              , This "3"
              , That 'F'
              , This "5"
              , These "6" 'H'
              , This "7"
              , These "8" 'J'
              , This "9"
              , These "10" 'L'
              ]
    , testCase "boolThese'" $
        map (boolThese' even (> 5) show (chr . (+ 66)) (\_ a -> ("both(" ++ show a ++ ")", chr (a + 50)))) [1 :: Int .. 10]
          @?= [ This "1"
              , That 'D'
              , This "3"
              , That 'F'
              , This "5"
              , These "both(6)" '8'
              , This "7"
              , These "both(8)" ':'
              , This "9"
              , These "both(10)" '<'
              ]
    , testCase "boolEither" $
        map (boolEither even show (chr . (+ 66))) [1 :: Int .. 10]
          @?= [ Left "1"
              , Right 'D'
              , Left "3"
              , Right 'F'
              , Left "5"
              , Right 'H'
              , Left "7"
              , Right 'J'
              , Left "9"
              , Right 'L'
              ]
    , testCase "boolM" $
        boolM even ("odd",) ("even",) (3 :: Int) @?= ("odd" :: String, 3)
    , testCase "boolM" $
        boolM even ("odd",) ("even",) (4 :: Int) @?= ("even" :: String, 4)
    , testCase "boolThese" $
        traverse (boolThese (const True) (< 5) show id) [1 :: Int .. 10]
          @?= These "1234" [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    , testCase "boolThese" $
        traverse (boolThese (< 5) (const True) show id) [1 :: Int .. 10]
          @?= This "12345"
    , testCase "boolEither" $
        traverse (boolEither (< 5) show id) [1 :: Int .. 10]
          @?= Left "5"
    , testCase "boolEither" $
        traverse (boolEither (const True) show id) [1 :: Int .. 10]
          @?= Right [1 .. 10]
    , testCase "boolMaybe" $
        traverse (boolMaybe (const True) (chr . (+ 64))) [1 :: Int .. 10]
          @?= Just ['A' .. 'J']
    , testCase "boolThese" $
        map (boolThese even (> 5) show (chr . (+ 65))) [1 :: Int .. 10]
          @?= [This "1", That 'C', This "3", That 'E', This "5", These "6" 'G', This "7", These "8" 'I', This "9", These "10" 'K']
    ]
