{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module TestTypeLevel where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Pos
import Primus.TypeLevel
import Test.Tasty
import Test.Tasty.HUnit

doit :: IO ()
doit = defaultMain suite

suite :: TestTree
suite =
  testGroup
    "TestTypeLevel"
    [ testCase "LengthT" $
        pnat @(LengthT '[]) @?= 0
    , testCase "LengthT" $
        pnat @(LengthT '[1, 2, 3]) @?= 3
    , testCase "LengthT" $
        pnat @(LengthT '[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) @?= 10
    , testCase "LengthT" $
        pnat @(LengthT '[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]) @?= 11
    , testCase "LengthT" $
        pnat @(LengthT '[1, 2, 3, 4, 5, 6, 7, 8, 9]) @?= 9
    , testCase "LastT" $
        pnat @(LastT '[1, 2, 3]) @?= 3
    , testCase "InitT" $
        fromNSP @(InitT '[1, 2, 3]) @?= _1P :| [_2P]
    , testCase "SnocT" $
        fromNSP @(SnocT '[1, 2, 3] 5) @?= _1P :| [_2P, _3P, _5P]
    , testCase "UnsnocT Fst" $
        fromNSP @(Fst (UnsnocT '[1, 2, 3, 4])) @?= _1P :| [_2P, _3P]
    , testCase "UnsnocT Snd" $
        fromNP @(Snd (UnsnocT '[1, 2, 3, 4])) @?= _4P
    ]
