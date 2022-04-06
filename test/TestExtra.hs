{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestExtra where

import Primus.Extra
import Test.Tasty
import Test.Tasty.HUnit

suite :: TestTree
suite =
  testGroup
    "TestExtra"
    [ testCase "on1" $ on1 compare length "adsf" [1 :: Int .. 2] @?= GT
    ]
