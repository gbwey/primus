{-# OPTIONS -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestZipNonEmpty where

import Data.Foldable
import qualified Data.List.NonEmpty as N
import qualified Data.Monoid as MM
import Primus.ZipNonEmpty
import Test.QuickCheck
import Test.QuickCheck.Checkers
import "checkers" Test.QuickCheck.Classes
import Test.Tasty
import qualified Test.Tasty.QuickCheck as TQ

argsVerbose :: Args
argsVerbose = stdArgs{maxSuccess = 500, chatty = True}

instance Arbitrary a => Arbitrary (ZipNonEmpty a) where
  -- arbitrary = ((ZipNonEmpty .) . (:|)) <$> arbitrary <*> arbitrary
  arbitrary = ZipNonEmpty . N.fromList <$> listOf1 arbitrary

instance Eq a => EqProp (ZipNonEmpty a) where (=-=) = eq

testLawsZipNonEmpty :: [TestBatch]
testLawsZipNonEmpty =
  [functor z, semigroup (z, Fixed (10 :: Int)), foldable z1] -- , traversable z]
 where
  z = undefined :: ZipNonEmpty (MM.Sum Integer, String, MM.Sum Int)
  z1 = undefined :: ZipNonEmpty (String, Integer, String, Int, Bool)

testLawsZipNonEmptyIO :: IO ()
testLawsZipNonEmptyIO = do
  traverse_ verboseBatch testLawsZipNonEmpty

{-
doit :: IO ()
doit = defaultMain suite

suite :: TestTree
suite =
  testGroup
    "TestZipNonEmpty"
    []
-}
suiteCheckers :: TestTree
suiteCheckers =
  testGroup
    "TestZipNonEmpty Checkers"
    [ adj' False 100 1000 10 $ TQ.testProperties "ZipNonEmpty" (checkersToProps testLawsZipNonEmpty)
    ]

checkersToProps :: [TestBatch] -> [(String, Property)]
checkersToProps = concatMap (\(a, bs) -> map (\(x, y) -> (a ++ " " ++ x, y)) bs)

adj' :: Bool -> Int -> Int -> Int -> TestTree -> TestTree
adj' v sz n ratio =
  adjustOption (const $ TQ.QuickCheckMaxSize sz)
    . adjustOption (max $ TQ.QuickCheckTests n)
    . adjustOption (max $ TQ.QuickCheckMaxRatio ratio)
    . adjustOption (const (TQ.QuickCheckVerbose v))
