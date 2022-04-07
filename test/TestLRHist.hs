{-# OPTIONS -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module TestLRHist where

import Control.Monad
import Data.Char
import Data.Foldable
import Data.Functor
import qualified Data.Monoid as MM
import Data.Proxy
import Data.These
import Primus.LRHist
import qualified Primus.TypeLevel as TP
import Test.QuickCheck
import Test.QuickCheck.Checkers
import "checkers" Test.QuickCheck.Classes
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as TQ

argsVerbose :: Args
argsVerbose = stdArgs{maxSuccess = 500, chatty = True}

doit :: IO ()
doit = defaultMain suite

instance (LRHistArbC as e a, Arbitrary a, Arbitrary e) => Arbitrary (LRHist as e a) where
  arbitrary = lrhistArbC

class LRHistArbC as e a where
  lrhistArbC :: (Arbitrary a, Arbitrary e) => Gen (LRHist as e a)
instance LRHistArbC '[] e a where
  lrhistArbC = Rhi <$> arbitrary
instance (Arbitrary a', LRHistArbC as e a') => LRHistArbC (a' ': as) e a where
  lrhistArbC = do
    x <- lrhistArbC @as @e @a'
    case x of
      LhSkip{} -> pure $ LhSkip x
      Lh{} -> pure $ LhSkip x
      Rhi{} -> oneof [flip Rh x <$> arbitrary, flip Lh x <$> arbitrary]
      Rh{} -> oneof [flip Rh x <$> arbitrary, flip Lh x <$> arbitrary]

instance (TP.ApplyConstraint Eq as, Eq e, Eq a) => EqProp (LRHist as e a) where (=-=) = eq

testLawsLRHist0 :: [TestBatch]
testLawsLRHist0 =
  [functor z, monoid z, semigroup (z, Fixed (10 :: Int)), foldable z1] --  , traversable z] -- 9.2 issues
 where
  z = undefined :: LRHist '[] String (MM.Sum Integer, String, MM.Sum Int)
  z1 = undefined :: LRHist '[] String (String, Integer, String, Int, Bool)

testLawsLRHist1 :: [TestBatch]
testLawsLRHist1 =
  [functor z, monoid z, semigroup (z, Fixed (10 :: Int)), foldable z1] --   , traversable z]
 where
  z = undefined :: LRHist '[MM.Sum Int, String] String (MM.Sum Integer, String, MM.Sum Int)
  z1 = undefined :: LRHist '[MM.Sum Int, String] String (String, Integer, String, Int, Bool)

testLawsLRHistIO :: IO ()
testLawsLRHistIO = do
  traverse_ verboseBatch testLawsLRHist0
  traverse_ verboseBatch testLawsLRHist1

suite :: TestTree
suite =
  testGroup
    "TestLRHist"
    [ testCase "lhToEither" $ lhToEither (appLR Left (appLR Right (Rhi 'x'))) @?= Left @_ @String 'x'
    , testCase "lhToEither" $ lhToEither (appLR Right (appLR Right (appLR (const (Left "xx")) (Rhi 'x')))) @?= Left @String @() "xx"
    , testCase "lhToEither" $ lhToEither (appLR Right (appLR Right (appLR Right (Rhi 'x')))) @?= Right @String 'x'
    , testCase "lhToEither" $ lhToEither (appLR (Right . show) (appLR Right (appLR Right (Rhi 'x')))) @?= Right @String "'x'"
    , testCase "lhToEither" $ lhToEither (appLR Left (Rhi 'x')) @?= Left @_ @String 'x'
    , testCase "lhToEither" $ lhToEither (Rhi 'x') @?= Right @() 'x'
    , testCase "validateLRHist" $ validateLRHist (LhSkip (Rhi ())) @?= Left "LhSkip cannot wrap Rhi"
    , testCase "validateLRHist" $ validateLRHist (Lh () (Rhi ())) @?= Right ()
    , testCase "validateLRHist" $ validateLRHist (Lh () (Rh () (Rhi ()))) @?= Right ()
    , testCase "validateLRHist" $ validateLRHist (Rh () (Lh () (Rhi ()))) @?= Left "Rh cannot wrap Lh"
    , testCase "read" $ readsPrec @(LRHist '[Bool] String Char) 1 (show (Rh 'x' (Rhi @_ @String True))) @?= [(Rh 'x' (Rhi True), "")]
    , testCase "read" $ readsPrec @(LRHist '[Double, Bool] String Char) 1 (show (LhSkip @_ @_ @_ @Double $ Lh @_ @_ @_ @Int "ogre" (Rhi @_ @String True))) @?= [(LhSkip (Lh "ogre" (Rhi @_ @String True)), "")]
    , testCase "read dsl" $ readsPrec @(LRHist '[Double, Bool] String Char) 1 (show (snd (lhskip @Double $ lh @Int "ogre" (rhi @String True)))) @?= [(LhSkip (Lh "ogre" (Rhi @_ @String True)), "")]
    , testCase "read" $ readsPrec @(LRHist '[Bool] String Char) 1 (show (Lh @_ @_ @_ @Int "ogre" (Rhi @_ @String True))) @?= [(Lh "ogre" (Rhi True), "")]
    , testCase "read dsl" $ readsPrec @(LRHist '[Bool] String Char) 1 (show (snd (lh @Int "ogre" (rhi @String True)))) @?= [(Lh "ogre" (Rhi True), "")]
    , testCase "dsl" $ lhskip @() (lh @Int "ogre" (rhi @String True)) @?= (Proxy, LhSkip (Lh "ogre" (Rhi True)))
    , testCase "dsl" $ (lhskip @() $ lh @Int "ogre" $ rh True $ rhi @String True) @?= (Proxy, LhSkip (Lh "ogre" (Rh True (Rhi True))))
    , testCase "dsl" $ (lhskip @() $ lh @Int "ogre" $ rhi @String True) @?= (Proxy, LhSkip (Lh "ogre" (Rhi True)))
    , testCase "dsl" $ snd (lhskip @() (lh @Int "hello" (rhi @String ()))) @?= LhSkip (Lh "hello" (Rhi ()))
    , testCase "dsl" $ snd (lhskip @Double (lhskip @() (lh @Int "hello" (rhi @String ())))) @?= LhSkip (LhSkip (Lh "hello" (Rhi ())))
    , testCase "dsl" $ snd (lhskip @Double (lhskip @Bool (lh @Int "hello" (rh 'x' (rhi @String ()))))) @?= LhSkip (LhSkip (Lh "hello" (Rh 'x' (Rhi ()))))
    , testCase "validateLRHist" $ validateLRHist (snd (lhskip (lhskip (lh @_ @String "xx" (rhi 'x'))))) @?= Right ()
    , testCase "dsl" $ snd (lhskip (lhskip (lh @_ @String "xx" (rhi 'x')))) @?= (LhSkip (LhSkip (Lh "xx" (Rhi 'x'))) :: LRHist '[Double, Bool, Char] String ())
    , testCase "read" $
        let x :: LRHist '[Bool, Char, Int] String ()
            x = LhSkip (LhSkip (Lh "xx" (Rhi 3)))
         in read (show x) @?= x
    , testCase "read" $
        let x :: LRHist '[Bool, Char, Float, Int] String ()
            x = LhSkip (LhSkip (Lh "xx" (Rh 1.2 (Rhi 3))))
         in read (show x) @?= x
    , testCase "read" $
        let x :: LRHist '[Char, Float, Int] String Bool
            x = Rh False (Rh 'x' (Rh 1.2 (Rhi 3)))
         in read (show x) @?= x
    , testCase "read" $
        let x :: LRHist '[Bool, Char, Float, Int] String ()
            x = Lh "yy" (Lh "xx" (Rh 'x' (Rh 1.2 (Rhi 3)))) -- even though fundamentally flawed
         in read (show x) @?= x
    , testCase "read" $ read @(LRHist '[Int] String Bool) (show (snd (lh @() "adf" $ rhi @String @Int 1))) @?= Lh "adf" (Rhi 1)
    , testCase "validateLRHist" $
        let x :: LRHist '[Bool, Char, Float, Int] String ()
            x = Lh "yy" (Lh "xx" (Rh 'x' (Rh 1.2 (Rhi 3))))
         in validateLRHist x @?= Left "Lh cannot wrap Lh"
    , testCase "lhToEither" $
        map lhToEither zz2
          @?= [ Left "ab"
              , Right (This 'H')
              , Left "oops"
              , Right (That 50)
              ]
    , testCase "lhToEitherI" $
        map lhToEitherI zz2
          @?= [ Left "ab"
              , Right (This 'H', (72, (2, ('y', (True, ())))))
              , Left "oops"
              , Right (That 50, (50, (299, ('a', (False, ())))))
              ]
    , testCase "lhToEitherTuples" $
        map lhToEitherTuples zz2
          @?= [ Left "ab"
              , Right (This 'H', 72, 2, 'y', True)
              , Left "oops"
              , Right (That 50, 50, 299, 'a', False)
              ]
    , testCase "lhToEitherTuples appLR" $
        (zz1 <&> lhToEitherTuples . appLR (> 50))
          @?= [ Left "ab"
              , Right (72, 72, 2, 'y', True)
              , Right (71, 71, 299, 'a', False)
              , Left ""
              ]
    , testCase "lhToEitherTuples appLR" $
        (zz1 <&> lhToEitherTuples . appLR (\a -> if a > 50 then Just (chr a) else Nothing))
          @?= [ Left "ab"
              , Right ('H', 72, 2, 'y', True)
              , Right ('G', 71, 299, 'a', False)
              , Left ""
              ]
    , testCase "appLR" $
        map (appLR @String (\a -> if even a then Right (chr (a + 50)) else Left ("oops:" ++ show a)) . Rhi) [1 :: Int .. 6]
          @?= [ Lh "oops:1" (Rhi 1)
              , Rh '4' (Rhi 2)
              , Lh "oops:3" (Rhi 3)
              , Rh '6' (Rhi 4)
              , Lh "oops:5" (Rhi 5)
              , Rh '8' (Rhi 6)
              ]
    , testCase "appLR" $
        map (appLR even . Rhi) [1 :: Int .. 5]
          @?= [ Lh
                  ()
                  (Rhi 1)
              , Rh
                  2
                  (Rhi 2)
              , Lh
                  ()
                  (Rhi 3)
              , Rh
                  4
                  (Rhi 4)
              , Lh
                  ()
                  (Rhi 5)
              ]
    , testCase "appLR" $
        map (appLR (\a -> if a <= 3 then Nothing else Just (chr (a + 50))) . appLR even . Rhi) [1 :: Int .. 6]
          @?= [ LhSkip
                  ( Lh
                      ()
                      (Rhi 1)
                  )
              , Lh
                  ()
                  ( Rh
                      2
                      (Rhi 2)
                  )
              , LhSkip
                  ( Lh
                      ()
                      (Rhi 3)
                  )
              , Rh
                  '6'
                  ( Rh
                      4
                      (Rhi 4)
                  )
              , LhSkip
                  ( Lh
                      ()
                      (Rhi 5)
                  )
              , Rh
                  '8'
                  ( Rh
                      6
                      (Rhi 6)
                  )
              ]
    , testCase "lhMaybe'" $
        map (lhMaybe' (> 3) (chr . (+ 50)) . lhBool even . Rhi) [1 :: Int .. 6]
          @?= [ LhSkip
                  ( Lh
                      ()
                      (Rhi 1)
                  )
              , Lh
                  ()
                  ( Rh
                      2
                      (Rhi 2)
                  )
              , LhSkip
                  ( Lh
                      ()
                      (Rhi 3)
                  )
              , Rh
                  '6'
                  ( Rh
                      4
                      (Rhi 4)
                  )
              , LhSkip
                  ( Lh
                      ()
                      (Rhi 5)
                  )
              , Rh
                  '8'
                  ( Rh
                      6
                      (Rhi 6)
                  )
              ]
    , testCase "traverseLRHist" $
        traverseLRHist (\z a -> (z + 1, if even a then Right (z, a) else Left (a, z))) (100 :: Int) (map Rhi [1 :: Int .. 6])
          @?= ( 106
              ,
              [ Lh
                  ( 1
                  , 100
                  )
                  (Rhi 1)
              , Rh
                  ( 101
                  , 2
                  )
                  (Rhi 2)
              , Lh
                  ( 3
                  , 102
                  )
                  (Rhi 3)
              , Rh
                  ( 103
                  , 4
                  )
                  (Rhi 4)
              , Lh
                  ( 5
                  , 104
                  )
                  (Rhi 5)
              , Rh
                  ( 105
                  , 6
                  )
                  (Rhi 6)
              ]
              )
    , testCase "appLR" $
        appLR (\a -> if even a then Just a else Nothing) (Rhi @Int @() 4)
          @?= Rh 4 (Rhi 4)
    , testCase "appLR" $
        appLR (\a -> if even a then Just (chr (a + 50)) else Nothing) (Rhi @Int @() 4)
          @?= Rh '6' (Rhi 4)
    , testCase "appLR" $
        appLR (\a -> if even a then Just (chr (a + 50)) else Nothing) (Rhi @Int @() 4)
          @?= Rh '6' (Rhi 4)
    , testCase "appLR" $
        appLR even (Rhi @Int @() 4)
          @?= Rh 4 (Rhi 4)
    , testCase "appLR" $
        appLR (\a -> if even a then Right (chr (a + 50)) else Left @String "znork") (Rhi @Int 4)
          @?= Rh '6' (Rhi 4)
    , testCase "appLR" $
        appLR (\a -> if even a then Right (chr (a + 50)) else Left @String "znork") (Rhi @Int 5)
          @?= Lh "znork" (Rhi 5)
    , testCase "appLR" $
        lhEither' even (const @String "znork") (chr . (+ 50)) (Rhi @Int 5)
          @?= Lh "znork" (Rhi 5)
    , testCase "appLRB" $
        appLRB even show (chr . (65 +)) (Rhi @Int 12)
          @?= Rh 'M' (Rhi 12)
    , testCase "appLRB" $
        appLRB even show (chr . (65 +)) (Rhi @Int 11)
          @?= Lh "11" (Rhi 11)
    , testCase "eitherToLH" $ eitherToLH (Left @_ @Int 'x') @?= Lh 'x' (Rhi ())
    , testCase "eitherToLH" $ eitherToLH (Right @Int 'x') @?= Rh 'x' (Rhi ())
    , testCase "lhEither'" $
        lhEither' ((> 4) . length) show (join (,)) (lhMaybe' even show (lhBool even (Rhi @Int 12)))
          @?= Lh "\"12\"" (Rh "12" (Rh 12 (Rhi 12)))
    , testCase "traverseLRHistB" $
        traverseLRHistB (< 'x') show id [snd $ lhskip $ lh "sdf" $ rhi 12, snd $ rh 'a' $ rh @_ @Int 999 $ rhi @_ @Int 123]
          @?= [ LhSkip (LhSkip (Lh "sdf" (Rhi 12)))
              , Rh 'a' (Rh 'a' (Rh 999 (Rhi 123)))
              ]
    ]

zz1 :: [LRHist '[Int, Char, Bool] String Int]
zz1 =
  [ snd $ lhskip $ lh "ab" $ rh 'x' $ rhi False
  , snd $ rh 72 $ rh 2 $ rh 'y' $ rhi True
  , snd $ rh 71 $ rh 299 $ rh 'a' $ rhi False
  , snd $ rh 50 $ rh 299 $ rh 'a' $ rhi False
  ]

zz2 :: [LRHist '[Int, Int, Char, Bool] String (These Char Int)]
zz2 =
  zz1
    <&> appLR
      ( \a ->
          if even a
            then
              if a > 50
                then Right (This (chr a))
                else Right (That a)
            else Left "oops"
      )

suiteCheckers :: TestTree
suiteCheckers =
  testGroup
    "TestLRHist Checkers"
    [ adj' False 100 1000 10 $ TQ.testProperties "LRHist '[]" (checkersToProps testLawsLRHist0)
    , adj' False 100 1000 10 $ TQ.testProperties "LRHist (a ': as)" (checkersToProps testLawsLRHist1)
    ]

checkersToProps :: [TestBatch] -> [(String, Property)]
checkersToProps = concatMap (\(a, bs) -> map (\(x, y) -> (a ++ " " ++ x, y)) bs)

adj' :: Bool -> Int -> Int -> Int -> TestTree -> TestTree
adj' v sz n ratio =
  adjustOption (const $ TQ.QuickCheckMaxSize sz)
    . adjustOption (max $ TQ.QuickCheckTests n)
    . adjustOption (max $ TQ.QuickCheckMaxRatio ratio)
    . adjustOption (const (TQ.QuickCheckVerbose v))
