module Main where

import System.Environment
import Test.Tasty
import qualified TestAsMaybe
import qualified TestBool
import qualified TestEnum
import qualified TestFold
import qualified TestLRHist
import qualified TestList
import qualified TestNonEmpty
import qualified TestNum1
import qualified TestTypeLevel
import qualified TestZipNonEmpty

main :: IO ()
main = do
  xs <- getArgs
  let x1 = [TestLRHist.suiteCheckers, TestZipNonEmpty.suiteCheckers]
  (os, zs) <- case xs of
    "0" : os -> putStrLn "NORMAL (Explicit)" >> return (os, mempty)
    "1" : os -> putStrLn "VERBOSE" >> return (os, x1)
    "2" : os -> putStrLn "EXTRA VERBOSE" >> return (os, x1)
    os -> putStrLn "NORMAL" >> return (os, [])
  withArgs os $
    defaultMain $
      testGroup
        "alltests"
        ( [ TestAsMaybe.suite
          , TestBool.suite
          , TestEnum.suite
          , TestFold.suite
          , TestList.suite
          , TestLRHist.suite
          , TestNonEmpty.suite
          , TestNum1.suite
          , TestTypeLevel.suite
          --          , TestZipNonEmpty.suite
          ]
            ++ zs
        )
