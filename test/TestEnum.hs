{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module TestEnum where

import Control.Arrow
import Control.Monad
import Data.Int
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as N
import Data.Pos
import Primus.AsMaybe
import Primus.Enum
import Primus.NonEmpty
import Test.Tasty
import Test.Tasty.HUnit

suite :: TestTree
suite =
  testGroup
    "TestEnum"
    [ testCase "enumFrom1" $ take1 _5P (enumFrom1 _5P) @?= (_5P :| [_6P, _7P, _8P, _9P])
    , testCase "enumFrom1R" $ enumFrom1R _5P @?= (_5P :| [_4P, _3P, _2P, _1P])
    , testCase "enumFrom1R" $ enumFrom1R _1P @?= (_1P :| [])
    , testCase "enumTo1" $ enumTo1 _5P @?= (_1P :| [_2P, _3P, _4P, _5P])
    , testCase "enumTo1" $ enumTo1 _1P @?= (_1P :| [])
    , testCase "enumTo1" $ enumTo1 EQ @?= (LT :| [EQ])
    , testCase "enumTo1" $ enumTo1 LT @?= (LT :| [])
    , testCase "enumTo1" $ enumTo1 GT @?= (LT :| [EQ, GT])
    , testCase "enumFromThen1" $ take1 _5P (enumFromThen1 _5P _11P) @?= (_5P :| [_11P, _17P, _P @23, _P @29])
    , testCase "enumFromThen1" $ enumFromThen1 _15P _10P @?= (_15P :| [_10P, _5P])
    , testCase "enumFromThen1" $ enumFromThen1 _5P _5P @?= (_5P :| [])
    , testCase "enumFromThen1" $ enumFromThen1 LT LT @?= (LT :| [])
    , testCase "enumFromThen1" $ enumFromThen1 LT GT @?= (LT :| [GT])
    , testCase "enumFromThen1" $ enumFromThen1 GT LT @?= (GT :| [LT])
    , testCase "enumFromThen1" $ enumFromThen1 GT EQ @?= (GT :| [EQ, LT])
    , testCase "enumFromTo1" $ enumFromTo1 _5P _9P @?= (_5P :| [_6P, _7P, _8P, _9P])
    , testCase "enumFromTo1" $ enumFromTo1 _9P _5P @?= (_9P :| [_8P, _7P, _6P, _5P])
    , testCase "enumFromTo1" $ enumFromTo1 _5P _5P @?= (_5P :| [])
    , testCase "enumFromTo1" $ enumFromTo1 LT LT @?= (LT :| [])
    , testCase "enumFromTo1" $ enumFromTo1 LT GT @?= (LT :| [EQ, GT])
    , testCase "enumFromTo1" $ enumFromTo1 GT LT @?= (GT :| [EQ, LT])
    , testCase "enumFromTo1" $ enumFromTo1 GT EQ @?= (GT :| [EQ])
    , testCase "universe1" $ universe1 @?= (LT :| [EQ, GT])
    , testCase "universe1R" $ universe1R @?= (GT :| [EQ, LT])
    , testCase "universe1R" $ universe1R @?= (GT :| [EQ, LT])
    , testCase "enumTo1" $ enumTo1 _4P @?= (_1P :| [_2P, _3P, _4P])
    , testCase "enumFrom1" $ enumFrom1 EQ @?= (EQ :| [GT])
    , testCase "enumFrom1" $ enumFrom1 GT @?= (GT :| [])
    , testCase "enumFrom1R" $ enumFrom1R LT @?= (LT :| [])
    , testCase "enumFrom1R" $ enumFrom1R EQ @?= (EQ :| [LT])
    , testCase "enumFrom1R" $ enumFrom1R GT @?= (GT :| [EQ, LT])
    , testCase "toEnumTraversable" $ toEnumTraversable @Ordering (Just ()) 0 @?= Right (Just LT)
    , testCase "toEnumTraversable" $ toEnumTraversable @Ordering (Just ()) 2 @?= Right (Just GT)
    , testCase "toEnumTraversable" $ toEnumTraversable @Ordering (Just ()) 10 @?= Left "cap=(0,2):padL: negative fill: would need to truncate the data"
    , testCase "toEnumTraversable" $ toEnumTraversable @Ordering (replicate 5 ()) 242 @?= Right [GT, GT, GT, GT, GT]
    , testCase "toEnumTraversable" $ toEnumTraversable @Ordering (replicate 5 ()) 243 @?= Left "cap=(0,242):padL: negative fill: would need to truncate the data"
    , testCase "toEnumTraversable" $ toEnumTraversable @Ordering (replicate 5 ()) 0 @?= Right [LT, LT, LT, LT, LT]
    , testCase "toEnumTraversable" $ toEnumTraversable @Ordering (replicate 5 ()) (-1) @?= Left "calcNextEnum:not defined for negative numbers"
    , testCase "toEnumTraversable" $ toEnumTraversable @Ordering (replicate 5 ()) (-123) @?= Left "calcNextEnum:not defined for negative numbers"
    , testCase "toEnumList" $ toEnumList @Ordering 14 @?= Right [EQ, EQ, GT]
    , testCase "toEnumList" $ toEnumList1 @Ordering 0 @?= Right (LT :| [])
    , testCase "toEnumList" $ toEnumList1 @() 0 @?= Right (() :| [])
    , testCase "enumlist" $ traverse (fromEnumFoldable <=< toEnumList @PosNeg) [-5 .. 5] @?= Right [-5 .. 5]
    , testCase "enumlist" $ map (toEnumList @PosOnly) [-5 .. -1] @?= replicate 5 (Left "calcNextEnum:not defined for negative numbers")
    , testCase "enumlist" $ toEnumList @PosOnly 0 @?= Left "zerolr: not defined at zero"
    , testCase "enumlist" $ toEnumList @PosOnly 1 @?= Right [AA1]
    , testCase "enumlist" $ toEnumList @PosOnly 2 @?= Right [BB1]
    , testCase "enumlist" $ toEnumList @PosOnly 3 @?= Right [CC1]
    , testCase "enumlist" $ left (const ()) (toEnumList @PosOnly 4) @?= Left ()
    , testCase "enumlist" $ toEnumList @PosOnly 5 @?= Right [AA1, AA1]
    , testCase "enumlist" $ toEnumList @PosOnly 6 @?= Right [AA1, BB1]
    , testCase "enumlist" $ toEnumList @PosOnly 7 @?= Right [AA1, CC1]
    , testCase "enumlist" $ left (const ()) (toEnumList @PosOnly 8) @?= Left ()
    , testCase "enumlist" $ toEnumList @PosOnly2 0 @?= Left "zerolr: not defined at zero"
    , testCase "enumlist" $ left (const ()) (toEnumList @PosOnly2 1) @?= Left ()
    , testCase "enumlist" $ toEnumList @PosOnly2 2 @?= Right [AA5]
    , testCase "enumlist" $ toEnumList @PosOnly2 123 @?= Right [CC5, CC5, BB5]
    , testCase "enumlist" $ map (toEnumList @NegOnly) [1 .. 5] @?= replicate 5 (Left "calcNextEnum:not defined for positive numbers")
    , testCase "enumlist" $ toEnumList @NegOnly 0 @?= Left "zerolr: not defined at zero"
    , testCase "enumlist" $ toEnumList @NegOnly (-1) @?= Right [CC4]
    , testCase "enumlist" $ toEnumList @NegOnly (-2) @?= Right [BB4]
    , testCase "enumlist" $ toEnumList @NegOnly (-3) @?= Right [AA4]
    , testCase "enumlist" $ left (const ()) (toEnumList @NegOnly (-4)) @?= Left ()
    , testCase "enumlist" $ toEnumList @NegOnly (-5) @?= Right [CC4, CC4]
    , testCase "enumlist" $ toEnumList @NegOnly (-6) @?= Right [CC4, BB4]
    , testCase "enumlist" $ toEnumList @NegOnly (-7) @?= Right [CC4, AA4]
    , testCase "enumlist" $ left (const ()) (toEnumList @NegOnly (-8)) @?= Left ()
    , testCase "enumlist" $ map (toEnumList @PosNat) [-5 .. -1] @?= replicate 5 (Left "calcNextEnum:not defined for negative numbers")
    , testCase "enumlist" $ toEnumList @PosNat 0 @?= Right []
    , testCase "enumlist" $ toEnumList @PosNat 1 @?= Right [BB2]
    , testCase "enumlist" $ toEnumList @PosNat 2 @?= Right [CC2]
    , testCase "enumlist" $ toEnumList @PosNat 3 @?= Right [DD2]
    , testCase "enumlist" $ toEnumList @PosNat 4 @?= Right [BB2, AA2]
    , testCase "enumlist" $ toEnumList @PosNat 5 @?= Right [BB2, BB2]
    , testCase "enumlist" $ map (toEnumList @NegNat) [1 .. 5] @?= replicate 5 (Left "calcNextEnum:not defined for positive numbers")
    , testCase "enumlist" $ toEnumList @NegNat 0 @?= Right []
    , testCase "enumlist" $ toEnumList @NegNat (-1) @?= Right [CC3]
    , testCase "enumlist" $ toEnumList @NegNat (-2) @?= Right [BB3]
    , testCase "enumlist" $ toEnumList @NegNat (-3) @?= Right [AA3]
    , testCase "enumlist" $ toEnumList @NegNat (-4) @?= Right [CC3, DD3]
    , testCase "enumlist" $ toEnumList @NegNat (-5) @?= Right [CC3, CC3]
    , testCase "enumlist" $ toEnumList @NegNat (-6) @?= Right [CC3, BB3]
    , testCase "enumlist" $ toEnumList @NegNat (-7) @?= Right [CC3, AA3]
    , testCase "enumlist" $ toEnumList @NegNat (-8) @?= Right [BB3, DD3]
    , testCase "enumlist" $ toEnumList 100 @?= Right [True, True, False, False, True, False, False]
    , testCase "enumlist1" $ toEnumList1 100 @?= Right (True :| [True, False, False, True, False, False])
    , testCase "fromenum" $ fromEnumFoldable @PosOnly [] @?= Left "zerolr: not defined at zero"
    , testCase "fromenum" $ fromEnumFoldable @NegNat [] @?= Right 0
    , testCase "fromenum" $ fromEnumFoldable @PosNat [] @?= Right 0
    , testCase "fromenum" $ fromEnumFoldable @PosNeg [] @?= Right 0
    , testCase "universe1" $ universe1 @Ordering @?= (LT :| [EQ, GT])
    , testCase "toEnumList" $ toEnumList @Ordering 0 @?= Right []
    , testCase "toEnumList" $ toEnumList @Ordering 1 @?= Right [EQ]
    , testCase "toEnumList" $ toEnumList @Ordering 2 @?= Right [GT]
    , testCase "toEnumList" $ toEnumList @Ordering 3 @?= Right [EQ, LT]
    , testCase "toEnumList" $ toEnumList @Ordering 10 @?= Right [EQ, LT, EQ]
    , testCase "toEnumList" $ toEnumList @Ordering 200 @?= Right [GT, EQ, EQ, LT, GT]
    , testCase "toEnumList1" $ toEnumList1 @Ordering 200 @?= Right (GT :| [EQ, EQ, LT, GT])
    , testCase "toEnumList1" $ toEnumList1 @Ordering 0 @?= Right (LT :| [])
    , testCase "toEnumList1" $ toEnumList1 @Ordering 1 @?= Right (EQ :| [])
    , testCase "toEnumList1" $ toEnumList1 @Ordering 100 @?= Right (EQ :| [LT, GT, LT, EQ])
    , testCase "toEnumList1" $ toEnumList1 @Ordering 27 @?= Right (EQ :| [LT, LT, LT])
    , testCase "succ pred traversable" $ iterateT1 succTraversable [LT, LT, LT] @?= N.reverse (iterateT1 predTraversable [GT, GT, GT])
    , testCase "succ traversable" $
        iterateT1 succTraversable [LT, LT, LT]
          @?= ( [LT, LT, LT]
                  :| [ [LT, LT, EQ]
                     , [LT, LT, GT]
                     , [LT, EQ, LT]
                     , [LT, EQ, EQ]
                     , [LT, EQ, GT]
                     , [LT, GT, LT]
                     , [LT, GT, EQ]
                     , [LT, GT, GT]
                     , [EQ, LT, LT]
                     , [EQ, LT, EQ]
                     , [EQ, LT, GT]
                     , [EQ, EQ, LT]
                     , [EQ, EQ, EQ]
                     , [EQ, EQ, GT]
                     , [EQ, GT, LT]
                     , [EQ, GT, EQ]
                     , [EQ, GT, GT]
                     , [GT, LT, LT]
                     , [GT, LT, EQ]
                     , [GT, LT, GT]
                     , [GT, EQ, LT]
                     , [GT, EQ, EQ]
                     , [GT, EQ, GT]
                     , [GT, GT, LT]
                     , [GT, GT, EQ]
                     , [GT, GT, GT]
                     ]
              )
    , testCase "predSafe" $
        predSafe LT @?= Nothing
    , testCase "predSafe" $
        predSafe EQ @?= Just LT
    , testCase "predSafe" $
        predSafe GT @?= Just EQ
    , testCase "predSafe" $
        predSafe AA @?= Nothing
    , testCase "predSafe" $
        predSafe BB @?= Just AA
    , testCase "predSafe" $
        predSafe EE @?= Just DD
    , testCase "succSafe" $
        succSafe LT @?= Just EQ
    , testCase "succSafe" $
        succSafe EQ @?= Just GT
    , testCase "succSafe" $
        succSafe GT @?= Nothing
    , testCase "succSafe" $
        succSafe AA @?= Just BB
    , testCase "succSafe" $
        succSafe DD @?= Just EE
    , testCase "succSafe" $
        succSafe EE @?= Nothing
    , testCase "enumFromThen1" $ enumFromThen1 AA AA @?= AA :| []
    , testCase "enumFromThen1" $ enumFromThen1 BB AA @?= BB :| [AA]
    , testCase "enumFromThen1" $ enumFromThen1 BB CC @?= BB :| [CC, DD, EE]
    , testCase "enumFromThen1" $ enumFromThen1 EE CC @?= EE :| [CC, AA]
    , testCase "enumFromTo1" $ enumFromTo1 AA AA @?= AA :| []
    , testCase "enumFromTo1" $ enumFromTo1 BB AA @?= BB :| [AA]
    , testCase "enumFromTo1" $ enumFromTo1 BB EE @?= BB :| [CC, DD, EE]
    , testCase "enumFromTo1" $ enumFromTo1 BB CC @?= BB :| [CC]
    , testCase "enumFromTo1" $ enumFromTo1 EE CC @?= EE :| [DD, CC]
    , testCase "predSafe" $ predSafe _4P @?= Just _3P
    , testCase "universe1" $ universe1 @?= (AA :| [BB, CC, DD, EE])
    , testCase "universe1R" $ universe1R @?= (EE :| [DD, CC, BB, AA])
    , testCase "universe1" $ universe1 @?= (AA3 :| [BB3, CC3, DD3])
    , testCase "universe1R" $ universe1R @?= (DD3 :| [CC3, BB3, AA3])
    , testCase "fromEnumFoldable" $
        fromEnumFoldable (replicate 5 (maxBound @Int8))
          @?= Right 34359738367
    , testCase "fromEnumFoldable" $
        fromEnumFoldable (replicate 5 (minBound @Int8))
          @?= Right (-35723051648)
    , testCase "capacity" $
        capacity @Int8 (replicate 5 ())
          @?= Right (-35723051648, 34359738367)
    , testCase "capacity" $
        capacity @NegOnly []
          @?= Left "capacity:unsupported mx < 0: (-3,-1)"
    , testCase "capacity" $
        capacity @PosOnly ['x']
          @?= Left "capacity:unsupported mn > 0: (1,3)"
    , testCase "universeTraversable" $
        universeTraversable [EQ]
          @?= Right ([LT] :| [[EQ], [GT]])
    , testCase "universeTraversable" $
        universeTraversable [EQ, EQ]
          @?= Right ([LT, LT] :| [[LT, EQ], [LT, GT], [EQ, LT], [EQ, EQ], [EQ, GT], [GT, LT], [GT, EQ], [GT, GT]])
    , testCase "universeTraversable" $
        universeTraversable [EQ, EQ, EQ]
          @?= Right ([LT, LT, LT] :| [[LT, LT, EQ], [LT, LT, GT], [LT, EQ, LT], [LT, EQ, EQ], [LT, EQ, GT], [LT, GT, LT], [LT, GT, EQ], [LT, GT, GT], [EQ, LT, LT], [EQ, LT, EQ], [EQ, LT, GT], [EQ, EQ, LT], [EQ, EQ, EQ], [EQ, EQ, GT], [EQ, GT, LT], [EQ, GT, EQ], [EQ, GT, GT], [GT, LT, LT], [GT, LT, EQ], [GT, LT, GT], [GT, EQ, LT], [GT, EQ, EQ], [GT, EQ, GT], [GT, GT, LT], [GT, GT, EQ], [GT, GT, GT]])
    , testCase "universeTraversable" $
        universeTraversable [(), (), ()]
          @?= Right ([(), (), ()] :| [])
    , testCase "capacity" $
        capacity @PosNeg (replicate 5 ())
          @?= Right (-242, 242)
    , testCase "capacity" $
        capacity @PosNeg (replicate 3 ())
          @?= Right (-26, 26)
    , testCase "universeTraversable" $
        universeTraversable [AA, BB, CC]
          @?= Right ([AA, AA, AA] :| [[AA, AA, BB], [AA, AA, CC], [AA, BB, AA], [AA, BB, BB], [AA, BB, CC], [AA, CC, AA], [AA, CC, BB], [AA, CC, CC], [BB, AA, AA], [BB, AA, BB], [BB, AA, CC], [BB, BB, AA], [BB, BB, BB], [BB, BB, CC], [BB, CC, AA], [BB, CC, BB], [BB, CC, CC], [CC, AA, AA], [CC, AA, BB], [CC, AA, CC], [CC, BB, AA], [CC, BB, BB], [CC, BB, CC], [CC, CC, AA], [CC, CC, BB], [CC, CC, CC], [CC, CC, DD], [CC, CC, EE], [CC, DD, CC], [CC, DD, DD], [CC, DD, EE], [CC, EE, CC], [CC, EE, DD], [CC, EE, EE], [DD, CC, CC], [DD, CC, DD], [DD, CC, EE], [DD, DD, CC], [DD, DD, DD], [DD, DD, EE], [DD, EE, CC], [DD, EE, DD], [DD, EE, EE], [EE, CC, CC], [EE, CC, DD], [EE, CC, EE], [EE, DD, CC], [EE, DD, DD], [EE, DD, EE], [EE, EE, CC], [EE, EE, DD], [EE, EE, EE]])
    , testCase "capacity" $
        capacity @NegNat (replicate 3 ())
          @?= Right (-63, 0)
    , testCase "universeTraversable" $
        universeTraversable [AA3, AA3, AA3]
          @?= Right ([AA3, AA3, AA3] :| [[AA3, AA3, BB3], [AA3, AA3, CC3], [AA3, AA3, DD3], [AA3, BB3, AA3], [AA3, BB3, BB3], [AA3, BB3, CC3], [AA3, BB3, DD3], [AA3, CC3, AA3], [AA3, CC3, BB3], [AA3, CC3, CC3], [AA3, CC3, DD3], [AA3, DD3, AA3], [AA3, DD3, BB3], [AA3, DD3, CC3], [AA3, DD3, DD3], [BB3, AA3, AA3], [BB3, AA3, BB3], [BB3, AA3, CC3], [BB3, AA3, DD3], [BB3, BB3, AA3], [BB3, BB3, BB3], [BB3, BB3, CC3], [BB3, BB3, DD3], [BB3, CC3, AA3], [BB3, CC3, BB3], [BB3, CC3, CC3], [BB3, CC3, DD3], [BB3, DD3, AA3], [BB3, DD3, BB3], [BB3, DD3, CC3], [BB3, DD3, DD3], [CC3, AA3, AA3], [CC3, AA3, BB3], [CC3, AA3, CC3], [CC3, AA3, DD3], [CC3, BB3, AA3], [CC3, BB3, BB3], [CC3, BB3, CC3], [CC3, BB3, DD3], [CC3, CC3, AA3], [CC3, CC3, BB3], [CC3, CC3, CC3], [CC3, CC3, DD3], [CC3, DD3, AA3], [CC3, DD3, BB3], [CC3, DD3, CC3], [CC3, DD3, DD3], [DD3, AA3, AA3], [DD3, AA3, BB3], [DD3, AA3, CC3], [DD3, AA3, DD3], [DD3, BB3, AA3], [DD3, BB3, BB3], [DD3, BB3, CC3], [DD3, BB3, DD3], [DD3, CC3, AA3], [DD3, CC3, BB3], [DD3, CC3, CC3], [DD3, CC3, DD3], [DD3, DD3, AA3], [DD3, DD3, BB3], [DD3, DD3, CC3], [DD3, DD3, DD3]])
    , testCase "fromEnumFoldable1 universeTraversable" $
        let m = universeTraversable [AA3, AA3, AA3]
            z = join $ (traverse . traverse) fromEnumFoldable m
         in z @?= Right (-63 :| [-62 .. 0])
    , testCase "fromEnumFoldable1 universeTraversable" $
        let m = universeTraversable [AA, AA, AA]
            z = join $ (traverse . traverse) fromEnumFoldable m
         in z @?= Right (-26 :| [-25 .. 26])
    , testCase "universeTraversable" $
        universeTraversable ([] :: [Ordering])
          @?= Right ([] :| [])
    , testCase "capacity" $
        capacity @Ordering []
          @?= Right (0, 0)
    , testCase "integerToEnumSafe" $
        integerToEnumSafe @() 1
          @?= Left "integerToEnumSafe:overflow where 1 not in range [0..0]"
    , testCase "integerToEnumSafe" $
        integerToEnumSafe @() 0
          @?= Right ()
    , testCase "integerToEnumSafe" $
        integerToEnumSafe @PosNeg 3
          @?= Left "integerToEnumSafe:overflow where 3 not in range [-2..2]"
    , testCase "integerToEnumSafe" $
        integerToEnumSafe @PosNeg (-3)
          @?= Left "integerToEnumSafe:underflow where -3 not in range [-2..2]"
    , testCase "integerToEnumSafe" $
        integerToEnumSafe @PosNeg (-2)
          @?= Right AA
    , testCase "integerToIntSafe" $
        integerToIntSafe (fromIntegral (minBound :: Int))
          @?= Right (-9223372036854775808)
    , testCase "integerToIntSafe" $
        integerToIntSafe (-1 + fromIntegral (minBound :: Int))
          @?= Left "integerToEnumSafe:underflow where -9223372036854775809 not in range [-9223372036854775808..9223372036854775807]"
    , testCase "integerToIntSafe" $
        integerToIntSafe (fromIntegral (maxBound :: Int))
          @?= Right 9223372036854775807
    , testCase "integerToIntSafe" $
        integerToIntSafe (1 + fromIntegral (maxBound :: Int))
          @?= Left "integerToEnumSafe:overflow where 9223372036854775808 not in range [-9223372036854775808..9223372036854775807]"
    , testCase "universe1" $
        universe1 @() @?= () :| []
    , testCase "universe1R" $
        universe1R @() @?= () :| []
    , testCase "enumFrom1" $
        enumFrom1 @() () @?= () :| []
    , testCase "enumFrom1R" $
        enumFrom1R @() () @?= () :| []
    , testCase "enumFromTo1" $
        enumFromTo1 @() () () @?= () :| []
    , testCase "enumTo1" $
        enumTo1 @() () @?= () :| []
    , testCase "enumFromThen1" $
        enumFromThen1 @() () () @?= () :| []
    , testCase "enumFromThenTo1" $
        enumFromThenTo1 @() () () () @?= () :| []
    , testCase "enumFrom1" $
        enumFrom1 @PosNeg DD @?= DD :| [EE]
    , testCase "enumFrom1R" $
        enumFrom1R @PosNeg DD @?= DD :| [CC, BB, AA]
    , testCase "enumFromTo1" $
        enumFromTo1 @PosNeg DD BB @?= DD :| [CC, BB]
    , testCase "enumTo1" $
        enumTo1 @PosNeg AA @?= AA :| []
    , testCase "enumFromThen1" $
        enumFromThen1 @PosNeg AA CC @?= AA :| [CC, EE]
    , testCase "enumFromThenTo1" $
        enumFromThenTo1 @PosNeg AA AA BB @?= AA :| []
    , testCase "enumFromThenTo1" $
        enumFromThenTo1 @PosNeg EE DD DD @?= EE :| [DD]
    , testCase "enumFromThenTo1" $
        enumFromThenTo1 @PosNeg EE DD BB @?= EE :| [DD, CC, BB]
    ]

data PosNeg = AA | BB | CC | DD | EE deriving stock (Bounded, Show, Eq)
instance Enum PosNeg where
  toEnum (-2) = AA
  toEnum (-1) = BB
  toEnum 0 = CC
  toEnum 1 = DD
  toEnum 2 = EE
  toEnum _ = error "bad toEnum PosNeg"

  fromEnum AA = -2
  fromEnum BB = -1
  fromEnum CC = 0
  fromEnum DD = 1
  fromEnum EE = 2

data PosNat = AA2 | BB2 | CC2 | DD2 deriving stock (Bounded, Show, Eq)
instance Enum PosNat where
  toEnum 0 = AA2
  toEnum 1 = BB2
  toEnum 2 = CC2
  toEnum 3 = DD2
  toEnum _ = error "bad toEnum PosNat"

  fromEnum AA2 = 0
  fromEnum BB2 = 1
  fromEnum CC2 = 2
  fromEnum DD2 = 3

data NegNat = AA3 | BB3 | CC3 | DD3 deriving stock (Bounded, Show, Eq)
instance Enum NegNat where
  toEnum (-3) = AA3
  toEnum (-2) = BB3
  toEnum (-1) = CC3
  toEnum 0 = DD3
  toEnum _ = error "bad toEnum NegNat"

  fromEnum AA3 = -3
  fromEnum BB3 = -2
  fromEnum CC3 = -1
  fromEnum DD3 = 0

data PosOnly = AA1 | BB1 | CC1 deriving stock (Bounded, Show, Eq)
instance Enum PosOnly where
  toEnum 1 = AA1
  toEnum 2 = BB1
  toEnum 3 = CC1
  toEnum _ = error "bad toEnum PosOnly"

  fromEnum AA1 = 1
  fromEnum BB1 = 2
  fromEnum CC1 = 3

data PosOnly2 = AA5 | BB5 | CC5 deriving stock (Bounded, Show, Eq)
instance Enum PosOnly2 where
  toEnum 2 = AA5
  toEnum 3 = BB5
  toEnum 4 = CC5
  toEnum _ = error "bad toEnum PosOnly2"

  fromEnum AA5 = 2
  fromEnum BB5 = 3
  fromEnum CC5 = 4

data NegOnly = AA4 | BB4 | CC4 deriving stock (Bounded, Show, Eq)
instance Enum NegOnly where
  toEnum (-3) = AA4
  toEnum (-2) = BB4
  toEnum (-1) = CC4
  toEnum _ = error "bad toEnum NegOnly"

  fromEnum AA4 = -3
  fromEnum BB4 = -2
  fromEnum CC4 = -1
