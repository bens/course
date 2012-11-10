{-# OPTIONS_GHC -fno-warn-orphans #-}

module L04.ListZipper.Tests where

import Data.Maybe (isJust)
import L03.Fluffy
import L04.ListZipper
import Test.Framework
import Test.Framework.Providers.HUnit       (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit                           hiding (Test, test)
import Test.QuickCheck
import Test.QuickCheck.Function

main ::
  IO ()
main =
  defaultMain [test]

test ::
  Test
test =
  testGroup "ListZipper"
    [
      testCase "furry on ListZipper" testcase_furryListZipper
    , testCase "furry on MaybeListZipper" testcase_furryMaybeListZipper
    , testProperty "fromList/toList roundtrip" prop_fromList_toList
    , testProperty "toMaybe" prop_toMaybe
    , testCase "withFocus (front)" testcase_withFocus_front
    , testCase "withFocus (middle)" testcase_withFocus_middle
    , testCase "setFocus (front)" testcase_setFocus_front
    , testCase "setFocus (middle)" testcase_setFocus_middle
    , testCase "hasLeft (yes)" testcase_hasLeft_yes
    , testCase "hasLeft (no)" testcase_hasLeft_no
    , testCase "hasRight (yes)" testcase_hasRight_yes
    , testCase "hasRight (no)" testcase_hasRight_no
    , testCase "findLeft (empty)" testcase_findLeft_empty
    , testProperty "findLeft" prop_findLeft
    , testProperty "findLeft has a 0 unit" prop_findLeft_0_unit
    -- , testProperty "findLeft has an identity" prop_findLeft_1_unit
    , testCase "findRight (empty)" testcase_findRight_empty
    , testProperty "findRight" prop_findRight
    , testProperty "findRight has a 0 unit" prop_findRight_0_unit
    -- , testProperty "findRight has an identity" prop_findRight_1_unit
    , testCase "moveLeftLoop (empty)" testcase_moveLeftLoop_empty
    , testCase "moveLeftLoop (looping)" testcase_moveLeftLoop_loop
    , testCase "moveLeftLoop (no looping)" testcase_moveLeftLoop_noloop
    , testCase "moveRightLoop (empty)" testcase_moveRightLoop_empty
    , testCase "moveRightLoop (looping)" testcase_moveRightLoop_loop
    , testCase "moveRightLoop (no looping)" testcase_moveRightLoop_noloop
    , testCase "moveLeft (empty)" testcase_moveLeft_empty
    , testCase "moveLeft (at left)" testcase_moveLeft_atLeft
    , testCase "moveLeft (ok)" testcase_moveLeft_notAtLeft
    , testCase "moveRight (empty)" testcase_moveRight_empty
    , testCase "moveRight (at right)" testcase_moveRight_atRight
    , testCase "moveRight (ok)" testcase_moveRight_notAtRight
    , testCase "swapLeft (empty)" testcase_swapLeft_empty
    , testCase "swapLeft (at left)" testcase_swapLeft_atLeft
    , testCase "swapLeft (ok)" testcase_swapLeft_notAtLeft
    , testProperty "swapLeft . swapLeft" prop_swapLeft
    , testCase "swapRight (empty)" testcase_swapRight_empty
    , testCase "swapRight (at right)" testcase_swapRight_atRight
    , testCase "swapRight (ok)" testcase_swapRight_notAtRight
    , testProperty "swapRight . swapRight" prop_swapRight
    , testProperty "dropLefts" prop_dropLefts
    , testProperty "dropRights" prop_dropRights
    ]

instance Arbitrary a => Arbitrary (ListZipper a) where
  arbitrary = do
    ls <- arbitrary
    x  <- arbitrary
    rs <- arbitrary
    return $ ListZipper ls x rs

instance Arbitrary a => Arbitrary (MaybeListZipper a) where
  arbitrary = frequency [(1, return IsNotZ), (80, fmap IsZ arbitrary)]

testcase_furryListZipper ::
  Assertion
testcase_furryListZipper =
  furry (+1) (ListZipper [3,2,1] (4 :: Int) [5,6,7]) @?=
    (ListZipper [4,3,2] 5 [6,7,8])

testcase_furryMaybeListZipper ::
  Assertion
testcase_furryMaybeListZipper =
  furry (+1) (IsZ (ListZipper [3,2,1] (4 :: Int) [5,6,7])) @?=
    IsZ (ListZipper [4,3,2] 5 [6,7,8])

prop_fromList_toList ::
  [Int]
  -> Bool
prop_fromList_toList xs =
  xs == toList (fromList xs)

prop_toMaybe ::
  [Int]
  -> Bool
prop_toMaybe xs =
  if null xs then m == Nothing else m /= Nothing
  where
    m = toMaybe (fromList xs)

testcase_withFocus_front ::
  Assertion
testcase_withFocus_front =
  withFocus (+1) (ListZipper [] 0 [1]) @?= ListZipper [] 1 [1 :: Int]

testcase_withFocus_middle ::
  Assertion
testcase_withFocus_middle =
  withFocus (+1) (ListZipper [1,0] 2 [3,4]) @?= ListZipper [1,0] 3 [3,4 :: Int]

testcase_setFocus_front ::
  Assertion
testcase_setFocus_front =
  setFocus 1 (ListZipper [] 0 [1]) @?= ListZipper [] 1 [1 :: Int]

testcase_setFocus_middle ::
  Assertion
testcase_setFocus_middle =
  setFocus 1 (ListZipper [1,0] 2 [3,4]) @?= ListZipper [1,0] 1 [3,4 :: Int]

testcase_hasLeft_yes ::
  Assertion
testcase_hasLeft_yes =
  hasLeft (ListZipper [1,0] 2 [3,4 :: Int]) @?= True

testcase_hasLeft_no ::
  Assertion
testcase_hasLeft_no =
  hasLeft (ListZipper [] 0 [1,2 :: Int]) @?= False

testcase_hasRight_yes ::
  Assertion
testcase_hasRight_yes =
  hasRight (ListZipper [1,0] 2 [3,4 :: Int]) @?= True

testcase_hasRight_no ::
  Assertion
testcase_hasRight_no =
  hasRight (ListZipper [1,0 :: Int] 2 []) @?= False

testcase_findLeft_empty ::
  Assertion
testcase_findLeft_empty =
  findLeft (\_ -> True) (IsNotZ :: MaybeListZipper Int) @?= IsNotZ

prop_findLeft ::
  Fun Int Bool
  -> ListZipper Int
  -> Bool
prop_findLeft (Fun _ f) z@(ListZipper ls _ _) =
  (if null (filter f ls) then (== IsNotZ) else (/= IsNotZ)) $ findLeft f z

prop_findLeft_0_unit ::
  Fun Int Bool
  -> MaybeListZipper Int
  -> Bool
prop_findLeft_0_unit (Fun _ f) z =
  let u0 = findLeft (const False)
  in (u0 . findLeft f) z == u0 z && (findLeft f . u0) z == u0 z

-- Not currently true
prop_findLeft_1_unit ::
  Fun Int Bool
  -> MaybeListZipper Int
  -> Bool
prop_findLeft_1_unit (Fun _ f) z =
  let u0 = findLeft (const True)
  in (u0 . findLeft f) z == findLeft f z &&
     (findLeft f . u0) z == findLeft f z

testcase_findRight_empty ::
  Assertion
testcase_findRight_empty =
  findRight (\_ -> True) (IsNotZ :: MaybeListZipper Int) @?= IsNotZ

prop_findRight ::
  Fun Int Bool
  -> ListZipper Int
  -> Bool
prop_findRight (Fun _ f) z@(ListZipper _ _ rs) =
  (if null (filter f rs) then (== IsNotZ) else (/= IsNotZ)) $ findRight f z

prop_findRight_0_unit ::
  Fun Int Bool
  -> MaybeListZipper Int
  -> Bool
prop_findRight_0_unit (Fun _ f) z =
  let u0 = findRight (const False)
  in (u0 . findRight f) z == u0 z && (findRight f . u0) z == u0 z

-- Not currently true
prop_findRight_1_unit ::
  Fun Int Bool
  -> MaybeListZipper Int
  -> Bool
prop_findRight_1_unit (Fun _ f) z =
  let u0 = findRight (const True)
  in (u0 . findRight f) z == findRight f z &&
     (findRight f . u0) z == findRight f z

testcase_moveLeftLoop_empty ::
  Assertion
testcase_moveLeftLoop_empty =
  moveLeftLoop IsNotZ @?= (IsNotZ :: MaybeListZipper Int)

testcase_moveLeftLoop_loop ::
  Assertion
testcase_moveLeftLoop_loop =
  moveLeftLoop (ListZipper [] 0 [1,2]) @?= ListZipper [1,0 :: Int] 2 []

testcase_moveLeftLoop_noloop ::
  Assertion
testcase_moveLeftLoop_noloop =
  moveLeftLoop (ListZipper [0] 1 [2]) @?= ListZipper [] 0 [1,2 :: Int]

testcase_moveRightLoop_empty ::
  Assertion
testcase_moveRightLoop_empty =
  moveRightLoop IsNotZ @?= (IsNotZ :: MaybeListZipper Int)

testcase_moveRightLoop_loop ::
  Assertion
testcase_moveRightLoop_loop =
  moveRightLoop (ListZipper [1,0 :: Int] 2 []) @?= ListZipper [] 0 [1,2]

testcase_moveRightLoop_noloop ::
  Assertion
testcase_moveRightLoop_noloop =
  moveRightLoop (ListZipper [0] 1 [2 :: Int]) @?= ListZipper [1,0] 2 []

testcase_moveLeft_empty ::
  Assertion
testcase_moveLeft_empty =
  moveLeft IsNotZ @?= (IsNotZ :: MaybeListZipper Int)

testcase_moveLeft_atLeft ::
  Assertion
testcase_moveLeft_atLeft =
  moveLeft (IsZ (ListZipper [] 0 [1,2 :: Int])) @?= IsNotZ

testcase_moveLeft_notAtLeft ::
  Assertion
testcase_moveLeft_notAtLeft =
  moveLeft (ListZipper [0] 1 [2]) @?= IsZ (ListZipper [] 0 [1,2 :: Int])

testcase_moveRight_empty ::
  Assertion
testcase_moveRight_empty =
  moveRight IsNotZ @?= (IsNotZ :: MaybeListZipper Int)

testcase_moveRight_atRight ::
  Assertion
testcase_moveRight_atRight =
  moveRight (IsZ (ListZipper [1,2 :: Int] 0 [])) @?= IsNotZ

testcase_moveRight_notAtRight ::
  Assertion
testcase_moveRight_notAtRight =
  moveRight (ListZipper [0] 1 [2]) @?= IsZ (ListZipper [1,0 :: Int] 2 [])

testcase_swapLeft_empty ::
  Assertion
testcase_swapLeft_empty =
  swapLeft IsNotZ @?= (IsNotZ :: MaybeListZipper Int)

testcase_swapLeft_atLeft ::
  Assertion
testcase_swapLeft_atLeft =
  swapLeft (ListZipper [] 0 [1,2 :: Int]) @?= IsNotZ

testcase_swapLeft_notAtLeft ::
  Assertion
testcase_swapLeft_notAtLeft =
  swapLeft (ListZipper [1,0] 2 []) @?= IsZ (ListZipper [2,0 :: Int] 1 [])

prop_swapLeft ::
  (Int, Int, [Int])
  -> Bool
prop_swapLeft (x, l, ls) =
  let z = IsZ (ListZipper (l:ls) x [])
  in (swapLeft . swapLeft) z == z

testcase_swapRight_empty ::
  Assertion
testcase_swapRight_empty =
  swapRight IsNotZ @?= (IsNotZ :: MaybeListZipper Int)

testcase_swapRight_atRight ::
  Assertion
testcase_swapRight_atRight =
  swapRight (ListZipper [1,0 :: Int] 2 []) @?= IsNotZ

testcase_swapRight_notAtRight ::
  Assertion
testcase_swapRight_notAtRight =
  swapRight (ListZipper [] 0 [1,2]) @?= IsZ (ListZipper [] 1 [0,2 :: Int])

prop_swapRight ::
  (Int, Int, [Int])
  -> Bool
prop_swapRight (x, r, rs) =
  let z = IsZ (ListZipper [] x (r:rs))
  in (swapRight . swapRight) z == z

prop_dropLefts ::
  MaybeListZipper Int
  -> Bool
prop_dropLefts z =
  hasLeft (dropLefts z) == False

prop_dropRights ::
  MaybeListZipper Int
  -> Bool
prop_dropRights z =
  hasRight (dropRights z) == False
