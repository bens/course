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
    ]

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
  -> NonEmptyList Int
  -> Bool
prop_findLeft (Fun _ f) (NonEmpty (x:ls)) =
  (if null (filter f ls) then (== IsNotZ) else (/= IsNotZ)) $
    findLeft f (ListZipper ls x [])

prop_findLeft_0_unit ::
  Fun Int Bool
  -> NonEmptyList Int
  -> Bool
prop_findLeft_0_unit (Fun _ f) (NonEmpty (x:ls)) =
  let z = IsZ (ListZipper ls x [])
      u0 = findLeft (const False)
  in (u0 . findLeft f) z == u0 z && (findLeft f . u0) z == u0 z

-- Not currently true
prop_findLeft_1_unit ::
  Fun Int Bool
  -> NonEmptyList Int
  -> Bool
prop_findLeft_1_unit (Fun _ f) (NonEmpty (x:ls)) =
  let z = IsZ (ListZipper ls x [])
      u0 = findLeft (const True)
  in (u0 . findLeft f) z == findLeft f z &&
     (findLeft f . u0) z == findLeft f z

testcase_findRight_empty ::
  Assertion
testcase_findRight_empty =
  findRight (\_ -> True) (IsNotZ :: MaybeListZipper Int) @?= IsNotZ

prop_findRight ::
  Fun Int Bool
  -> NonEmptyList Int
  -> Bool
prop_findRight (Fun _ f) (NonEmpty (x:rs)) =
  (if null (filter f rs) then (== IsNotZ) else (/= IsNotZ)) $
    findRight f (ListZipper [] x rs)

prop_findRight_0_unit ::
  Fun Int Bool
  -> NonEmptyList Int
  -> Bool
prop_findRight_0_unit (Fun _ f) (NonEmpty (x:rs)) =
  let z = IsZ (ListZipper [] x rs)
      u0 = findRight (const False)
  in (u0 . findRight f) z == u0 z && (findRight f . u0) z == u0 z

-- Not currently true
prop_findRight_1_unit ::
  Fun Int Bool
  -> NonEmptyList Int
  -> Bool
prop_findRight_1_unit (Fun _ f) (NonEmpty (x:rs)) =
  let z = IsZ (ListZipper [] x rs)
      u0 = findRight (const True)
  in (u0 . findRight f) z == findRight f z &&
     (findRight f . u0) z == findRight f z
