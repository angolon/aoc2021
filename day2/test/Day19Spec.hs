{-# LANGUAGE TemplateHaskell #-}

module Day19Spec where

import Data.Foldable
import qualified Data.Map.Monoidal as MMap
import Data.Monoid (Endo (..))
import Day19
import Linear.V3 (V3 (..))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck as QC
import Test.Tasty.TH

instance (Arbitrary a) => Arbitrary (V3 a) where
  arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary

unit_allOrientationsLength =
  length allFacingOrientations @?= 24

test_angleBetween =
  testGroup
    "angleBetween"
    [ testCase "xy" $ angleBetween xAxis yAxis @?= pi / 2,
      testCase "xz" $ angleBetween xAxis zAxis @?= pi / 2,
      testCase "x negx" $ angleBetween (V3 (-1) 0 0) (V3 1 0 0) @?= pi
    ]

prop_reorientationInversionIsIdentity :: V3 Int -> Property
prop_reorientationInversionIsIdentity v =
  let us = _reorient <$> allFacingOrientations <*> [v]
      ws = zipWith invert allFacingOrientations us
   in property $ ws `shouldSatisfy` (all (== v))

unit_recurseLookupIsn'tGarbage =
  let k1 = 1
      k2 = 2
      k3 = 3
      k4 = 4
      f1to2 = Endo (+ 7)
      f2to3 = Endo (`div` 3)
      f3to4 = Endo (* 5)
      m1 = MMap.singleton k1 [(k2, f1to2)]
      m2 = MMap.singleton k2 [(k3, f2to3)]
      m3 = MMap.singleton k3 [(k4, f3to4)]
      mfs = fold [m1, m2, m3]
      (Just ffs) = recurseLookup mfs 4 1
   in appEndo ffs 8 `shouldBe` 25

test_recurseDoesn'tLoopInfinitely =
  let k1 = 1
      k2 = 2
      k3 = 3
      k4 = 4
      f1to2 = Endo (+ 7)
      f2to1 = Endo (subtract 7)
      f2to3 = Endo (`div` 3)
      f3to4 = Endo (* 5)
      m1 = MMap.singleton k1 [(k2, f1to2)]
      m2 = MMap.singleton k2 [(k3, f2to3), (k1, f2to1)]
      m3 = MMap.singleton k3 [(k4, f3to4)]
      mfs = fold [m1, m2, m3]
      (Just ffs) = recurseLookup mfs 4 1
      imp = 1 `shouldBe` 2
   in localOption (Timeout 10 "s") . testCase "no infinite loops" $
        appEndo ffs 8 `shouldBe` 25
