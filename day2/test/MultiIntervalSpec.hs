{-# LANGUAGE TemplateHaskell #-}

module MultiIntervalSpec where

import Data.Foldable
import qualified Data.Set as Set
import MultiInterval
import Numeric.Interval.NonEmpty (interval)
import qualified Numeric.Interval.NonEmpty as Interval
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck as QC
import Test.Tasty.TH

instance (Arbitrary a, Ord a) => Arbitrary (MultiInterval a) where
  arbitrary = (...) <$> arbitrary <*> arbitrary

unit_unionXsUnionEmptyIsXs =
  (singleton 5 `union` empty) `shouldBe` singleton 5

unit_unionEmptyUnionXsIsXs =
  (empty `union` singleton 3) `shouldBe` singleton 3

unit_unionEqualsIsIdentity =
  (singleton 12 `union` singleton 12) `shouldBe` singleton 12

unit_unionOverlappingMerges =
  let as = 1 ... 5
      bs = 3 ... 8
   in (as `union` bs) `shouldBe` 1 ... 8

unit_unionTouchingMerges =
  let as = 2 ... 6
      bs = 7 ... 9
   in (as `union` bs) `shouldBe` 2 ... 9

unit_unionDisjointRetainsSplit =
  let as = 3 ... 8
      bs = 12 ... 15
      cs = MI . Set.fromList $ [(Interval....) 3 8, (Interval....) 12 15]
   in (as `union` bs) `shouldBe` cs

prop_unionCommutative :: MultiInterval Int -> MultiInterval Int -> Property
prop_unionCommutative as bs =
  let abs = as `union` bs
      bas = bs `union` as
   in property $ abs `shouldBe` bas

prop_unionAssociative :: MultiInterval Int -> MultiInterval Int -> MultiInterval Int -> Property
prop_unionAssociative as bs cs =
  let leftAssoc = (as `union` bs) `union` cs
      rightAssoc = as `union` (bs `union` cs)
   in property $ leftAssoc `shouldBe` rightAssoc

unit_containsSingletonDoesContain =
  (singleton 7) `shouldSatisfy` (`contains` 7)

unit_containsSingletonDoesn'tContainLT =
  (singleton 9 `contains` 5) `shouldBe` False

unit_containsDisjointDoesContainL =
  let as = (1 ... 3) `union` (8 ... 10)
   in as `shouldSatisfy` (`contains` 3)

unit_containsDisjointDoesContainR =
  let as = (1 ... 3) `union` (8 ... 10)
   in as `shouldSatisfy` (`contains` 9)

unit_intersectionEmptyXsIsEmpty =
  (empty `intersection` (8 ... 15)) `shouldBe` empty

unit_intersectionXsEmptyIsEmpty =
  ((1 ... 5) `intersection` empty) `shouldBe` empty

unit_intersectionOfSingleIntervalsIsOverlap =
  let as = (1 ... 10)
      bs = (8 ... 15)
   in as `intersection` bs `shouldBe` (8 ... 10)

unit_intersectionBothEdges =
  let as = (2 ... 5) `union` (8 ... 13)
      bs = (4 ... 11)
      cs = (4 ... 5) `union` (8 ... 11)
   in as `intersection` bs `shouldBe` cs

unit_intersectionManyToOne =
  let as = (5 ... 25)
      bs = (8 ... 13) `union` (18 ... 21) `union` (24 ... 30)
      cs = (8 ... 13) `union` (18 ... 21) `union` (24 ... 25)
   in as `intersection` bs `shouldBe` cs

unit_intersectionRestOfTheFuckingOwl =
  let as = ((-8) ... (-3)) `union` (1 ... 10) `union` (15 ... 25)
      bs = (8 ... 15) `union` (20 ... 30) `union` (45 ... 100)
      cs = (8 ... 10) `union` singleton 15 `union` (20 ... 25)
   in as `intersection` bs `shouldBe` cs

prop_intersectionCommutative :: MultiInterval Int -> MultiInterval Int -> Property
prop_intersectionCommutative as bs =
  let abs = as `intersection` bs
      bas = bs `intersection` as
   in property $ abs `shouldBe` bas

prop_intersectionAssociative :: MultiInterval Int -> MultiInterval Int -> MultiInterval Int -> Property
prop_intersectionAssociative as bs cs =
  let leftAssoc = (as `intersection` bs) `intersection` cs
      rightAssoc = as `intersection` (bs `intersection` cs)
   in property $ leftAssoc `shouldBe` rightAssoc

unit_diffL = (5 ... 10) `diff` (3 ... 6) `shouldBe` (7 ... 10)

unit_diffR = (3 ... 6) `diff` (5 ... 10) `shouldBe` (3 ... 4)

unit_diffM =
  let a = (1 ... 10)
      b = (4 ... 7)
      c = (1 ... 3) `union` (8 ... 10)
   in a `diff` b `shouldBe` c

unit_diffAll = (5 ... 7) `diff` (1 ... 20) `shouldBe` empty

unit_diffLT = (6 ... 8) `diff` (1 ... 3) `shouldBe` (6 ... 8)

unit_diffGT = (3 ... 9) `diff` (13 ... 17) `shouldBe` (3 ... 9)

unit_diff_restOfTheFuckingOwl =
  let as = (3 ... 8) `union` (12 ... 20) `union` (25 ... 31)
      bs = (8 ... 9) `union` (11 ... 13) `union` (15 ... 18) `union` (20 ... 22) `union` (26 ... 29)
      cs = (3 ... 7) `union` singleton 14 `union` singleton 19 `union` singleton 25 `union` (30 ... 31)
   in as `diff` bs `shouldBe` cs

prop_diffSelfCancels :: MultiInterval Int -> Property
prop_diffSelfCancels as =
  property $ as `diff` as `shouldBe` empty

prop_diffNoIntersection :: MultiInterval Int -> MultiInterval Int -> Property
prop_diffNoIntersection as bs =
  let as' = as `diff` bs
   in property $ as' `intersection` bs `shouldBe` empty

prop_unionOfDiffsIsDiffOfIntersection :: MultiInterval Int -> MultiInterval Int -> Property
prop_unionOfDiffsIsDiffOfIntersection as bs =
  let diffA = as `diff` bs
      diffB = bs `diff` as
      abs1 = diffA `union` diffB
      abs2 = (as `union` bs) `diff` (as `intersection` bs)
   in property $ abs1 `shouldBe` abs2

-- TODO: decide how multiplication should behave, and then test it.
--

unit_iquotNegCrossesZero =
  let as = (-4) ... 4
   in as `iquot` as `shouldBe` as
