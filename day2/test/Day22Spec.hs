{-# LANGUAGE TemplateHaskell #-}

module Day22Spec where

import Control.Lens
import Data.Foldable
import qualified Data.List as List
import qualified Data.Map.Monoidal as MMap
import Data.Monoid (Endo (..))
import qualified Data.Set as Set
import Day22
import Linear.V3 (V3 (..))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck as QC
import Test.Tasty.TH

unit_dimensionMergeEqual =
  let d1 = DimensionSegment 1 2
   in d1 `dimensionMerge` d1 `shouldBe` Just d1

unit_dimensionMergeOverlapping =
  let d1 = DimensionSegment 1 5
      d2 = DimensionSegment 3 7
      d3 = Just $ DimensionSegment 1 7
   in d1 `dimensionMerge` d2 `shouldBe` d3

unit_dimensionMergeTouching =
  let d1 = DimensionSegment 6 8
      d2 = DimensionSegment 9 13
      d3 = Just $ DimensionSegment 6 13
   in d1 `dimensionMerge` d2 `shouldBe` d3

unit_dimensionMergeNotTouchingRight =
  let d1 = DimensionSegment 3 5
      d2 = DimensionSegment 7 8
   in d1 `dimensionMerge` d2 `shouldBe` Nothing

unit_dimensionMergeNotTouchingLeft =
  let d1 = DimensionSegment 6 13
      d2 = DimensionSegment 1 2
   in d1 `dimensionMerge` d2 `shouldBe` Nothing

unit_dimensionOverwriteEqual =
  let d1 = DimensionSegment 1 2
   in d1 `dimensionOverwrite` d1 `shouldBe` Just (Nothing, Nothing)

unit_dimensionOverwriteContainsPrior =
  let prior = DimensionSegment 2 4
      next = DimensionSegment 1 5
   in prior `dimensionOverwrite` next `shouldBe` Just (Nothing, Nothing)

unit_dimensionOverwriteOverlapsLowerBound =
  let prior = DimensionSegment 3 7
      next = DimensionSegment 1 5
      expectedUpper = DimensionSegment 6 7
   in prior `dimensionOverwrite` next `shouldBe` Just (Nothing, Just expectedUpper)

unit_dimensionOverwriteOverlapsUpperBound =
  let prior = DimensionSegment 3 7
      next = DimensionSegment 5 9
      expectedLower = DimensionSegment 3 4
   in prior `dimensionOverwrite` next `shouldBe` Just (Just expectedLower, Nothing)

unit_dimensionOverwriteContainedWithinPrior =
  let prior = DimensionSegment (-1) 8
      next = DimensionSegment 2 6
      expectedLeft = DimensionSegment (-1) 1
      expectedRight = DimensionSegment 7 8
   in prior `dimensionOverwrite` next `shouldBe` Just (Just expectedLeft, Just expectedRight)

unit_dimensionOverwriteNoOverlapOnRight =
  let prior = DimensionSegment 1 3
      next = DimensionSegment 4 6
   in prior `dimensionOverwrite` next `shouldBe` Nothing

unit_dimensionOverwriteNoOverlapOnLeft =
  let prior = DimensionSegment 7 10
      next = DimensionSegment 3 6
   in prior `dimensionOverwrite` next `shouldBe` Nothing

unit_n3OverwriteHalf =
  let s1 = DimensionSegment 2 5
      c1 = N3Section s1 s1 s1
      c2 = N3Section (DimensionSegment 4 6) (DimensionSegment 0 8) (DimensionSegment 0 8)
      expectedCube = N3Section (DimensionSegment 2 3) s1 s1
   in c1 `n3SectionOverwrite` c2 `shouldBe` Just [expectedCube]

unit_mergeN3Sections =
  let sections =
        [ N3Section (DimensionSegment 10 10) (DimensionSegment 11 12) (DimensionSegment 11 12),
          N3Section (DimensionSegment 11 12) (DimensionSegment 10 10) (DimensionSegment 11 12),
          N3Section (DimensionSegment 11 12) (DimensionSegment 11 12) (DimensionSegment 10 10),
          N3Section (DimensionSegment 10 10) (DimensionSegment 10 10) (DimensionSegment 11 12),
          N3Section (DimensionSegment 10 10) (DimensionSegment 10 10) (DimensionSegment 10 10),
          N3Section (DimensionSegment 10 10) (DimensionSegment 11 12) (DimensionSegment 10 10),
          N3Section (DimensionSegment 11 12) (DimensionSegment 10 10) (DimensionSegment 10 10)
        ]
      expectedSections =
        [ N3Section (DimensionSegment 10 12) (DimensionSegment 11 12) (DimensionSegment 10 10),
          N3Section (DimensionSegment 11 12) (DimensionSegment 10 10) (DimensionSegment 10 12),
          N3Section (DimensionSegment 10 10) (DimensionSegment 10 12) (DimensionSegment 11 12),
          N3Section (DimensionSegment 10 10) (DimensionSegment 10 10) (DimensionSegment 10 10)
        ]
   in mergeN3Sections sections `shouldBe` expectedSections

-- unit_n3OverwriteCorner =
--   let priorAllD = DimensionSegment 0 4
--       priorN3 = N3Section priorAllD priorAllD priorAllD
--       nextAllD = DimensionSegment 3 6
--       nextN3 = N3Section nextAllD nextAllD nextAllD
--       fragment1 = N3Section (DimensionSegment 0 2) (DimensionSegment 0 4) (DimensionSegment 0 4)
--       fragment2 = N3Section priorAllD (DimensionSegment 3 4) (DimensionSegment 0 2)
--       fragment3 = N3Section (DimensionSegment  (DimensionSegment 3 4) (DimensionSegment 3 4)
--       expectedPoints = Set.toList . Set.fromList $ n3Points =<< [fragment1, fragment2, fragment3]
--       overwritten = n3Points =<< n3SectionOverwrite priorN3 nextN3
--       actualPoints = List.sort overwritten
--    in (print . length $ actualPoints) >> (actualPoints `shouldBe` expectedPoints)

-- unit_n3OverwriteEdgeMiddle =
--   let priorAllD = DimensionSegment 0 8
--       priorN3 = N3Section priorAllD priorAllD priorAllD
--       nextN3 = N3Section (DimensionSegment 7 9) (DimensionSegment 3 6) (DimensionSegment 7 9)
--       fragment1 = N3Section (DimensionSegment 0 8) (DimensionSegment 0 8) (DimensionSegment 0 6)
--       fragment2 = N3Section (DimensionSegment 0 8) (DimensionSegment 0 2) (DimensionSegment 7 8)
--       fragment3 = N3Section (DimensionSegment 0 6) (DimensionSegment 3 6) (DimensionSegment 7 8)
--       fragment4 = N3Section (DimensionSegment 0 8) (DimensionSegment 7 8) (DimensionSegment 7 8)
--       expectedPoints = Set.fromList $ n3Points =<< [fragment1, fragment2, fragment3, fragment4]
--       overwritten = n3Points =<< n3SectionOverwrite priorN3 nextN3
--       actualPoints = Set.fromList overwritten
--    in actualPoints `shouldBe` expectedPoints

-- unit_equalBothOn =
--   let cube1 = Cuboid True (1, 2) (1, 2) (1, 2)
--    in cube1 `addCuboids` cube1 `shouldBe` [cube1]

-- unit_noOverlapBothOn =
--   let cube1 = Cuboid True (1, 2) (1, 2) (1, 2)
--       cube2 = Cuboid True (1, 2) (1, 2) (4, 5)
--    in cube1 `addCuboids` cube2 `shouldBe` [cube1, cube2]

-- unit_sharedFaceXYBothOn =
--   let cube1 = Cuboid True (1, 2) (1, 2) (1, 2)
--       cube2 = Cuboid True (1, 2) (1, 2) (3, 7)
--       expectedCube = Cuboid True (1, 2) (1, 2) (1, 7)
--    in cube1 `addCuboids` cube2 `shouldBe` [expectedCube]
