{-# LANGUAGE TemplateHaskell #-}

module Day20Spec where

import Data.Foldable
import qualified Data.Map.Monoidal as MMap
import Data.Monoid (Endo (..))
import qualified Data.Set as Set
import Day20
import Linear.V3 (V3 (..))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck as QC
import Test.Tasty.TH

unit_imageBounds =
  let img = Set.fromList [(-1, 8), (5, -11), (71, 3), (62, 99)]
      expectedBounds = Bounds (-1) 71 (-11) 99
   in imageBounds img `shouldBe` expectedBounds

unit_expandBounds =
  let b = Bounds 0 2 (-1) 7
   in expandBounds b `shouldBe` Bounds (-1) 3 (-2) 8

unit_enhancementWindow =
  enhancementWindow 0 4
    `shouldBe` [ (-1, 3),
                 (0, 3),
                 (1, 3),
                 (-1, 4),
                 (0, 4),
                 (1, 4),
                 (-1, 5),
                 (0, 5),
                 (1, 5)
               ]

unit_windowBits =
  let img = Set.fromList [(0, 1), (1, 2)]
   in windowBits img 1 1 `shouldBe` 34
