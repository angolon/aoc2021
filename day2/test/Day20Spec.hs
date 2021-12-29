{-# LANGUAGE TemplateHaskell #-}

module Day20Spec where

import Control.Lens
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

unit_pixelBounds =
  let img = Set.fromList [(-1, 8), (5, -11), (71, 3), (62, 99)]
      expectedBounds = Bounds (-1) 71 (-11) 99
   in pixelBounds img `shouldBe` expectedBounds

unit_expandBounds =
  let b = Bounds 0 2 (-1) 7
   in expandBounds b `shouldBe` Bounds (-2) 4 (-3) 9

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

test_inBounds =
  let b = Bounds 3 10 (-1) 4
      bottomLeft = (b ^. minX, b ^. minY)
      bottomRight = (b ^. maxX, b ^. minY)
      topLeft = (b ^. minX, b ^. maxY)
      topRight = (b ^. maxX, b ^. maxY)
      out =
        [ bottomLeft & _1 -~ 1,
          bottomLeft & _2 -~ 1,
          bottomRight & _1 +~ 1,
          bottomRight & _2 -~ 1,
          topLeft & _1 -~ 1,
          topLeft & _2 +~ 1,
          topRight & _1 +~ 1,
          topRight & _2 +~ 1
        ]
   in testGroup
        "inBounds"
        [ testCase "bottomLeft" $ (bottomLeft `inBounds` b) `shouldBe` True,
          testCase "bottomRight" $ (bottomRight `inBounds` b) `shouldBe` True,
          testCase "topLeft" $ (topLeft `inBounds` b) `shouldBe` True,
          testCase "topRight" $ (topRight `inBounds` b) `shouldBe` True,
          testCase "out of bounds" $
            all (not . (`inBounds` b)) out `shouldBe` True
        ]

-- unit_windowBits =
--   let img = Set.fromList [(0, 1), (1, 2)]
--    in windowBits img 1 1 `shouldBe` 34
