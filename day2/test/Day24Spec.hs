{-# LANGUAGE TemplateHaskell #-}

module Day24Spec where

import Control.Lens
import Data.Foldable
import Day24
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck as QC
import Test.Tasty.TH

unit_invertAmod14Eq7 =
  let inverted = invertInstruction (Mod 1 X (Reg Y)) 7
      f = _r2ToR1 inverted
      as = take 5 $ f 14
   in as `shouldBe` [7, 21, 35, 49, 63]

unit_invertAMod3Eq3 =
  let inverted = invertInstruction (Mod 1 X (Reg Y)) 3
      as = _r2ToR1 inverted 3
   in as `shouldBe` []

unit_invertAMod6Eq7 =
  let inverted = invertInstruction (Mod 1 X (Reg Y)) 7
      as = _r2ToR1 inverted 6
   in as `shouldBe` []

unit_invert15ModBEq3 =
  let inverted = invertInstruction (Mod 1 X (Reg Y)) 3
      f = _r1ToR2 inverted
      as = f 15
   in as `shouldBe` [4, 6, 12]

-- a % b = a ==> b = [a + 1, a + 2, ...]
unit_invert7ModBEq7 =
  let inverted = invertInstruction (Mod 1 X (Reg Y)) 7
      as = take 5 $ _r1ToR2 inverted 7
   in as `shouldBe` [8 .. 12]

unit_invert6ModBEq5 =
  let inverted = invertInstruction (Mod 1 X (Reg Y)) 5
      as = _r1ToR2 inverted 6
   in as `shouldBe` []

unit_invert5MulBEq12 =
  let inverted = invertInstruction (Mul 1 X (Reg Y)) 12
      as = _r1ToR2 inverted 5
   in as `shouldBe` []

unit_invert3MulBEq12 =
  let inverted = invertInstruction (Mul 1 X (Reg Y)) 12
      as = _r1ToR2 inverted 3
   in as `shouldBe` [4]

unit_invertAMul4Eq12 =
  let inverted = invertInstruction (Mul 1 X (Reg Y)) 12
      as = _r2ToR1 inverted 4
   in as `shouldBe` [3]

unit_invert5DivBEq2 =
  let inverted = invertInstruction (Div 1 X (Reg Y)) 2
      as = _r1ToR2 inverted 5
   in as `shouldBe` [2]

unit_invert6DivBEq7 =
  let inverted = invertInstruction (Div 1 X (Reg Y)) 7
      as = _r1ToR2 inverted 6
   in as `shouldBe` []

unit_invertBDiv3Eq6 =
  let inverted = invertInstruction (Div 1 X (Reg Y)) 6
      as = _r2ToR1 inverted 3
   in as `shouldBe` [18, 19, 20]

unit_invertBDiv8Eq0 =
  let inverted = invertInstruction (Div 1 X (Reg Y)) 0
      as = _r2ToR1 inverted 8
   in as `shouldBe` [0 .. 7]

-- TODO: how handle negative mod operands
--
