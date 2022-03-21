{-# LANGUAGE TemplateHaskell #-}

module Day24Spec where

import Control.Lens hiding ((...))
import Data.Foldable hiding (toList)
import Day24
import MultiInterval
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck as QC
import Test.Tasty.TH

unit_invertAmod14Eq7 =
  let inverted = invertInstruction (Mod 1 X (Reg Y)) (0 ... 76) (14 ... 14) 7
      f = _r2ToR1 inverted
      as = toList $ f 14
   in as `shouldBe` [7, 21, 35, 49, 63]

unit_invertAMod3Eq3 =
  let inverted = invertInstruction (Mod 1 X (Reg Y)) (1 ... 50) (3 ... 3) 3
      as = toList $ _r2ToR1 inverted 3
   in as `shouldBe` []

unit_invertAMod6Eq7 =
  let inverted = invertInstruction (Mod 1 X (Reg Y)) (1 ... 50) (6 ... 6) 7
      as = toList $ _r2ToR1 inverted 6
   in as `shouldBe` []

unit_invert15ModBEq3 =
  let inverted = invertInstruction (Mod 1 X (Reg Y)) (15 ... 15) (1 ... 16) 3
      f = _r1ToR2 inverted
      as = toList $ f 15
   in as `shouldBe` [4, 6, 12]

---- a % b = a ==> b = [a + 1, a + 2, ...]
unit_invert7ModBEq7 =
  let inverted = invertInstruction (Mod 1 X (Reg Y)) (7 ... 7) ((-3) ... 12) 7
      as = _r1ToR2 inverted 7
   in as `shouldBe` 8 ... 12

unit_invert7ModBEq7OutOfRange =
  let inverted = invertInstruction (Mod 1 X (Reg Y)) (7 ... 7) (1 ... 7) 7
      as = _r1ToR2 inverted 7
   in as `shouldBe` empty

unit_invert6ModBEq5 =
  let inverted = invertInstruction (Mod 1 X (Reg Y)) (6 ... 6) (1 ... 50) 5
      as = _r1ToR2 inverted 6
   in as `shouldBe` empty

unit_invert5MulBEq12 =
  let inverted = invertInstruction (Mul 1 X (Reg Y)) (5 ... 5) (1 ... 10) 12
      as = _r1ToR2 inverted 5
   in as `shouldBe` empty

unit_invert3MulBEq12 =
  let inverted = invertInstruction (Mul 1 X (Reg Y)) (3 ... 3) (1 ... 10) 12
      as = _r1ToR2 inverted 3
   in as `shouldBe` singleton 4

unit_invertAMul4Eq12 =
  let inverted = invertInstruction (Mul 1 X (Reg Y)) (1 ... 10) (4 ... 4) 12
      as = _r2ToR1 inverted 4
   in as `shouldBe` singleton 3

unit_invert5DivBEq2 =
  let inverted = invertInstruction (Div 1 X (Reg Y)) (5 ... 5) (1 ... 10) 2
      as = _r1ToR2 inverted 5
   in as `shouldBe` singleton 2

unit_invert6DivBEq7 =
  let inverted = invertInstruction (Div 1 X (Reg Y)) (6 ... 6) (1 ... 100) 7
      as = _r1ToR2 inverted 6
   in as `shouldBe` empty

unit_invertBDiv3Eq6 =
  let inverted = invertInstruction (Div 1 X (Reg Y)) ((-21) ... 21) (3 ... 3) 6
      as = toList $ _r2ToR1 inverted 3
   in as `shouldBe` [18, 19, 20]

unit_invertBDiv3EqNeg6 =
  let inverted = invertInstruction (Div 1 X (Reg Y)) ((-21) ... 21) (3 ... 3) (-6)
      as = toList $ _r2ToR1 inverted 3
   in as `shouldBe` [(-20), (-19), (-18)]

unit_invertBDivNeg3Eq6 =
  let inverted = invertInstruction (Div 1 X (Reg Y)) ((-21) ... 21) ((-3) ... (-3)) 6
      as = toList $ _r2ToR1 inverted (-3)
   in as `shouldBe` [(-20), (-19), (-18)]

unit_invertBDivNeg3EqNeg6 =
  let inverted = invertInstruction (Div 1 X (Reg Y)) ((-21) ... 21) ((-3) ... (-3)) (-6)
      as = toList $ _r2ToR1 inverted (-3)
   in as `shouldBe` [18, 19, 20]

unit_invertBDiv8Eq0 =
  let inverted = invertInstruction (Div 1 X (Reg Y)) ((-8) ... 8) (8 ... 8) 0
      as = _r2ToR1 inverted 8
   in as `shouldBe` (-7) ... 7

unit_clampNegMul =
  let instrs =
        [ (Inp 1 W),
          (Add 2 W (Constant (-5))),
          (Inp 3 X),
          (Add 4 X (Constant (-5))),
          (Mul 5 X (Reg W))
        ]
      g = simplify . graphify $ instrs
   in clamp g `shouldBe` ((-16) ... 16)

unit_clampNegDivCrossesZero =
  let instrs =
        [ (Inp 1 W),
          (Add 2 W (Constant (-5))),
          (Inp 3 X),
          (Add 4 X (Constant (-5))),
          (Div 5 X (Reg W))
        ]
      g = simplify . graphify $ instrs
   in clamp g `shouldBe` ((-4) ... 4)

unit_clampNegDiv =
  let instrs =
        [ (Inp 1 X),
          (Add 2 X (Constant (-21))),
          (Inp 3 W),
          (Add 4 W (Constant 3)),
          (Div 5 X (Reg W))
        ]
      g = simplify . graphify $ instrs
   in clamp g `shouldBe` ((-5) ... (-1))

---- TODO: how handle negative mod operands
----
