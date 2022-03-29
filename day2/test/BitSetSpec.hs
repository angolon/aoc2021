{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module BitSetSpec where

import BitSet
import Data.Bits
import Data.Foldable
import Data.Kind
import qualified Data.Set as Set
import Data.Word (Word64)
import GHC.TypeLits
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck as QC
import Test.Tasty.TH

type WordT = Word64

type PartialSingleWord = BitSet64 52

type WholeSingleWord = BitSet64 64

newtype BitIndex w n = BitIndex Int deriving (Eq, Ord, Show)

instance (BitSet w n) => Arbitrary (BitIndex w n) where
  arbitrary = BitIndex <$> chooseInt (0, (bitsetWidth @w @n) - 1)

unit_singleBit =
  let a :: BitSet64 64
      a = singleBit 7
   in a `shouldSatisfy` (`testBit` 7)

zeroBitsP ::
  forall b w n.
  ( ShiftHelper w (FirstLength' w n),
    Show b,
    Eq b,
    Bits w,
    BitSet w (FirstLength' w n),
    b ~ (BS w (FirstLength' w n))
  ) =>
  TestTree
zeroBitsP =
  let zeroBits' :: b
      zeroBits' = zeroBits @b
      clearProp :: BitIndex w (FirstLength' w n) -> IO ()
      clearProp (BitIndex i) = clearBit zeroBits' i `shouldBe` zeroBits'
   in testGroup "zeroBit laws" [QC.testProperty "clearBit zeroBits n == zeroBits" clearProp]

test_partialSingleWord =
  testGroup
    "Partial Single Word BitSet properties"
    [ zeroBitsP @PartialSingleWord @Word64 @52
    ]

-- zeroBitsP :: (Bits w, BitSet w (Left n), Arbitrary (BS w n), KnownNat n) => forall w n. TestTree
-- zeroBitsP =
--   let clearProp (WordBitIndex i) = clearBit zeroBits i `shouldBe` zeroBits
--    in testGroup "zeroBit laws" [QC.testProperty "clearBit zeroBits n == zeroBits" clearProp]

-- TODO: other test ideas:
-- popCount empty should be zero
-- popCount . complement $ empty should be n
-- complement . complement should be identity
-- bitwise operators should be commutative
