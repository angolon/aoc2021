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
import Data.Proxy
import qualified Data.Set as Set
import Data.Word (Word16, Word32, Word64, Word8)
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
  ( Show b,
    Eq b,
    Bits w,
    KnownNat n,
    BitSet w (FirstLength' w n),
    b ~ (BS w (FirstLength' w n))
  ) =>
  TestTree
zeroBitsP =
  let zeroBits' :: b
      zeroBits' = zeroBits @b

      n :: (Integral a) => a
      n = fromInteger $ natVal (Proxy :: Proxy n)

      clearProp :: BitIndex w (FirstLength' w n) -> IO ()
      clearProp (BitIndex i) = clearBit zeroBits' i `shouldBe` zeroBits'

      setBitProp :: BitIndex w (FirstLength' w n) -> IO ()
      setBitProp (BitIndex i) = setBit zeroBits' i `shouldBe` bit i

      testBitProp :: BitIndex w (FirstLength' w n) -> IO ()
      testBitProp (BitIndex i) = testBit zeroBits' i `shouldBe` False

      popCountZero = popCount zeroBits' `shouldBe` 0
      popCountComplement = (popCount . complement $ zeroBits') `shouldBe` n
   in testGroup
        "zeroBit laws"
        [ QC.testProperty "clearBit zeroBits n == zeroBits" clearProp,
          QC.testProperty "setBit zeroBits n == bit n" setBitProp,
          QC.testProperty "testBit zeroBits n == False" testBitProp,
          testCase "popCount zeroBits == 0" popCountZero,
          testCase "popCount (complement zeroBits) == n" popCountComplement
        ]

test_partialSingleWord =
  testGroup
    "Partial Single Word BitSet properties"
    [ zeroBitsP @(BitSetW Word64 52) @Word64 @52,
      zeroBitsP @(BitSetW Word64 64) @Word64 @64
    ]

-- zeroBitsP :: (Bits w, BitSet w (Left n), Arbitrary (BS w n), KnownNat n) => forall w n. TestTree
-- zeroBitsP =
--   let clearProp (WordBitIndex i) = clearBit zeroBits i `shouldBe` zeroBits
--    in testGroup "zeroBit laws" [QC.testProperty "clearBit zeroBits n == zeroBits" clearProp]

-- TODO: other test ideas:
-- complement . complement should be identity
-- bitwise operators should be commutative
