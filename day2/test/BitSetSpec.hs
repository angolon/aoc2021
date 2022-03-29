{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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
import Test.Tasty.QuickCheck as QC hiding ((.&.))
import Test.Tasty.TH

type WordT = Word64

type PartialSingleWord = BitSet64 52

type WholeSingleWord = BitSet64 64

newtype BitIndex w n = BitIndex Int deriving (Eq, Ord, Show)

instance (BitSet w n) => Arbitrary (BitIndex w n) where
  arbitrary = BitIndex <$> chooseInt (0, (bitsetWidth @w @n) - 1)

instance
  ( KnownNat n,
    KnownNat (WordWidth w),
    Arbitrary w,
    Bits w,
    Integral w
  ) =>
  Arbitrary (BS w (Right (n :: Nat)))
  where
  arbitrary = BSEnd . (.&. (wordMask @w @(Right n))) <$> arbitrary

instance
  ( Bits w,
    Integral w,
    KnownNat n,
    KnownNat (WordWidth w),
    BitSetN w n,
    Arbitrary w,
    Arbitrary (BS w (BSN w n))
  ) =>
  Arbitrary (BS w (Left (n :: Nat)))
  where
  arbitrary = BSCons <$> (((wordMask @w @(Left n)) .&.) <$> arbitrary) <*> arbitrary

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
    b ~ (BS w (FirstLength' w n)),
    Arbitrary b
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

      doubleComplement :: BS w (FirstLength' w n) -> IO ()
      doubleComplement bs = (complement . complement $ bs) `shouldBe` bs

      popCountPlus :: BS w (FirstLength' w n) -> IO ()
      popCountPlus bs = popCount bs + (popCount . complement $ bs) `shouldBe` n

      commutative :: (BitSetW w n -> BitSetW w n -> BitSetW w n) -> BitSetW w n -> BitSetW w n -> IO ()
      commutative op as bs = as `op` bs `shouldBe` bs `op` as

      shiftL0 :: BitSetW w n -> IO ()
      shiftL0 bs = bs `shiftL` 0 `shouldBe` bs

      shiftLN :: BitSetW w n -> IO ()
      shiftLN bs = bs `shiftL` n `shouldBe` zeroBits

      shiftRN :: BitSetW w n -> IO ()
      shiftRN bs = bs `shiftR` n `shouldBe` zeroBits

      shiftR0 :: BitSetW w n -> IO ()
      shiftR0 bs = bs `shiftR` 0 `shouldBe` bs

      shiftLR :: BitSetW w n -> BitIndex w (FirstLength' w n) -> Property
      shiftLR bs (BitIndex i) =
        let offset = n - i
            mask :: BitSetW w n
            mask = foldl' (\x _ -> (`setBit` 0) . (`shiftL` 1) $ x) zeroBits [1 .. offset]
            masked = bs .&. mask
            shifted = (bs `shiftL` i) `shiftR` i
         in i < n ==> shifted `shouldBe` masked

      shiftRL :: BitSetW w n -> BitIndex w (FirstLength' w n) -> Property
      shiftRL bs (BitIndex i) =
        let maskComplement :: BitSetW w n
            maskComplement = foldl' (\x _ -> (`setBit` 0) . (`shiftL` 1) $ x) zeroBits [1 .. i]
            mask = complement maskComplement
            masked = bs .&. mask
            shifted = (bs `shiftR` i) `shiftL` i
         in i < n ==> shifted `shouldBe` masked
   in testGroup
        "zeroBit laws"
        [ QC.testProperty "clearBit zeroBits n == zeroBits" clearProp,
          QC.testProperty "setBit zeroBits n == bit n" setBitProp,
          QC.testProperty "testBit zeroBits n == False" testBitProp,
          testCase "popCount zeroBits == 0" popCountZero,
          testCase "popCount (complement zeroBits) == n" popCountComplement,
          QC.testProperty "complement . complement == id" doubleComplement,
          QC.testProperty "popCount bs + (popCount . complement $ bs) == n" popCountPlus,
          QC.testProperty "(.|.) is commutative" $ commutative (.|.),
          QC.testProperty "(.&.) is commutative" $ commutative (.&.),
          QC.testProperty "xor is commutative" $ commutative xor,
          QC.testProperty "bs `shiftL` n == zeroBits" shiftLN,
          QC.testProperty "bs `shiftL` 0 == bs" shiftL0,
          QC.testProperty "bs `shiftR` n == zeroBits" shiftRN,
          QC.testProperty "bs `shiftR` 0 == bs" shiftR0,
          QC.testProperty "shifting left then right should preserve the bits on the right." shiftLR,
          QC.testProperty "shifting right then left should preserve the bits on the left." shiftRL
        ]

test_partialSingleWord =
  testGroup
    "Partial Single Word BitSet properties"
    [ zeroBitsP @(BitSetW Word64 52) @Word64 @52,
      zeroBitsP @(BitSetW Word64 64) @Word64 @64,
      zeroBitsP @(BitSetW Word64 103) @Word64 @103,
      zeroBitsP @(BitSetW Word64 128) @Word64 @128
    ]

-- zeroBitsP :: (Bits w, BitSet w (Left n), Arbitrary (BS w n), KnownNat n) => forall w n. TestTree
-- zeroBitsP =
--   let clearProp (WordBitIndex i) = clearBit zeroBits i `shouldBe` zeroBits
--    in testGroup "zeroBit laws" [QC.testProperty "clearBit zeroBits n == zeroBits" clearProp]
