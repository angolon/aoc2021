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

bitSetProps ::
  forall b w n.
  ( Show b,
    Eq b,
    Bits w,
    KnownNat n,
    BitSet w (FirstLength' w n),
    b ~ (BS w (FirstLength' w n)),
    Arbitrary b
  ) =>
  String ->
  TestTree
bitSetProps groupName =
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

      -- TODO: it'd be "nicer" get the masks for these test using arithmetic, rather than relying
      -- on the `shift` operation to get the mask bits.
      shiftLR :: BitSetW w n -> BitIndex w (FirstLength' w n) -> Property
      shiftLR bs (BitIndex i) =
        let offset = n - i
            mask :: BitSetW w n
            mask = (`shiftR` i) . complement $ zeroBits
            masked = bs .&. mask
            shifted = (bs `shiftL` i) `shiftR` i
         in i < n ==> shifted `shouldBe` masked

      shiftRL :: BitSetW w n -> BitIndex w (FirstLength' w n) -> Property
      shiftRL bs (BitIndex i) =
        let mask :: BitSetW w n
            mask = (`shiftL` i) . complement $ zeroBits
            masked = bs .&. mask
            shifted = (bs `shiftR` i) `shiftL` i
         in i < n ==> shifted `shouldBe` masked

      rotateLnIdentity :: BitSetW w n -> IO ()
      rotateLnIdentity bs = (bs `rotateL` n) `shouldBe` bs

      rotateRnIdentity :: BitSetW w n -> IO ()
      rotateRnIdentity bs = (bs `rotateR` n) `shouldBe` bs

      -- rotations won't overflow, so we can test with plain integer arguments.
      rotateLPreservesPopCount :: BitSetW w n -> Int -> IO ()
      rotateLPreservesPopCount bs i = popCount (bs `rotateL` i) `shouldBe` popCount bs

      rotateRPreservesPopCount :: BitSetW w n -> Int -> IO ()
      rotateRPreservesPopCount bs i = popCount (bs `rotateR` i) `shouldBe` popCount bs

      rotateLRIdentity :: BitSetW w n -> Int -> IO ()
      rotateLRIdentity bs i = (bs `rotateL` i) `rotateR` i `shouldBe` bs

      rotateRLIdentity :: BitSetW w n -> Int -> IO ()
      rotateRLIdentity bs i = (bs `rotateR` i) `rotateL` i `shouldBe` bs

      rotateLshiftLPartial :: BitSetW w n -> BitIndex w (FirstLength' w n) -> IO ()
      rotateLshiftLPartial bs (BitIndex i) =
        let rotBs = bs `rotateL` i
            shiftBs = bs `shiftL` i
         in rotBs .&. shiftBs `shouldBe` shiftBs

      rotateRshiftRPartial :: BitSetW w n -> BitIndex w (FirstLength' w n) -> IO ()
      rotateRshiftRPartial bs (BitIndex i) =
        let rotBs = bs `rotateR` i
            shiftBs = bs `shiftR` i
         in rotBs .&. shiftBs `shouldBe` shiftBs
   in testGroup
        groupName
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
          QC.testProperty "shifting right then left should preserve the bits on the left." shiftRL,
          QC.testProperty "bs `rotateL` n == bs" rotateLnIdentity,
          QC.testProperty "bs `rotateR` n == bs" rotateRnIdentity,
          QC.testProperty "popCount (bs `rotateL` n) == popCount bs" rotateLPreservesPopCount,
          QC.testProperty "popCount (bs `rotateR` n) == popCount bs" rotateRPreservesPopCount,
          QC.testProperty "rotateR n . rotateL n == id" rotateLRIdentity,
          QC.testProperty "rotateL n . rotateR n == id" rotateRLIdentity,
          QC.testProperty "rotateL and shiftL should match on the leftmost bits" rotateLshiftLPartial,
          QC.testProperty "rotateR and shiftR should match on the rightmost bits" rotateRshiftRPartial
        ]

test_BitSet =
  testGroup
    "BitSet properties"
    [ bitSetProps @(BitSetW Word64 52) @Word64 @52 "partial single Word64",
      bitSetProps @(BitSetW Word64 64) @Word64 @64 "whole single Word64",
      bitSetProps @(BitSetW Word64 103) @Word64 @103 "partial double Word64",
      bitSetProps @(BitSetW Word64 128) @Word64 @128 "whole double Word64",
      bitSetProps @(BitSetW Word64 129) @Word64 @129 "partial triple Word64",
      bitSetProps @(BitSetW Word64 192) @Word64 @192 "whole triple Word64",
      bitSetProps @(BitSetW Word64 1903) @Word64 @1903 "Large bitset of Word64"
    ]
