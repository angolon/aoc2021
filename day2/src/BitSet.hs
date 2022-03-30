{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

module BitSet where

import Control.Exception
import Data.Bits
import Data.Kind
import Data.Proxy
import Data.Type.Equality
import GHC.Exts (Constraint)
import GHC.TypeLits
import GHC.Word (Word16, Word32, Word64, Word8)
import Numeric (showHex)

type family WordWidth (a :: Type) :: Nat where
  WordWidth Word8 = 8
  WordWidth Word16 = 16
  WordWidth Word32 = 32
  WordWidth Word64 = 64

type Remainder w n = Mod n (WordWidth w)

type CmpRemainder w n = CmpNat (Remainder w n) 0

type CmpWidth w n = CmpNat n (WordWidth w)

type family NextLength w n cmpRemainder cmpWidth where
  NextLength w n 'GT 'GT = n - (Remainder w n)
  NextLength w n 'EQ 'GT = n - (WordWidth w)
  NextLength _ n 'EQ 'EQ = n

type NextLength' w n = NextLength w n (CmpRemainder w n) (CmpWidth w n)

type family MoreWords n cmp where
  MoreWords n 'GT = Left n
  MoreWords n 'EQ = Right n

type MoreWords' w n = MoreWords (NextLength' w n) (CmpWidth w (NextLength' w n))

type family FirstLength n cmp where
  FirstLength n 'GT = Left n
  FirstLength n 'EQ = Right n
  FirstLength n 'LT = Right n

type FirstLength' w n = FirstLength n (CmpWidth w n)

type BSN w (n :: Nat) = MoreWords' w n

class BitSet w n where
  data BS w n

  empty :: BS w n

  bitsetWidth :: Integral a => a
  upperBitIndex :: Integral a => a
  lowerBitIndex :: Integral a => a
  singleBit :: Int -> BS w n
  bsTestBit :: BS w n -> Int -> Bool
  bsPopCount :: BS w n -> Int

  -- The word at this index
  word :: BS w n -> w

  -- Bitmask for bits in the word that are included in
  -- this bitset. Only matters for the left-most word, which is
  -- potentially shorter than a full word.
  wordMask :: (Integral w, Bits w) => w

  -- The word at the n'th index
  -- All zeros if the index is past the end of the bitset
  wordN :: Int -> BS w n -> w

  -- Leftmost word in bitset
  wordL :: BS w n -> w

  -- Rightmost word in bitset
  wordR :: BS w n -> w

  setWordN :: Int -> w -> BS w n -> BS w n

  shiftRWithCarry :: BS w n -> Int -> w -> BS w n
  shiftRWholeWords :: BS w n -> Int -> BS w n
  shiftRInit :: BS w n -> Int -> BS w n

  -- (Carry, result)
  shiftLWithCarry :: BS w n -> Int -> (w, BS w n)
  shiftLWholeWords :: BS w n -> Int -> BS w n
  shiftLInit :: BS w n -> Int -> BS w n

  mapWords :: (w -> w) -> BS w n -> BS w n

  -- Function used on words 2 to n, which can ignore masking. Not for public consumption.
  mapWords' :: (w -> w) -> BS w n -> BS w n

  zipWordsWith :: (w -> w -> w) -> BS w n -> BS w n -> BS w n

type BitSetN w (n :: Nat) = BitSet w (BSN w n)

wordWidth :: forall a w. (Integral a, KnownNat (WordWidth w)) => a
wordWidth = fromInteger $ natVal (Proxy :: Proxy (WordWidth w))

instance
  ( Bits w,
    Integral w,
    KnownNat n,
    KnownNat (WordWidth w)
  ) =>
  BitSet (w :: Type) (Right (n :: Nat))
  where
  data BS w (Right n) = BSEnd w

  bitsetWidth = fromInteger $ natVal (Proxy :: Proxy n)

  empty = BSEnd zeroBits

  upperBitIndex = fromIntegral (natVal (Proxy :: Proxy (n)))
  {-# INLINE upperBitIndex #-}
  lowerBitIndex = 0
  {-# INLINE lowerBitIndex #-}

  singleBit i
    | 0 <= i && i < (upperBitIndex @w @(Right n)) = BSEnd $ bit i
    | otherwise = throw Overflow
  {-# INLINE singleBit #-}

  bsTestBit (BSEnd w) i
    | 0 <= i && i < (upperBitIndex @w @(Right n)) = testBit w i
    | otherwise = throw Overflow
  {-# INLINE bsTestBit #-}

  bsPopCount (BSEnd w) = popCount w
  {-# INLINE bsPopCount #-}

  word (BSEnd w) = w
  {-# INLINE word #-}

  wordMask = (1 `shiftL` (upperBitIndex @w @(Right n))) - 1
  {-# INLINE wordMask #-}

  wordN 0 = word
  wordN _ = const zeroBits

  setWordN 0 w = const $ BSEnd w
  setWordN _ _ = id

  wordL = word
  wordR = word

  shiftLWholeWords bs 0 = bs
  shiftLWholeWords _ _ = BSEnd zeroBits
  {-# INLINE shiftLWholeWords #-}

  shiftLWithCarry (BSEnd w) n =
    let (carryOut, w') = shiftLCarry w n
     in (carryOut,) $ BSEnd w'
  {-# INLINE shiftLWithCarry #-}

  shiftRWholeWords bs 0 = bs
  shiftRWholeWords _ _ = BSEnd zeroBits
  {-# INLINE shiftRWholeWords #-}

  shiftRWithCarry (BSEnd w) n carryIn =
    let w' = w `shiftR` n
        w'' = carryIn .|. w'
     in BSEnd w''
  {-# INLINE shiftRWithCarry #-}

  mapWords f (BSEnd w) = BSEnd . (.&. (wordMask @w @(Right n))) . f $ w
  {-# INLINE mapWords #-}

  mapWords' f (BSEnd w) = BSEnd . f $ w
  {-# INLINE mapWords' #-}

  zipWordsWith op (BSEnd w) (BSEnd v) = BSEnd $ w `op` v
  {-# INLINE zipWordsWith #-}

  -- If BSEnd contains the first and only word, we can defer to the vanilla
  -- `shiftL` and `shiftR` operations.
  shiftLInit (BSEnd w) n = BSEnd . (.&. (wordMask @w @(Right n))) $ w `shiftL` n
  {-# INLINE shiftLInit #-}

  shiftRInit (BSEnd w) n = BSEnd $ w `shiftR` n
  {-# INLINE shiftRInit #-}

type HackL (a :: Nat) = Left @Nat @() a

instance
  ( Bits w,
    Integral w,
    KnownNat n,
    BitSetN w n,
    KnownNat (WordWidth w)
  ) =>
  BitSet (w :: Type) (Left (n :: Nat))
  where
  data BS w (Left n) = BSCons w (BS w (BSN w n))

  bitsetWidth = fromInteger $ natVal (Proxy :: Proxy n)
  {-# INLINE bitsetWidth #-}

  upperBitIndex = fromIntegral (natVal (Proxy :: Proxy (n)))
  {-# INLINE upperBitIndex #-}
  lowerBitIndex = upperBitIndex @w @(BSN w n)
  {-# INLINE lowerBitIndex #-}

  empty = BSCons zeroBits empty
  {-# INLINE empty #-}

  singleBit i
    | i >= upperBitIndex @w @(Left n) = throw Overflow
    | i < lowerBitIndex @w @(Left n) = BSCons zeroBits $ singleBit i
    | otherwise =
      let i' = i - (lowerBitIndex @w @(Left n))
       in BSCons (bit i') empty
  {-# INLINE singleBit #-}

  bsTestBit (BSCons w ws) i
    | i >= upperBitIndex @w @(Left n) = throw Overflow
    | i < lowerBitIndex @w @(Left n) = bsTestBit ws i
    | otherwise =
      let i' = i - (lowerBitIndex @w @(Left n))
       in testBit w i'
  {-# INLINE bsTestBit #-}

  bsPopCount (BSCons w ws) = popCount w + bsPopCount ws
  {-# INLINE bsPopCount #-}
  {-# SPECIALISE bsPopCount :: BS Word64 (Left 1903) -> Int #-}
  -- {-# SPECIALISE bsPopCount :: (BitSetN Word64 1903, KnownNat (WordWidth Word64)) => BS Word64 (Left 1903) -> Int #-}
  -- {-# SPECIALISE bsPopCount :: (BitSetN Word64 n, KnownNat n, KnownNat (WordWidth Word64)) => BS Word64 (Left n) -> Int #-}

  word (BSCons word _) = word
  {-# INLINE word #-}

  wordMask = (1 `shiftL` ((upperBitIndex @w @(Left n)) - (lowerBitIndex @w @(Left n)))) - 1
  {-# INLINE wordMask #-}

  wordN 0 bs = word bs
  wordN n (BSCons _ ws) = wordN (n - 1) ws

  setWordN 0 w (BSCons _ ws) = BSCons w ws
  setWordN n v (BSCons w ws) = BSCons w $ setWordN (n - 1) v ws

  wordL (BSCons word _) = word

  wordR (BSCons _ words) = wordR words

  shiftLInit bs@(BSCons w ws) n
    | n < 0 = throw Overflow
    | n == 0 = bs
    | n >= bitsetWidth @w @(Left n) = empty
    | n >= (wordWidth @Int @w) =
      let (nWords, nBits) = n `quotRem` (wordWidth @Int @w)
          (BSCons w' ws') = shiftLWholeWords bs nWords
          w'' = (wordMask @w @(Left n)) .&. w'
       in shiftLInit (BSCons w'' ws') nBits
    | n < (wordWidth @Int @w) =
      let (carry, ws') = shiftLWithCarry ws n
          w' = ((w `shiftL` n) .|. carry) .&. (wordMask @w @(Left n))
       in BSCons w' ws'
  {-# INLINE shiftLInit #-}
  {-# SPECIALISE shiftLInit :: BS Word64 (HackL 1903) -> Int -> BS Word64 (HackL 1903)  #-}
  -- {-# SPECIALISE shiftLInit :: (BitSetN Word64 n, KnownNat n, KnownNat (WordWidth Word64)) => BS Word64 (HackL n) -> Int -> BS Word64 (HackL n)  #-}

  shiftLWholeWords bs 0 = bs
  shiftLWholeWords bs@(BSCons w ws) n =
    let w' = wordN n bs
        ws' = shiftLWholeWords ws n
     in BSCons w' ws'
  {-# INLINE shiftLWholeWords #-}
  {-# SPECIALISE shiftLWholeWords :: BS Word64 (HackL 128) -> Int -> BS Word64 (HackL 128)  #-}
  {-# SPECIALISE shiftLWholeWords :: BS Word64 (HackL 192) -> Int -> BS Word64 (HackL 192)  #-}
  {-# SPECIALISE shiftLWholeWords :: BS Word64 (HackL 256) -> Int -> BS Word64 (HackL 256)  #-}
  {-# SPECIALISE shiftLWholeWords :: BS Word64 (HackL 320) -> Int -> BS Word64 (HackL 320)  #-}
  {-# SPECIALISE shiftLWholeWords :: BS Word64 (HackL 384) -> Int -> BS Word64 (HackL 384)  #-}
  {-# SPECIALISE shiftLWholeWords :: BS Word64 (HackL 448) -> Int -> BS Word64 (HackL 448)  #-}
  {-# SPECIALISE shiftLWholeWords :: BS Word64 (HackL 512) -> Int -> BS Word64 (HackL 512)  #-}
  {-# SPECIALISE shiftLWholeWords :: BS Word64 (HackL 576) -> Int -> BS Word64 (HackL 576)  #-}
  {-# SPECIALISE shiftLWholeWords :: BS Word64 (HackL 640) -> Int -> BS Word64 (HackL 640)  #-}
  {-# SPECIALISE shiftLWholeWords :: BS Word64 (HackL 704) -> Int -> BS Word64 (HackL 704)  #-}
  {-# SPECIALISE shiftLWholeWords :: BS Word64 (HackL 768) -> Int -> BS Word64 (HackL 768)  #-}
  {-# SPECIALISE shiftLWholeWords :: BS Word64 (HackL 832) -> Int -> BS Word64 (HackL 832)  #-}
  {-# SPECIALISE shiftLWholeWords :: BS Word64 (HackL 896) -> Int -> BS Word64 (HackL 896)  #-}
  {-# SPECIALISE shiftLWholeWords :: BS Word64 (HackL 960) -> Int -> BS Word64 (HackL 960)  #-}
  {-# SPECIALISE shiftLWholeWords :: BS Word64 (HackL 1024) -> Int -> BS Word64 (HackL 1024)  #-}
  {-# SPECIALISE shiftLWholeWords :: BS Word64 (HackL 1088) -> Int -> BS Word64 (HackL 1088)  #-}
  {-# SPECIALISE shiftLWholeWords :: BS Word64 (HackL 1152) -> Int -> BS Word64 (HackL 1152)  #-}
  {-# SPECIALISE shiftLWholeWords :: BS Word64 (HackL 1216) -> Int -> BS Word64 (HackL 1216)  #-}
  {-# SPECIALISE shiftLWholeWords :: BS Word64 (HackL 1280) -> Int -> BS Word64 (HackL 1280)  #-}
  {-# SPECIALISE shiftLWholeWords :: BS Word64 (HackL 1344) -> Int -> BS Word64 (HackL 1344)  #-}
  {-# SPECIALISE shiftLWholeWords :: BS Word64 (HackL 1408) -> Int -> BS Word64 (HackL 1408)  #-}
  {-# SPECIALISE shiftLWholeWords :: BS Word64 (HackL 1472) -> Int -> BS Word64 (HackL 1472)  #-}
  {-# SPECIALISE shiftLWholeWords :: BS Word64 (HackL 1536) -> Int -> BS Word64 (HackL 1536)  #-}
  {-# SPECIALISE shiftLWholeWords :: BS Word64 (HackL 1600) -> Int -> BS Word64 (HackL 1600)  #-}
  {-# SPECIALISE shiftLWholeWords :: BS Word64 (HackL 1664) -> Int -> BS Word64 (HackL 1664)  #-}
  {-# SPECIALISE shiftLWholeWords :: BS Word64 (HackL 1728) -> Int -> BS Word64 (HackL 1728)  #-}
  {-# SPECIALISE shiftLWholeWords :: BS Word64 (HackL 1792) -> Int -> BS Word64 (HackL 1792)  #-}
  {-# SPECIALISE shiftLWholeWords :: BS Word64 (HackL 1856) -> Int -> BS Word64 (HackL 1856)  #-}
  {-# SPECIALISE shiftLWholeWords :: BS Word64 (HackL 1903) -> Int -> BS Word64 (HackL 1903)  #-}

  shiftLWithCarry (BSCons w ws) n =
    let (carryOut, w') = shiftLCarry w n
        (carryIn, ws') = shiftLWithCarry ws n
        w'' = w' .|. carryIn
        cons = BSCons w'' ws'
     in (carryOut, cons)
  {-# INLINE shiftLWithCarry #-}
  {-# SPECIALISE shiftLWithCarry :: BS Word64 (HackL 128) -> Int -> (Word64, BS Word64 (HackL 128))  #-}
  {-# SPECIALISE shiftLWithCarry :: BS Word64 (HackL 192) -> Int -> (Word64, BS Word64 (HackL 192))  #-}
  {-# SPECIALISE shiftLWithCarry :: BS Word64 (HackL 256) -> Int -> (Word64, BS Word64 (HackL 256))  #-}
  {-# SPECIALISE shiftLWithCarry :: BS Word64 (HackL 320) -> Int -> (Word64, BS Word64 (HackL 320))  #-}
  {-# SPECIALISE shiftLWithCarry :: BS Word64 (HackL 384) -> Int -> (Word64, BS Word64 (HackL 384))  #-}
  {-# SPECIALISE shiftLWithCarry :: BS Word64 (HackL 448) -> Int -> (Word64, BS Word64 (HackL 448))  #-}
  {-# SPECIALISE shiftLWithCarry :: BS Word64 (HackL 512) -> Int -> (Word64, BS Word64 (HackL 512))  #-}
  {-# SPECIALISE shiftLWithCarry :: BS Word64 (HackL 576) -> Int -> (Word64, BS Word64 (HackL 576))  #-}
  {-# SPECIALISE shiftLWithCarry :: BS Word64 (HackL 640) -> Int -> (Word64, BS Word64 (HackL 640))  #-}
  {-# SPECIALISE shiftLWithCarry :: BS Word64 (HackL 704) -> Int -> (Word64, BS Word64 (HackL 704))  #-}
  {-# SPECIALISE shiftLWithCarry :: BS Word64 (HackL 768) -> Int -> (Word64, BS Word64 (HackL 768))  #-}
  {-# SPECIALISE shiftLWithCarry :: BS Word64 (HackL 832) -> Int -> (Word64, BS Word64 (HackL 832))  #-}
  {-# SPECIALISE shiftLWithCarry :: BS Word64 (HackL 896) -> Int -> (Word64, BS Word64 (HackL 896))  #-}
  {-# SPECIALISE shiftLWithCarry :: BS Word64 (HackL 960) -> Int -> (Word64, BS Word64 (HackL 960))  #-}
  {-# SPECIALISE shiftLWithCarry :: BS Word64 (HackL 1024) -> Int -> (Word64, BS Word64 (HackL 1024))  #-}
  {-# SPECIALISE shiftLWithCarry :: BS Word64 (HackL 1088) -> Int -> (Word64, BS Word64 (HackL 1088))  #-}
  {-# SPECIALISE shiftLWithCarry :: BS Word64 (HackL 1152) -> Int -> (Word64, BS Word64 (HackL 1152))  #-}
  {-# SPECIALISE shiftLWithCarry :: BS Word64 (HackL 1216) -> Int -> (Word64, BS Word64 (HackL 1216))  #-}
  {-# SPECIALISE shiftLWithCarry :: BS Word64 (HackL 1280) -> Int -> (Word64, BS Word64 (HackL 1280))  #-}
  {-# SPECIALISE shiftLWithCarry :: BS Word64 (HackL 1344) -> Int -> (Word64, BS Word64 (HackL 1344))  #-}
  {-# SPECIALISE shiftLWithCarry :: BS Word64 (HackL 1408) -> Int -> (Word64, BS Word64 (HackL 1408))  #-}
  {-# SPECIALISE shiftLWithCarry :: BS Word64 (HackL 1472) -> Int -> (Word64, BS Word64 (HackL 1472))  #-}
  {-# SPECIALISE shiftLWithCarry :: BS Word64 (HackL 1536) -> Int -> (Word64, BS Word64 (HackL 1536))  #-}
  {-# SPECIALISE shiftLWithCarry :: BS Word64 (HackL 1600) -> Int -> (Word64, BS Word64 (HackL 1600))  #-}
  {-# SPECIALISE shiftLWithCarry :: BS Word64 (HackL 1664) -> Int -> (Word64, BS Word64 (HackL 1664))  #-}
  {-# SPECIALISE shiftLWithCarry :: BS Word64 (HackL 1728) -> Int -> (Word64, BS Word64 (HackL 1728))  #-}
  {-# SPECIALISE shiftLWithCarry :: BS Word64 (HackL 1792) -> Int -> (Word64, BS Word64 (HackL 1792))  #-}
  {-# SPECIALISE shiftLWithCarry :: BS Word64 (HackL 1856) -> Int -> (Word64, BS Word64 (HackL 1856))  #-}
  {-# SPECIALISE shiftLWithCarry :: BS Word64 (HackL 1903) -> Int -> (Word64, BS Word64 (HackL 1903))  #-}

  shiftRInit bs@(BSCons w ws) n
    | n < 0 = throw Overflow
    | n == 0 = bs
    | n >= (bitsetWidth @w @(Left n)) = empty
    | n >= (wordWidth @Int @w) =
      let (nWords, nBits) = n `quotRem` (wordWidth @Int @w)
          bs' = shiftRWholeWords bs nWords
       in shiftRInit bs' nBits
    | n < (wordWidth @Int @w) = shiftRWithCarry bs n zeroBits
  {-# INLINE shiftRInit #-}
  {-# SPECIALISE shiftRInit :: BS Word64 (HackL 1903) -> Int -> BS Word64 (HackL 1903)  #-}

  shiftRWholeWords bs 0 = bs
  shiftRWholeWords bs@(BSCons w ws) n =
    let ws' = shiftRWholeWords ws n
        ws'' = setWordN (n - 1) w ws'
        w' = zeroBits
     in BSCons w' ws''
  {-# INLINE shiftRWholeWords #-}
  {-# SPECIALISE shiftRWholeWords :: BS Word64 (HackL 128) -> Int -> BS Word64 (HackL 128)  #-}
  {-# SPECIALISE shiftRWholeWords :: BS Word64 (HackL 192) -> Int -> BS Word64 (HackL 192)  #-}
  {-# SPECIALISE shiftRWholeWords :: BS Word64 (HackL 256) -> Int -> BS Word64 (HackL 256)  #-}
  {-# SPECIALISE shiftRWholeWords :: BS Word64 (HackL 320) -> Int -> BS Word64 (HackL 320)  #-}
  {-# SPECIALISE shiftRWholeWords :: BS Word64 (HackL 384) -> Int -> BS Word64 (HackL 384)  #-}
  {-# SPECIALISE shiftRWholeWords :: BS Word64 (HackL 448) -> Int -> BS Word64 (HackL 448)  #-}
  {-# SPECIALISE shiftRWholeWords :: BS Word64 (HackL 512) -> Int -> BS Word64 (HackL 512)  #-}
  {-# SPECIALISE shiftRWholeWords :: BS Word64 (HackL 576) -> Int -> BS Word64 (HackL 576)  #-}
  {-# SPECIALISE shiftRWholeWords :: BS Word64 (HackL 640) -> Int -> BS Word64 (HackL 640)  #-}
  {-# SPECIALISE shiftRWholeWords :: BS Word64 (HackL 704) -> Int -> BS Word64 (HackL 704)  #-}
  {-# SPECIALISE shiftRWholeWords :: BS Word64 (HackL 768) -> Int -> BS Word64 (HackL 768)  #-}
  {-# SPECIALISE shiftRWholeWords :: BS Word64 (HackL 832) -> Int -> BS Word64 (HackL 832)  #-}
  {-# SPECIALISE shiftRWholeWords :: BS Word64 (HackL 896) -> Int -> BS Word64 (HackL 896)  #-}
  {-# SPECIALISE shiftRWholeWords :: BS Word64 (HackL 960) -> Int -> BS Word64 (HackL 960)  #-}
  {-# SPECIALISE shiftRWholeWords :: BS Word64 (HackL 1024) -> Int -> BS Word64 (HackL 1024)  #-}
  {-# SPECIALISE shiftRWholeWords :: BS Word64 (HackL 1088) -> Int -> BS Word64 (HackL 1088)  #-}
  {-# SPECIALISE shiftRWholeWords :: BS Word64 (HackL 1152) -> Int -> BS Word64 (HackL 1152)  #-}
  {-# SPECIALISE shiftRWholeWords :: BS Word64 (HackL 1216) -> Int -> BS Word64 (HackL 1216)  #-}
  {-# SPECIALISE shiftRWholeWords :: BS Word64 (HackL 1280) -> Int -> BS Word64 (HackL 1280)  #-}
  {-# SPECIALISE shiftRWholeWords :: BS Word64 (HackL 1344) -> Int -> BS Word64 (HackL 1344)  #-}
  {-# SPECIALISE shiftRWholeWords :: BS Word64 (HackL 1408) -> Int -> BS Word64 (HackL 1408)  #-}
  {-# SPECIALISE shiftRWholeWords :: BS Word64 (HackL 1472) -> Int -> BS Word64 (HackL 1472)  #-}
  {-# SPECIALISE shiftRWholeWords :: BS Word64 (HackL 1536) -> Int -> BS Word64 (HackL 1536)  #-}
  {-# SPECIALISE shiftRWholeWords :: BS Word64 (HackL 1600) -> Int -> BS Word64 (HackL 1600)  #-}
  {-# SPECIALISE shiftRWholeWords :: BS Word64 (HackL 1664) -> Int -> BS Word64 (HackL 1664)  #-}
  {-# SPECIALISE shiftRWholeWords :: BS Word64 (HackL 1728) -> Int -> BS Word64 (HackL 1728)  #-}
  {-# SPECIALISE shiftRWholeWords :: BS Word64 (HackL 1792) -> Int -> BS Word64 (HackL 1792)  #-}
  {-# SPECIALISE shiftRWholeWords :: BS Word64 (HackL 1856) -> Int -> BS Word64 (HackL 1856)  #-}
  {-# SPECIALISE shiftRWholeWords :: BS Word64 (HackL 1903) -> Int -> BS Word64 (HackL 1903)  #-}

  shiftRWithCarry bs@(BSCons w ws) n carryIn =
    let (w', carryOut) = shiftRCarry w n
        w'' = w' .|. carryIn
        ws' = shiftRWithCarry ws n carryOut
     in BSCons w'' ws'
  {-# INLINE shiftRWithCarry #-}
  {-# SPECIALISE shiftRWithCarry :: BS Word64 (HackL 128) -> Int -> Word64 -> BS Word64 (HackL 128)  #-}
  {-# SPECIALISE shiftRWithCarry :: BS Word64 (HackL 192) -> Int -> Word64 -> BS Word64 (HackL 192)  #-}
  {-# SPECIALISE shiftRWithCarry :: BS Word64 (HackL 256) -> Int -> Word64 -> BS Word64 (HackL 256)  #-}
  {-# SPECIALISE shiftRWithCarry :: BS Word64 (HackL 320) -> Int -> Word64 -> BS Word64 (HackL 320)  #-}
  {-# SPECIALISE shiftRWithCarry :: BS Word64 (HackL 384) -> Int -> Word64 -> BS Word64 (HackL 384)  #-}
  {-# SPECIALISE shiftRWithCarry :: BS Word64 (HackL 448) -> Int -> Word64 -> BS Word64 (HackL 448)  #-}
  {-# SPECIALISE shiftRWithCarry :: BS Word64 (HackL 512) -> Int -> Word64 -> BS Word64 (HackL 512)  #-}
  {-# SPECIALISE shiftRWithCarry :: BS Word64 (HackL 576) -> Int -> Word64 -> BS Word64 (HackL 576)  #-}
  {-# SPECIALISE shiftRWithCarry :: BS Word64 (HackL 640) -> Int -> Word64 -> BS Word64 (HackL 640)  #-}
  {-# SPECIALISE shiftRWithCarry :: BS Word64 (HackL 704) -> Int -> Word64 -> BS Word64 (HackL 704)  #-}
  {-# SPECIALISE shiftRWithCarry :: BS Word64 (HackL 768) -> Int -> Word64 -> BS Word64 (HackL 768)  #-}
  {-# SPECIALISE shiftRWithCarry :: BS Word64 (HackL 832) -> Int -> Word64 -> BS Word64 (HackL 832)  #-}
  {-# SPECIALISE shiftRWithCarry :: BS Word64 (HackL 896) -> Int -> Word64 -> BS Word64 (HackL 896)  #-}
  {-# SPECIALISE shiftRWithCarry :: BS Word64 (HackL 960) -> Int -> Word64 -> BS Word64 (HackL 960)  #-}
  {-# SPECIALISE shiftRWithCarry :: BS Word64 (HackL 1024) -> Int -> Word64 -> BS Word64 (HackL 1024)  #-}
  {-# SPECIALISE shiftRWithCarry :: BS Word64 (HackL 1088) -> Int -> Word64 -> BS Word64 (HackL 1088)  #-}
  {-# SPECIALISE shiftRWithCarry :: BS Word64 (HackL 1152) -> Int -> Word64 -> BS Word64 (HackL 1152)  #-}
  {-# SPECIALISE shiftRWithCarry :: BS Word64 (HackL 1216) -> Int -> Word64 -> BS Word64 (HackL 1216)  #-}
  {-# SPECIALISE shiftRWithCarry :: BS Word64 (HackL 1280) -> Int -> Word64 -> BS Word64 (HackL 1280)  #-}
  {-# SPECIALISE shiftRWithCarry :: BS Word64 (HackL 1344) -> Int -> Word64 -> BS Word64 (HackL 1344)  #-}
  {-# SPECIALISE shiftRWithCarry :: BS Word64 (HackL 1408) -> Int -> Word64 -> BS Word64 (HackL 1408)  #-}
  {-# SPECIALISE shiftRWithCarry :: BS Word64 (HackL 1472) -> Int -> Word64 -> BS Word64 (HackL 1472)  #-}
  {-# SPECIALISE shiftRWithCarry :: BS Word64 (HackL 1536) -> Int -> Word64 -> BS Word64 (HackL 1536)  #-}
  {-# SPECIALISE shiftRWithCarry :: BS Word64 (HackL 1600) -> Int -> Word64 -> BS Word64 (HackL 1600)  #-}
  {-# SPECIALISE shiftRWithCarry :: BS Word64 (HackL 1664) -> Int -> Word64 -> BS Word64 (HackL 1664)  #-}
  {-# SPECIALISE shiftRWithCarry :: BS Word64 (HackL 1728) -> Int -> Word64 -> BS Word64 (HackL 1728)  #-}
  {-# SPECIALISE shiftRWithCarry :: BS Word64 (HackL 1792) -> Int -> Word64 -> BS Word64 (HackL 1792)  #-}
  {-# SPECIALISE shiftRWithCarry :: BS Word64 (HackL 1856) -> Int -> Word64 -> BS Word64 (HackL 1856)  #-}
  {-# SPECIALISE shiftRWithCarry :: BS Word64 (HackL 1903) -> Int -> Word64 -> BS Word64 (HackL 1903)  #-}

  mapWords f (BSCons w ws) = BSCons (f w .&. (wordMask @w @(Left n))) $ mapWords' f ws
  {-# INLINE mapWords #-}
  {-# SPECIALISE mapWords :: (Word64 -> Word64) -> BS Word64 (HackL 1903) -> BS Word64 (HackL 1903)  #-}

  mapWords' f (BSCons w ws) = BSCons (f w) $ mapWords' f ws
  {-# INLINE mapWords' #-}
  {-# SPECIALISE mapWords' :: (Word64 -> Word64) -> BS Word64 (HackL 1903) -> BS Word64 (HackL 1903)  #-}

  zipWordsWith op (BSCons w ws) (BSCons v vs) = BSCons (w `op` v) $ zipWordsWith op ws vs
  {-# INLINE zipWordsWith #-}
  {-# SPECIALISE zipWordsWith :: (Word64 -> Word64 -> Word64) -> BS Word64 (HackL 1903) -> BS Word64 (HackL 1903) -> BS Word64 (HackL 1903)  #-}

instance (Eq w) => Eq (BS w (Right (n :: Nat))) where
  (BSEnd as) == (BSEnd bs) = as == bs

instance (Eq w, Eq (BS w (BSN w m))) => Eq (BS w (Left (m :: Nat))) where
  (BSCons a as) == (BSCons b bs) = a == b && (as == bs)

instance (Integral w, Show w) => Show (BS w (Right (n :: Nat))) where
  showsPrec _ (BSEnd w) = showHex w

instance (Integral w, Show w, Show (BS w (BSN w n))) => Show (BS w (Left (n :: Nat))) where
  showsPrec d (BSCons w ws) = showHex w . (',' :) . showsPrec d ws

shiftLCarry :: forall a. (Bits a, Integral a, KnownNat (WordWidth a)) => a -> Int -> (a, a)
shiftLCarry word n =
  let carryOffset = (wordWidth @Int @a) - n
      mask = ((1 `shiftL` n) - 1) `shiftL` carryOffset
      carry = (word .&. mask) `shiftR` carryOffset
      word' = word `shiftL` n
   in (carry, word')

shiftRCarry :: forall a. (Bits a, Integral a, KnownNat (WordWidth a)) => a -> Int -> (a, a)
shiftRCarry word n =
  let carryOffset = (wordWidth @Int @a) - n
      mask = (1 `shiftL` n) - 1
      carry = (word .&. mask) `shiftL` carryOffset
      word' = word `shiftR` n
   in (word', carry)

instance (Bits w, BitSet w n, Eq (BS w n)) => Bits (BS w n) where
  (.&.) = zipWordsWith (.&.)
  (.|.) = zipWordsWith (.|.)
  xor = zipWordsWith xor
  complement = mapWords complement
  shiftL = shiftLInit
  shiftR = shiftRInit
  bitSize _ = (bitsetWidth @w @n)
  bitSizeMaybe _ = Just (bitsetWidth @w @n)
  isSigned = const False
  testBit = bsTestBit
  bit = singleBit
  popCount = bsPopCount

  rotateL bs i =
    let n = bitsetWidth @w @n
        j = i `mod` n
        lbs = bs `shiftL` j
        rbs = bs `shiftR` (n - j)
     in lbs .|. rbs

  rotateR bs i =
    let n = bitsetWidth @w @n
        j = i `mod` n
        rbs = bs `shiftR` j
        lbs = bs `shiftL` (n - j)
     in rbs .|. lbs

type BitSetW w (n :: Nat) = BS w (FirstLength' w n)

type BitSet64 (n :: Nat) = BS Word64 (FirstLength' Word64 n)

{-# SPECIALISE shiftLInit :: BitSet64 1903 -> Int -> BitSet64 1903 #-}
{-# SPECIALISE bitsetWidth :: Int #-}
{-# SPECIALISE bsPopCount  :: BitSet64 1903 #-}
{-# SPECIALISE bsTestBit  :: BitSet64 1903 -> Int -> BitSet64 1903 #-}
{-# SPECIALISE empty  :: BitSet64 1903 -> Int -> BitSet64 1903 #-}
{-# SPECIALISE lowerBitIndex :: BitSet64 1903 -> Int -> BitSet64 1903  #-}
{-# SPECIALISE mapWords :: BitSet64 1903 -> Int -> BitSet64 1903  #-}
{-# SPECIALISE mapWords' :: BitSet64 1903 -> Int -> BitSet64 1903  #-}
{-# SPECIALISE shiftLInit :: BitSet64 1903 -> Int -> BitSet64 1903  #-}
{-# SPECIALISE shiftLWholeWords :: BitSet64 1903 -> Int -> BitSet64 1903  #-}
{-# SPECIALISE shiftLWithCarry :: BitSet64 1903 -> Int -> BitSet64 1903  #-}
{-# SPECIALISE shiftRInit :: BitSet64 1903 -> Int -> BitSet64 1903  #-}
{-# SPECIALISE shiftRWholeWords :: BitSet64 1903 -> Int -> BitSet64 1903  #-}
{-# SPECIALISE shiftRWithCarry :: BitSet64 1903 -> Int -> BitSet64 1903  #-}
{-# SPECIALISE singleBit :: BitSet64 1903 -> Int -> BitSet64 1903  #-}
{-# SPECIALISE upperBitIndex :: BitSet64 1903 -> Int -> BitSet64 1903  #-}
{-# SPECIALISE word :: BitSet64 1903 -> Int -> BitSet64 1903  #-}
{-# SPECIALISE wordMask :: BitSet64 1903 -> Int -> BitSet64 1903  #-}
{-# SPECIALISE zipWordsWith :: BitSet64 1903 -> Int -> BitSet64 1903  #-}
