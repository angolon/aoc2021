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

import Control.Arrow
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Loops as Loops
import qualified Control.Monad.State.Lazy as S
import Data.Bifunctor.Swap (swap)
import Data.Bits
import Data.Either (either)
import Data.Foldable
import Data.Function (on)
import Data.Kind
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Monoidal as MMap
import Data.Map.Strict (Map, (!), (!?))
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Monoid (Endo (..), Product (..), Sum (..), getSum)
import Data.Ord (Down (..))
import Data.PQueue.Prio.Max (MaxPQueue)
import qualified Data.PQueue.Prio.Max as MaxPQueue
import Data.Proxy
import Data.Ratio
import Data.Set (Set (..), union)
import qualified Data.Set as Set
import Data.Type.Equality
import qualified Data.Vector as V
import Debug.Trace
import GHC.Exts (Constraint)
import GHC.TypeLits
import GHC.Word (Word16, Word32, Word64, Word8)
import Lib (MyParser, parseInt, parseStdin)
import Numeric (showHex)
import Text.Parsec
import Text.Parsec.Char
import Text.Show.Functions

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
  lowerBitIndex = 0

  singleBit i
    | 0 <= i && i < (upperBitIndex @w @(Right n)) = BSEnd $ bit i
    | otherwise = throw Overflow

  bsTestBit (BSEnd w) i
    | 0 <= i && i < (upperBitIndex @w @(Right n)) = testBit w i
    | otherwise = throw Overflow

  bsPopCount (BSEnd w) = popCount w

  word (BSEnd w) = w

  wordMask = (1 `shiftL` (upperBitIndex @w @(Right n))) - 1

  wordN 0 = word
  wordN _ = const zeroBits

  setWordN 0 w = const $ BSEnd w
  setWordN _ _ = id

  wordL = word
  wordR = word

  shiftLWholeWords bs 0 = bs
  shiftLWholeWords _ _ = BSEnd zeroBits

  shiftLWithCarry (BSEnd w) n =
    let (carryOut, w') = shiftLCarry w n
     in (carryOut,) $ BSEnd w'

  shiftRWholeWords bs 0 = bs
  shiftRWholeWords _ _ = BSEnd zeroBits

  shiftRWithCarry (BSEnd w) n carryIn =
    let w' = w `shiftR` n
        w'' = carryIn .|. w'
     in BSEnd w''

  mapWords f (BSEnd w) = BSEnd . (.&. (wordMask @w @(Right n))) . f $ w
  mapWords' f (BSEnd w) = BSEnd . f $ w

  zipWordsWith op (BSEnd w) (BSEnd v) = BSEnd $ w `op` v

  -- If BSEnd contains the first and only word, we can defer to the vanilla
  -- `shiftL` and `shiftR` operations.
  shiftLInit (BSEnd w) n = BSEnd $ w `shiftL` n

  shiftRInit (BSEnd w) n = BSEnd $ w `shiftR` n

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

  upperBitIndex = fromIntegral (natVal (Proxy :: Proxy (n)))
  lowerBitIndex = upperBitIndex @w @(BSN w n)

  empty = BSCons zeroBits empty

  singleBit i
    | i >= upperBitIndex @w @(Left n) = throw Overflow
    | i < lowerBitIndex @w @(Left n) = BSCons zeroBits $ singleBit i
    | otherwise =
      let i' = i - (lowerBitIndex @w @(Left n))
       in BSCons (bit i') empty

  bsTestBit (BSCons w ws) i
    | i >= upperBitIndex @w @(Left n) = throw Overflow
    | i < lowerBitIndex @w @(Left n) = bsTestBit ws i
    | otherwise =
      let i' = i - (lowerBitIndex @w @(Left n))
       in testBit w i'

  bsPopCount (BSCons w ws) = popCount w + bsPopCount ws

  word (BSCons word _) = word

  wordMask = (1 `shiftL` ((upperBitIndex @w @(Left n)) - (lowerBitIndex @w @(Left n)))) - 1

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
          bs' = shiftLWholeWords bs nWords
       in shiftLInit bs' nBits
    | n < (wordWidth @Int @w) =
      let (carry, ws') = shiftLWithCarry ws n
          w' = (w `shiftL` n) .|. carry
       in BSCons w' ws'

  shiftLWholeWords bs 0 = bs
  shiftLWholeWords bs@(BSCons w ws) n =
    let w' = wordN n bs
        ws' = shiftLWholeWords ws n
     in BSCons w' ws'

  shiftLWithCarry (BSCons w ws) n =
    let (carryOut, w') = shiftLCarry w n
        (carryIn, ws') = shiftLWithCarry ws n
        w'' = w' .|. carryIn
        cons = BSCons w'' ws'
     in (carryOut, cons)

  shiftRInit bs@(BSCons w ws) n
    | n < 0 = throw Overflow
    | n == 0 = bs
    | n >= (bitsetWidth @w @(Left n)) = empty
    | n >= (wordWidth @Int @w) =
      let (nWords, nBits) = n `quotRem` (wordWidth @Int @w)
          bs' = shiftRWholeWords bs nWords
       in shiftRInit bs' nBits
    | n < (wordWidth @Int @w) = shiftRWithCarry bs n zeroBits

  shiftRWholeWords bs 0 = bs
  shiftRWholeWords bs@(BSCons w ws) n =
    let ws' = shiftRWholeWords ws n
        ws'' = setWordN (n - 1) w ws'
        w' = zeroBits
     in BSCons w' ws''

  shiftRWithCarry bs@(BSCons w ws) n carryIn =
    let (w', carryOut) = shiftRCarry w n
        w'' = w' .|. carryIn
        ws' = shiftRWithCarry ws n carryOut
     in BSCons w'' ws'

  mapWords f (BSCons w ws) = BSCons (f w .&. (wordMask @w @(Left n))) $ mapWords' f ws
  mapWords' f (BSCons w ws) = BSCons (f w) $ mapWords' f ws

  zipWordsWith op (BSCons w ws) (BSCons v vs) = BSCons (w `op` v) $ zipWordsWith op ws vs

instance (Eq w) => Eq (BS w (Right (n :: Nat))) where
  (BSEnd as) == (BSEnd bs) = as == bs

instance (Eq w, Eq (BS w (BSN w m))) => Eq (BS w (Left (m :: Nat))) where
  (BSCons a as) == (BSCons b bs) = a == b && (as == bs)

instance (Integral w, Show w) => Show (BS w (Right (n :: Nat))) where
  showsPrec _ (BSEnd w) = showHex w

instance (Integral w, Show w, Show (BS w (BSN w n))) => Show (BS w (Left (n :: Nat))) where
  showsPrec d (BSCons w ws) = showHex w . showsPrec d ws

-- rotateL :: BS n -> Int -> w -> w -> BS n
-- rotateR :: BS n -> Int -> w -> w -> BS n

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
  rotate = undefined
  testBit = bsTestBit
  bit = singleBit
  popCount = bsPopCount

type BitSetW w (n :: Nat) = BS w (FirstLength' w n)

type BitSet64 (n :: Nat) = BS Word64 (FirstLength' Word64 n)
