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

module Day25 where

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
import Data.WideWord.Word256
import Debug.Trace
import GHC.Exts (Constraint)
import GHC.TypeLits
import GHC.Word (Word8)
import Lib (MyParser, parseInt, parseStdin)
import Text.Parsec
import Text.Parsec.Char
import Text.Show.Functions

type WordT = Word256

type Width = 139

type Height = 137

type WordWidth = 256

type NBits = Width * Height

type Remainder = Mod NBits WordWidth

type Con = CmpNat Remainder 0

type family Extra a where
  Extra 'GT = 1
  Extra _ = 0

type NWords = (Div NBits WordWidth) + (Extra Con)

type CmpWidth n = CmpNat n WordWidth

type family MoreWords cmp n where
  MoreWords 'GT n = Left (n - 256)
  MoreWords _ _ = Right ()

type BSN (n :: Nat) = MoreWords (CmpWidth n) n

class BitSet n where
  data BS n

  showBS :: BS n -> String

  empty :: BS n

type BitSetN (n :: Nat) = BitSet (BSN n)

instance BitSet (Right ()) where
  data BS (Right ()) = BSEnd WordT

  showBS (BSEnd bits) = showHexWord256 bits

  empty = BSEnd 0

instance (BitSetN n) => BitSet (Left (n :: Nat)) where
  data BS (Left n) = BSCons WordT (BS (BSN n))
  showBS (BSCons bits words) = showHexWord256 bits ++ ", " ++ showBS words
  empty = BSCons 0 empty

instance Eq (BS (Right ())) where
  (BSEnd as) == (BSEnd bs) = as == bs

instance (Eq (BS (BSN m))) => Eq (BS (Left (m :: Nat))) where
  (BSCons a as) == (BSCons b bs) = a == b && (as == bs)

class (BitSet n) => ShiftHelper n where
  -- The word at this index
  word :: BS n -> WordT

  -- The word at the n'th index
  -- All zeros if the index is past the end of the bitset
  wordN :: Int -> BS n -> WordT

  -- Leftmost word in bitset
  wordL :: BS n -> WordT

  -- Rightmost word in bitset
  wordR :: BS n -> WordT

  setWordN :: Int -> WordT -> BS n -> BS n

  shiftRWithCarry :: BS n -> Int -> WordT -> BS n
  shiftRWholeWords :: BS n -> Int -> BS n
  shiftRInit :: BS n -> Int -> BS n

  -- (Carry, result)
  shiftLWithCarry :: BS n -> Int -> (WordT, BS n)
  shiftLWholeWords :: BS n -> Int -> BS n
  shiftLInit :: BS n -> Int -> BS n

  mapWords :: (WordT -> WordT) -> BS n -> BS n
  zipWordsWith :: (WordT -> WordT -> WordT) -> BS n -> BS n -> BS n

-- rotateL :: BS n -> Int -> WordT -> WordT -> BS n
-- rotateR :: BS n -> Int -> WordT -> WordT -> BS n

type ShiftHelperN (n :: Nat) = ShiftHelper (BSN n)

wordWidth :: (Integral a) => a
wordWidth = fromInteger $ natVal (Proxy :: Proxy WordWidth)

bitsetWidth :: (Integral a) => a
bitsetWidth = fromInteger $ natVal (Proxy :: Proxy NBits)

shiftLCarry :: WordT -> Int -> (WordT, WordT)
shiftLCarry word n =
  let carryOffset = wordWidth - n
      mask = ((1 `shiftL` n) - 1) `shiftL` carryOffset
      carry = (word .&. mask) `shiftR` carryOffset
      word' = word `shiftL` n
   in (carry, word')

shiftRCarry :: WordT -> Int -> (WordT, WordT)
shiftRCarry word n =
  let carryOffset = wordWidth - n
      mask = (1 `shiftL` n) - 1
      carry = (word .&. mask) `shiftL` carryOffset
      word' = word `shiftR` n
   in (word', carry)

instance (BitSetN n, ShiftHelperN n) => ShiftHelper (Left (n :: Nat)) where
  word (BSCons word _) = word

  wordN 0 bs = word bs
  wordN n (BSCons _ ws) = wordN (n - 1) ws

  setWordN 0 w (BSCons _ ws) = BSCons w ws
  setWordN n v (BSCons w ws) = BSCons w $ setWordN (n - 1) v ws

  wordL (BSCons word _) = word

  wordR (BSCons _ words) = wordR words

  shiftLInit bs@(BSCons w ws) n
    | n < 0 = throw Overflow
    | n == 0 = bs
    | n >= bitsetWidth = empty
    | n >= wordWidth =
      let (nWords, nBits) = n `quotRem` wordWidth
          bs' = shiftLWholeWords bs nWords
       in shiftLInit bs' nBits
    | n < wordWidth =
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
    | n >= bitsetWidth = empty
    | n >= wordWidth =
      let (nWords, nBits) = n `quotRem` wordWidth
          bs' = shiftRWholeWords bs nWords
       in shiftRInit bs' nBits
    | n < wordWidth = shiftRWithCarry bs n zeroBits

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

  mapWords f (BSCons w ws) = BSCons (f w) $ mapWords f ws
  zipWordsWith op (BSCons w ws) (BSCons v vs) = BSCons (w `op` v) $ zipWordsWith op ws vs

instance ShiftHelper (Right ()) where
  word (BSEnd w) = w

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

  mapWords f (BSEnd w) = BSEnd . f $ w
  zipWordsWith op (BSEnd w) (BSEnd v) = BSEnd $ w `op` v

  -- If BSEnd contains the first and only word, we can defer to the vanilla
  -- `shiftL` and `shiftR` operations.
  shiftLInit (BSEnd w) n = BSEnd $ w `shiftL` n

  shiftRInit (BSEnd w) n = BSEnd $ w `shiftR` n

instance (Eq (BS n), ShiftHelper n) => Bits (BS n) where
  (.&.) = zipWordsWith (.&.)
  (.|.) = zipWordsWith (.|.)
  xor = zipWordsWith xor
  complement = mapWords complement
  shiftL = shiftLInit
  shiftR = shiftRInit
  bitSize _ = bitsetWidth
  bitSizeMaybe _ = Just bitsetWidth
  isSigned = const False
  rotate = undefined
  testBit = undefined
  bit = undefined
  popCount = undefined

-- instance Bits (BS (Right ())) where
--   (BSEnd as) .&. (BSEnd bs) = BSEnd (as .&. bs)
--   (BSEnd as) .|. (BSEnd bs) = BSEnd (as .|. bs)
--   xor (BSEnd as) (BSEnd bs) = BSEnd (xor as bs)
--   complement (BSEnd as) = BSEnd (complement as)

data Lane = Lane {_occupied :: WordT}

-- data family BitSet (n :: Nat)

-- data instance BitSet n where

-- instance (BitSetN n) => Bits (BS n) where
-- bloop
-- bloop :: BS @(BSN 512)
-- bloop = BSCons 0xFF

blah =
  let xs = empty @(BSN 512)
      ys = empty @(BSN 512)
   in xs == ys

cucumberDance :: IO ()
cucumberDance = do
  print $ natVal (Proxy :: Proxy NBits)
  print $ natVal (Proxy :: Proxy NWords)
  print . showBS $ empty @(BSN 257)

-- print . showBS $ BSCons @(BSN 512) 0xFFF $ BSEnd 0xFF
