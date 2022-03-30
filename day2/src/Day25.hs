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

import BitSet
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
import GHC.Word (Word64, Word8)
import Lib (MyParser, parseInt, parseStdin)
import Text.Parsec
import Text.Parsec.Char
import Text.Show.Functions

type WordT = Word64

type Width = 139

type Height = 137

data Lane = Lane {_occupied :: WordT}

-- data family BitSet (n :: Nat)

-- data instance BitSet n where

-- instance (BitSetN n) => Bits (BS n) where
-- bloop
-- bloop :: BS @(BSN 512)
-- bloop = BSCons 0xFF

-- blah =
--   let xs = empty @(BSN Word64 512)
--       ys = empty @(BSN Word64 512)
--    in xs == ys

blah :: BS Word64 (Left 257)
blah =
  let a = bit 256 :: BS Word64 (Left 257)
      b = bit 0 :: BS Word64 (Left 257)
      c = bit 1 :: BS Word64 (Left 257)
      d = bit 64 :: BS Word64 (Left 257)
      e = bit 128 :: BS Word64 (Left 257)
      f = bit 192 :: BS Word64 (Left 257)
   in a .|. b .|. c .|. d .|. e .|. f

blag :: Int
blag = lowerBitIndex @Word64 @(Left 257)

bloop :: BitSet64 1903
bloop = bit 1 `shiftL` 1832

cucumberDance :: IO ()
cucumberDance = do
  -- print $ empty @(BitSet Word64 257)
  print $ (empty :: BS Word64 (BSN Word64 257))
  print $ bitsetWidth @Word64 @(Left 257)
  print blah
  print blag
  print $ (empty :: BS Word64 (Right 64))
  print bloop

-- print . showBS $ BSCons @(BSN 512) 0xFFF $ BSEnd 0xFF
