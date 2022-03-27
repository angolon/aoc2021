{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}

module Day25 where

import Data.Bits
import GHC.Exts (Constraint)
import Control.Arrow
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Loops as Loops
import qualified Control.Monad.State.Lazy as S
import Data.Bifunctor.Swap (swap)
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
import qualified Data.Vector as V
import Data.WideWord.Word256
import Debug.Trace
import GHC.TypeLits
import GHC.Word (Word8)
import Lib (MyParser, parseInt, parseStdin)
import Text.Parsec
import Text.Parsec.Char
import Text.Show.Functions
import Data.Type.Equality

type Width = 139

type Height = 137

type BitWidth = 256

type NBits = Width * Height

type Remainder = Mod NBits BitWidth

type Con = CmpNat Remainder 0

type family Extra a where
  Extra 'GT = 1
  Extra _ = 0

type NWords = (Div NBits BitWidth) + (Extra Con)

type CmpWidth n = CmpNat n BitWidth

type family MoreWords cmp n where
  MoreWords 'GT n = Left (n - 256)
  MoreWords _ _ = Right ()

type BSN (n :: Nat) = MoreWords (CmpWidth n) n

class BitSet n where
  data family BS n

  showBS :: BS n -> String

  empty :: BS n

type BitSetN (n :: Nat) = BitSet (BSN n)

instance BitSet (Right ()) where
  data BS (Right ()) = BSNil Word256

  showBS (BSNil bits) = showHexWord256 bits

  empty = BSNil 0

instance (BitSetN n) => BitSet (Left (n :: Nat)) where
  data BS (Left n) = BSCons Word256 (BS (BSN n))
  showBS (BSCons bits words) = showHexWord256 bits ++ ", " ++ showBS words
  empty = BSCons 0 empty

instance Eq (BS (Right ())) where
  (BSNil as) == (BSNil bs) = as == bs
-- instance (n ~ 'Right()) => Eq (BS n) where
--   (BSNil as) == (BSNil bs) = as == bs
--   (BSNil as) /= (BSNil bs) = as /= bs

-- instance (n ~ Right(), BitSet n) => Eq (BS n) where
--   (BSNil as) == (BSNil bs) = as == bs
--   (BSNil as) /= (BSNil bs) = as /= bs

-- instance (BitSet n) => Eq (BS n) where
--   (BSNil as) == (BSNil bs) = as == bs
--   (BSCons a as) == (BSCons b bs) = a == b && as == bs
--   -- (BSNil as) /= (BSNil bs) = as /= bs

instance (Eq (BS (BSN m))) => Eq (BS (Left (m :: Nat))) where
  (BSCons a as) == (BSCons b bs) = a == b && (as == bs)

-- instance (n ~ Left m, BitSet n, BitSetN m, Eq (BS m)) => Eq (BS n) where
--   (BSCons a as) == (BSCons b bs) = a == b && (as == bs)

data Lane = Lane {_occupied :: Word256}

-- class Foo (n :: Nat) where
--   foo :: String

-- instance Foo 7 where
--   foo = "lol"

-- instance Foo 8 where
--   foo = "bloop"

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
  -- print . showBS $ BSCons @(BSN 512) 0xFFF $ BSNil 0xFF
