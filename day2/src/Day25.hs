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

module Day25 where

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

type family ToZero a where
  ToZero 0 = 0
  ToZero n = ToZero (n - 1)

type family Steps (a :: Nat) where
  Steps 0 = '[0]
  -- (n <= 256) => Steps n = '[0]
  Steps n = n : Steps (n - 1)

class BitSet (n :: [Nat]) where
  data BS n
  -- empty :: BS n
  -- testBit :: Int -> bs -> Bool

-- instance BitSet n where
--   data BS n = BSNil Word256 | BSCons Word256 (BS (n - 256))
--   empty =
--     if natVal (Proxy :: Proxy n) <= 256
--       then BSNil 0
--       else BSCons 0 (empty @(n - 256))

-- instance (n <= 256, 257 <= m, s ~ (n + m)) => BitSet s where
--   data BS s = Bloop Word256

instance BitSet '[0] where
  data BS '[0] = BSNil Word256

instance (n ~ (m:ns)) => BitSet n where
  data BS n = BSCons Word256 (BS '[0])

-- instance (BitSet n c, d ~ (m ~ (n + 256))) => BitSet m d where
-- instance (BitSet n c, d ~ (n <= m)) => BitSet m d where

data Lane = Lane {_occupied :: Word256}

-- class Foo (n :: Nat) where
--   foo :: String

-- instance Foo 7 where
--   foo = "lol"

-- instance Foo 8 where
--   foo = "bloop"

-- data family BitSet (n :: Nat)

-- data instance BitSet n where

cucumberDance :: IO ()
cucumberDance = do
  print $ natVal (Proxy :: Proxy NBits)
  print $ natVal (Proxy :: Proxy NWords)
  print $ natVal (Proxy :: Proxy (ToZero NWords))
