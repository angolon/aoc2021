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
{-# LANGUAGE StandaloneDeriving #-}
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
import Data.Vector ((!))
import qualified Data.Vector as V
import Data.WideWord.Word256
import Debug.Trace
import GHC.Exts (Constraint)
import GHC.TypeLits
import GHC.Word (Word16, Word32, Word64, Word8)
import Lib (MyParser, parseInt, parseStdin)
import Text.Parsec
import Text.Parsec.Char
import Text.Show.Functions

type WordT = Word16

type Width = 10

width :: (Integral a) => a
width = fromInteger $ natVal (Proxy :: Proxy Width)

type Height = 4

height :: (Integral a) => a
height = fromInteger $ natVal (Proxy :: Proxy Height)

newtype Eastwards = Eastwards (BitSetW WordT Width)

deriving instance (Eq (BitSetW WordT Width)) => Eq Eastwards

deriving instance (Bits (BitSetW WordT Width)) => Bits Eastwards

newtype Southwards = Southwards (BitSetW WordT Height)

deriving instance (Eq (BitSetW WordT Height)) => Eq Southwards

deriving instance (Bits (BitSetW WordT Height)) => Bits Southwards

data CucumberMap = CucumberMap
  { _east :: V.Vector Eastwards,
    _south :: V.Vector Southwards
  }
  deriving (Eq)

instance Show CucumberMap where
  show (CucumberMap eastwards southwards) = do
    i <- [0 .. (height - 1)]
    j <- [0 .. width]
    if
        | j == width -> return '\n'
        | (`testBit` (width - j - 1)) $ eastwards ! i -> return '>'
        | (`testBit` i) $ southwards ! j -> return 'v'
        | otherwise -> return '.'

makeLenses ''CucumberMap

emptyMap :: CucumberMap
emptyMap =
  let emptyE = V.fromList . replicate height $ zeroBits
      emptyS = V.fromList . replicate width $ zeroBits
   in CucumberMap emptyE emptyS

type CucumberParser = ParsecT String CucumberMap IO

parseMap :: CucumberParser CucumberMap
parseMap =
  let mapIndices = ((subtract 1 . sourceLine) &&& (subtract 2 . sourceColumn)) <$> getPosition
      setEast (i, j) m = m & east . traversed . index i %~ (`setBit` j)
      setSouth (i, j) m = m & south . traversed . index j %~ (`setBit` i)
      parseEast = do
        _ <- char '>'
        -- We need to mirror the horizontal bits so that our bitwise rotate/shift
        -- logic makes sense.
        (i, j) <- mapIndices
        let j' = width - j - 1
        modifyState $ setEast (i, j')
      parseSouth = do
        _ <- char 'v'
        ij <- mapIndices
        modifyState $ setSouth ij
      parseEmpty = char '.' *> pure ()
      parseCell = parseEast <|> parseSouth <|> parseEmpty
      parseRow = count width parseCell *> endOfLine
      parseLines = count height parseRow *> eof
   in parseLines *> getState

-- type Lane (n :: Nat) = BitSet64 n

newtype Lane n = Lane (BitSet64 n) -- deriving (Eq, Ord, Show, Bits)

deriving instance (Eq (BitSet64 n)) => Eq (Lane n)

deriving instance (Ord (BitSet64 n)) => Ord (Lane n)

deriving instance (Show (BitSet64 n)) => Show (Lane n)

deriving instance (Bits (BitSet64 n)) => Bits (Lane n)

slice ::
  forall a b.
  ( Bits a,
    Bits b
  ) =>
  Int ->
  V.Vector a ->
  b
slice i =
  let intersect :: b -> Int -> a -> b
      intersect bs idx as = if testBit as i then bs `setBit` idx else bs
   in V.ifoldl' intersect zeroBits

moveCucumbers :: (Bits b) => b -> b -> b
moveCucumbers lane cross =
  let notOccupied = complement (cross .|. lane)
      lane' = lane `rotateR` 1
      moved = lane' .&. notOccupied
      immovable = moved `xor` lane'
   in (immovable `rotateL` 1) .|. moved

moveEastwards :: CucumberMap -> CucumberMap
moveEastwards (CucumberMap eastwards southwards) =
  let eastwards' = do
        (i, e) <- V.indexed eastwards
        let overlaps = slice i southwards
        return $ moveCucumbers e overlaps
   in CucumberMap eastwards' southwards

moveSouthwards :: CucumberMap -> CucumberMap
moveSouthwards (CucumberMap eastwards southwards) =
  let southwards' = do
        (i, s) <- V.indexed southwards
        let overlaps = slice i eastwards
        return $ moveCucumbers s overlaps
   in CucumberMap eastwards southwards'

cucumberDance :: IO ()
cucumberDance =
  -- ...>>>>>...
  let eg :: Lane 11
      eg = (`setBit` 3) . (`setBit` 4) . (`setBit` 5) . (`setBit` 6) $ bit 7
      eg1 = moveCucumbers eg zeroBits
      eg2 = moveCucumbers eg1 zeroBits
      eg3 = moveCucumbers eg2 zeroBits
      eg4 = moveCucumbers eg3 zeroBits
      eg5 = moveCucumbers eg4 zeroBits
      eg6 = moveCucumbers eg5 zeroBits
      eg7 = moveCucumbers eg6 zeroBits
      eg8 = moveCucumbers eg7 zeroBits
   in do
        -- print $ empty @(BitSet Word64 257)
        print $ (empty :: BS Word64 (BSN Word64 257))
        print $ bitsetWidth @Word64 @(Left 257)
        print eg
        print eg1
        print eg2
        print eg3
        print eg4
        print eg6
        print eg7
        print eg8
        (Right parsed) <- getContents >>= runParserT parseMap emptyMap ""
        print parsed
        putStrLn "=========="
        print $ moveEastwards parsed
        putStrLn "=========="
        print $ moveSouthwards parsed

-- print bloop

-- print . showBS $ BSCons @(BSN 512) 0xFFF $ BSEnd 0xFF
