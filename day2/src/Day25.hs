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

type WordT = Word64

type Width = 139

width :: (Integral a) => a
width = fromInteger $ natVal (Proxy :: Proxy Width)

type Height = 137

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
        | (`testBit` j) $ eastwards ! i -> return '>'
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
        ij <- mapIndices
        modifyState $ setEast ij
      parseSouth = do
        _ <- char 'v'
        ij <- mapIndices
        modifyState $ setSouth ij
      parseEmpty = char '.' *> pure ()
      parseCell = parseEast <|> parseSouth <|> parseEmpty
      parseRow = count width parseCell *> endOfLine
      parseLines = count height parseRow *> eof
   in parseLines *> getState

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
      lane' = lane `rotateL` 1
      moved = lane' .&. notOccupied
      immovable = moved `xor` lane'
   in (immovable `rotateR` 1) .|. moved

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

stepMap :: CucumberMap -> CucumberMap
stepMap = moveSouthwards . moveEastwards

solve :: CucumberMap -> IO ()
solve initial =
  let step :: S.StateT (CucumberMap, CucumberMap, Int) IO ()
      step = do
        (m1, m2, i) <- S.get
        let m3 = stepMap m2
        let i' = i + 1
        S.lift $ putStrLn $ "After " ++ show i' ++ " steps"
        S.lift $ print m3
        S.put (m2, m3, i')
        S.lift $ putStrLn ""
        return ()

      finished :: S.StateT (CucumberMap, CucumberMap, Int) IO Bool
      finished = do
        (m1, m2, i) <- S.get
        return $ m1 == m2

      solutionS = do
        _ <- Loops.untilM_ step finished
        (_, _, i) <- S.get
        S.lift $ putStrLn $ "Finished after " ++ show i ++ " steps"
   in S.evalStateT solutionS (initial, initial, 0)

cucumberDance :: IO ()
cucumberDance = do
  (Right parsed) <- getContents >>= runParserT parseMap emptyMap ""
  putStrLn "Initial state:"
  print parsed
  putStrLn ""
  solve parsed
