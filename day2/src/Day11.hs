{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Day11 where

import Control.Lens
import Control.Monad
import Data.Foldable
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isNothing, maybeToList)
import Data.Monoid (Sum (..), getSum)
import Data.Set (Set, (\\))
import qualified Data.Set as Set
import Data.Vector ((!?))
import qualified Data.Vector as V
import GHC.Generics
import Lib (MyParser, parseInt, parseStdin)
import Text.Parsec
import Text.Parsec.Char

newtype Octopus = Octopus {_charge :: Maybe Int} deriving (Eq, Show)

makeLenses ''Octopus

type Octopi = Map (Int, Int) Octopus

parseOctopus :: MyParser (Octopus)
parseOctopus = Octopus . Just . read @Int . (: []) <$> digit

parseOctopi :: MyParser Octopi
parseOctopi =
  let parseLine = V.fromList <$> many1 parseOctopus
      ls = V.fromList <$> sepEndBy1 parseLine endOfLine <* eof
      tupleIndexed :: V.Vector (V.Vector Octopus) -> V.Vector ((Int, Int), Octopus)
      tupleIndexed octopi = do
        (x, ys) <- V.indexed octopi
        (y, octopus) <- V.indexed ys
        return ((x, y), octopus)
   in Map.fromList . V.toList . tupleIndexed <$> ls

incrementCharge :: Maybe Int -> Maybe Int
incrementCharge = mfilter (<= 9) . (fmap (+ 1))

chargeOctopus :: Octopus -> Octopus
chargeOctopus = charge %~ incrementCharge

chargeOctopi :: Octopi -> Octopi
chargeOctopi = mapped %~ chargeOctopus

resetOctopus :: Octopus -> Octopus
resetOctopus = charge .~ Just 0

resetOctopi :: Octopi -> Octopi
resetOctopi = mapped . charge . filtered (isNothing) .~ Just 0

adjacentIndices :: (Int, Int) -> Set (Int, Int)
adjacentIndices (x, y) = Set.fromList ((,) <$> [(x - 1) .. (x + 1)] <*> [(y -1) .. (y + 1)])

findChargedIndices :: Octopi -> Set (Int, Int)
findChargedIndices octopi =
  let charged = octopi ^.. (ifolded . withIndex . filtered (isNothing . _charge . snd) . _1)
   in Set.fromList charged

flashIndex :: Octopi -> (Int, Int) -> Octopi
flashIndex octopi index =
  let adjacent = adjacentIndices index
   in over (imapped . (indices (flip Set.member $ adjacent))) chargeOctopus $ octopi

chainReaction :: Octopi -> ((Set (Int, Int)), Octopi)
chainReaction octopi =
  let go flashedIndices octopi =
        let chargedIndices = findChargedIndices octopi
            toFlash = chargedIndices \\ flashedIndices
            nextOctopi = foldl' flashIndex octopi toFlash
         in if nextOctopi == octopi
              then (chargedIndices, nextOctopi)
              else go chargedIndices nextOctopi
   in go Set.empty octopi

step :: Octopi -> (Int, Octopi)
step octopi =
  let charged = chargeOctopi octopi
      (flashedIndices, flashedOctopi) = chainReaction charged
      nFlashed = Set.size flashedIndices
   in (nFlashed, resetOctopi flashedOctopi)

steps :: Int -> Octopi -> (Int, Octopi)
steps n octopi =
  let go 0 accum octopi = (accum, octopi)
      go n accum octopi =
        let (nFlashed, nextOctopi) = step octopi
         in go (n - 1) (accum + nFlashed) nextOctopi
   in go n 0 octopi

findSynchronizationPoint :: Octopi -> Maybe Int
findSynchronizationPoint octopi =
  let n = Map.size octopi
      simulation = List.iterate (step . snd) (0, octopi)
   in List.findIndex ((== n) . fst) simulation

observeOctopi :: IO ()
observeOctopi = do
  parsed <- parseStdin parseOctopi
  print $ fmap (findSynchronizationPoint) parsed
