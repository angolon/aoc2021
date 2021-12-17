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
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector ((!?))
import qualified Data.Vector as V
import GHC.Generics
import Lib (MyParser, parseInt, parseStdin)
import Text.Parsec
import Text.Parsec.Char

newtype Octopus = Octopus {_charge :: Maybe Int} deriving (Eq, Show)

makeLenses ''Octopus

type Octopi = (V.Vector (V.Vector (Octopus)))

parseOctopus :: MyParser (Octopus)
parseOctopus = Octopus . Just . read @Int . (: []) <$> digit

parseOctopi :: MyParser Octopi
parseOctopi =
  let parseLine = V.fromList <$> many1 parseOctopus
   in V.fromList <$> sepEndBy1 parseLine endOfLine <* eof

incrementCharge :: Maybe Int -> Maybe Int
incrementCharge = mfilter (<= 9) . (fmap (+ 1))

chargeOctopi :: Octopi -> Octopi
chargeOctopi = mapped . mapped . charge %~ incrementCharge

adjacentIndicies :: (Int, Int) -> Set (Int, Int)
adjacentIndicies (x, y) = Set.fromList ((,) <$> [(x - 1) .. (x + 1)] <*> [(y -1) .. (y + 1)])

findChargedIndices :: Octopi -> [(Int, (V.Vector Int))]
findChargedIndices octopi =
  let withOuterIndices = octopi ^.. ifolded . withIndex
      charged (i, os) = (i,) $ V.findIndices (isNothing . _charge) os
   in fmap charged withOuterIndices

chainReaction :: Octopi -> Octopi
chainReaction octopi =
  let is = findChargedIndices octopi
      outerIs :: [Int]
      (outerIs, _) = unzip is
      -- unsure if `imapped` is the right function here
      res = over (imapped . (indices (flip elem $ outerIs))) id $ V.toList octopi
   in V.fromList res

observeOctopi :: IO ()
observeOctopi = do
  parsed <- parseStdin parseOctopi
  print $ fmap chargeOctopi parsed
