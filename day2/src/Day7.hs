{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Day7 where

import Data.Monoid (Sum (..))
import Lib (MyParser, parseInt, parseStdin)
import Text.Parsec

newtype Crab = Crab {_position :: Int}

parseCrab :: MyParser Crab
parseCrab = Crab <$> parseInt

parseCrabbies :: MyParser [Crab]
parseCrabbies = (sepBy1 parseCrab $ char ',') <* endOfLine <* eof

-- -- tries to minimize the metric of c
binarySearch :: (Integral a, Num b, Ord b) => a -> a -> (a -> b) -> a
binarySearch lowerBound upperBound f =
  let lowestB = f lowerBound
      highestB = f upperBound
      go lowA lowB highA highB
        | lowA == highA = lowA -- converged, implies lowB == highB
        | (lowA + 1) == highA = if lowB <= highB then lowA else highA
        | otherwise =
          let midA = ((highA - lowA) `div` 2) + lowA
              midB = f midA
           in if (distance lowB midB) <= (distance midB highB)
                then go lowA lowB midA midB
                else go midA midB highA highB
   in go lowerBound (f lowerBound) upperBound (f upperBound)

distance :: (Num a) => a -> a -> a
distance a = abs . (a -)

fuelConsumption :: Int -> Int
fuelConsumption d = sum [1 .. d]

-- fuelConsumption = id

moveCrab :: Int -> Crab -> Int
moveCrab x (Crab position) =
  fuelConsumption $ distance x position

moveCrabs :: Int -> [Crab] -> Int
moveCrabs x = getSum . foldMap (Sum . moveCrab x)

optimizeCrabs :: [Crab] -> (Int, Int)
optimizeCrabs crabs =
  let minX = minimum . fmap _position $ crabs
      maxX = maximum . fmap _position $ crabs
      bestIdx =
        binarySearch
          minX
          maxX
          (\x -> moveCrabs x crabs)
   in (bestIdx, moveCrabs bestIdx crabs)

escapeWhale :: IO ()
escapeWhale = do
  parsed <- parseStdin parseCrabbies
  print $ fmap optimizeCrabs parsed
