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
binarySearch :: (Integral a, Ord c) => a -> a -> (a -> b) -> (b -> b -> c) -> b
binarySearch lowerBound upperBound f metric =
  let lowestB = f lowerBound
      highestB = f upperBound
      go lowA lowB highA highB
        | lowA == highA = lowB -- converged, implies lowB == highB
        | otherwise =
          let midA = ((highA - lowA) `div` 2) + lowA
              midB = f midA
              c1 = metric lowB midB
              c2 = metric midB highB
           in if c1 <= c2
                then go lowA lowB midA midB
                else go midA midB highA highB
   in go lowerBound (f lowerBound) upperBound (f upperBound)

distance :: (Num a) => a -> a -> a
distance a = abs . (a -)

moveCrab :: Int -> Crab -> Int
moveCrab x = abs . (x -) . _position

moveCrabs :: Int -> [Crab] -> Int
moveCrabs x = getSum . foldMap (Sum . moveCrab x)

optimizeCrabs :: [Crab] -> Int
optimizeCrabs crabs =
  let minX = minimum . fmap _position $ crabs
      maxX = maximum . fmap _position $ crabs
   in binarySearch minX maxX (\x -> moveCrabs x crabs) distance

escapeWhale :: IO ()
escapeWhale = do
  parsed <- parseStdin parseCrabbies
  print $ fmap optimizeCrabs parsed
