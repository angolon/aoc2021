{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Day6 where

import Lib (MyParser, parseInt, parseStdin)
import Text.Parsec
import Text.Parsec.Char

data LanternFish = LanternFish {_days :: Int} deriving (Eq, Show)

timerReset :: Int
timerReset = 6

newFishOffset :: Int
newFishOffset = 2

tickFish :: LanternFish -> [LanternFish]
tickFish (LanternFish 0) = [(LanternFish timerReset), (LanternFish (timerReset + newFishOffset))]
tickFish (LanternFish n) = [LanternFish (n - 1)]

tickFishies :: [LanternFish] -> [LanternFish]
tickFishies = (=<<) tickFish

tickDays :: Int -> [LanternFish] -> [LanternFish]
tickDays 0 fishies = fishies
tickDays days fishies = tickDays (days - 1) (tickFishies fishies)

parseFish :: MyParser LanternFish
parseFish = LanternFish <$> parseInt

parseFishies :: MyParser [LanternFish]
parseFishies = (sepBy1 parseFish $ char ',') <* endOfLine <* eof

nDays :: Int
nDays = 80

simulateFish :: IO ()
simulateFish = do
  parsed <- parseStdin parseFishies
  print $ fmap (length . tickDays nDays) parsed
