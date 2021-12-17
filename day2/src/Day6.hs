{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Day6 where

import qualified Data.Map.Monoidal as MMap
import Data.Monoid (Sum (..))
import Lib (MyParser, parseInt, parseStdin)
import Text.Parsec
import Text.Parsec.Char

newtype LanternFish = LanternFish {_days :: Int} deriving (Eq, Show, Ord)

type Ocean = MMap.MonoidalMap LanternFish (Sum Integer)

timerReset :: Int
timerReset = 6

newFishOffset :: Int
newFishOffset = 2

newFishReset :: Int
newFishReset = timerReset + newFishOffset

newFish :: LanternFish
newFish = LanternFish newFishReset

resetFish :: LanternFish
resetFish = LanternFish timerReset

tickFishies :: LanternFish -> (Sum Integer) -> Ocean
tickFishies (LanternFish 0) n = MMap.fromList . fmap (,n) $ [newFish, resetFish]
tickFishies (LanternFish m) n = MMap.singleton (LanternFish (m - 1)) n

tickOcean :: Ocean -> Ocean
tickOcean = MMap.foldMapWithKey tickFishies

countOcean :: Ocean -> (Sum Integer)
countOcean = foldMap id

tickDays :: Int -> Ocean -> Ocean
tickDays 0 ocean = ocean
tickDays days ocean = tickDays (days - 1) (tickOcean ocean)

parseFish :: MyParser LanternFish
parseFish = LanternFish <$> parseInt

parseFishies :: MyParser [LanternFish]
parseFishies = (sepBy1 parseFish $ char ',') <* endOfLine <* eof

parseOcean :: MyParser Ocean
parseOcean = foldMap (`MMap.singleton` (Sum (toInteger 1))) <$> parseFishies

nDays :: Int
nDays = 256

simulateFish :: IO ()
simulateFish = do
  parsed <- parseStdin parseOcean
  print $ fmap (countOcean . tickDays nDays) parsed
