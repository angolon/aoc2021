{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Day9 where

import Data.Foldable
import Data.Maybe (maybeToList)
import Data.Monoid (Sum (..), getSum)
import Data.Vector ((!?))
import qualified Data.Vector as V
import Lib (MyParser, parseInt, parseStdin)
import Text.Parsec
import Text.Parsec.Char

parseHeight :: MyParser Int
parseHeight = read @Int . (: []) <$> digit

type HeightLine = V.Vector Int

type HeightMap = V.Vector HeightLine

parseHeightLine :: MyParser HeightLine
parseHeightLine = V.fromList <$> many1 parseHeight

parseHeightMap :: MyParser HeightMap
parseHeightMap = V.fromList <$> sepEndBy1 parseHeightLine endOfLine <* eof

heightLookup :: HeightMap -> Int -> Int -> Maybe Int
heightLookup m x y = (!? x) =<< (m !? y)

isLowPoint :: HeightMap -> Int -> Int -> Maybe Int
isLowPoint m x y =
  let (Just p) = heightLookup m x y -- being dangerous and assuming the initial lookup never fails.
      surroundingCoords = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
      surroundingPointsM = uncurry (heightLookup m) <$> surroundingCoords
      surroundingPoints = maybeToList =<< surroundingPointsM
   in if all (p <) surroundingPoints then Just p else Nothing

allLowPoints :: HeightMap -> [Int]
allLowPoints m =
  let y = V.length m
      x = V.length . V.head $ m
   in do
        i <- [0 .. (x - 1)]
        j <- [0 .. (y - 1)]
        maybeToList $ isLowPoint m i j

riskLevel :: HeightMap -> Int
riskLevel = sum . map (+ 1) . allLowPoints

assessRisk :: IO ()
assessRisk = do
  parsed <- parseStdin parseHeightMap
  print $ fmap riskLevel parsed
