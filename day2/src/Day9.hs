{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Day9 where

import Data.Foldable
import qualified Data.List as List
import Data.Maybe (maybeToList)
import Data.Monoid (Sum (..), getSum)
import Data.Set (Set)
import qualified Data.Set as Set
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

adjacentPoints :: Int -> Int -> [(Int, Int)]
adjacentPoints x y = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

isLowPoint :: HeightMap -> Int -> Int -> Maybe (Int, Int)
isLowPoint m x y =
  let (Just p) = heightLookup m x y -- being dangerous and assuming the initial lookup never fails.
      surroundingPointsM = uncurry (heightLookup m) <$> adjacentPoints x y
      surroundingPoints = maybeToList =<< surroundingPointsM
   in if all (p <) surroundingPoints then (Just (x, y)) else Nothing

expandBasin :: HeightMap -> Int -> Int -> Int
expandBasin m x y =
  let go :: Set (Int, Int) -> Int -> [(Int, Int)] -> Int
      go visited accum ((x, y) : coords) =
        if Set.member (x, y) visited
          then go visited accum coords
          else
            let height = heightLookup m x y
                nextSet = Set.insert (x, y) visited
             in case height of
                  Just (h)
                    | h < 9 ->
                      let nextCoords = adjacentPoints x y
                       in go nextSet (accum + 1) (nextCoords ++ coords)
                  _ -> go visited accum coords
      go _ accum [] = accum
   in go Set.empty 0 [(x, y)]

allLowPoints :: HeightMap -> [(Int, Int)]
allLowPoints m =
  let y = V.length m
      x = V.length . V.head $ m
   in do
        i <- [0 .. (x - 1)]
        j <- [0 .. (y - 1)]
        maybeToList $ isLowPoint m i j

allLowHeights :: HeightMap -> [Int]
allLowHeights m =
  let maybePoints = fmap (uncurry (heightLookup m)) $ allLowPoints m
   in maybeToList =<< maybePoints

allBasinSizes :: HeightMap -> [Int]
allBasinSizes m = fmap (uncurry (expandBasin m)) $ allLowPoints m

bestBasinSizes :: HeightMap -> Int
bestBasinSizes = product . take 3 . reverse . List.sort . allBasinSizes

riskLevel :: HeightMap -> Int
riskLevel = sum . map (+ 1) . allLowHeights

assessRisk :: IO ()
assessRisk = do
  parsed <- parseStdin parseHeightMap
  print $ fmap bestBasinSizes parsed
