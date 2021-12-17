{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Day15 where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Bifunctor.Swap (swap)
import Data.Foldable
import Data.Function (on)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MMap
import Data.Maybe (isNothing, mapMaybe, maybeToList)
import Data.Monoid (Sum (..), getSum)
import Data.Ratio
import Data.Set (Set, (\\))
import qualified Data.Set as Set
import Data.Vector ((!?))
import qualified Data.Vector as V
import GHC.Generics
import Lib (MyParser, parseInt, parseStdin)
import Text.Parsec
import Text.Parsec.Char

data Cave = Cave {width :: Int, height :: Int, riskLevels :: Map (Int, Int) Int} deriving (Show, Eq)

parsePuzzle :: MyParser [[Int]]
parsePuzzle = sepEndBy1 parseLine endOfLine <* eof
  where
    parseDigit = (read @Int . (: [])) <$> digit
    parseLine = many1 parseDigit

puzzleToCave :: [[Int]] -> Cave
puzzleToCave risks =
  let y = (length risks) - 1
      x = (length . head $ risks) - 1
      coords = (,) <$> [0 .. x] <*> [0 .. y]
      lookup (x, y) = risks !! y !! x
      coordToRisk coord = (coord, lookup coord)
      riskLevels = Map.fromList $ fmap coordToRisk coords
   in Cave x y riskLevels

type Path = [(Int, Int)]

target :: Cave -> (Int, Int)
target c = ((width c), (height c))

distanceToTarget :: Cave -> (Int, Int) -> Int
distanceToTarget c (x, y) =
  let (targetX, targetY) = target c
   in (targetY - y) + (targetX - x)

-- Paths are reverse order, so ignore the first element because we don't
-- "enter" (0,0)
pathRisk :: Cave -> Path -> Int
pathRisk (Cave _ _ c) path = getSum $ foldMap (Sum . (c !)) (init path)

adjacent :: (Int, Int) -> [(Int, Int)]
adjacent (x, y) = [((x - 1), y), ((x + 1), y), (x, (y - 1)), (x, (y + 1))]

-- Prioritises exploration by least risk?
step :: Cave -> Path -> [Path]
step _ [] = [[(0, 0)]]
step cave@(Cave width height riskLevels) path@(coord : tail)
  | coord == (width, height) = [path]
  | otherwise =
    let validCoord (x, y) = x >= 0 && x <= width && y >= 0 && y <= height
        adj = filter validCoord . filter (not . flip elem tail) $ adjacent coord
        expanded = (: coord : tail) <$> adj
     in List.sortOn (pathRisk cave) expanded

trimPaths :: Cave -> [Path] -> [Path]
trimPaths cave paths =
  let endPoints = Set.fromList $ fmap head paths
      -- filter any redundant paths which intersect with some other endpoint
      -- noRedundant = filter (all (not . (`Set.member` endPoints)) . tail) paths
      groupedByEndPoint = NonEmpty.groupAllWith head paths
      deduped = fmap (minimumBy (compare `on` (pathRisk cave))) groupedByEndPoint
   in List.sortOn (distanceToTarget cave . head) deduped

findSafePath :: Cave -> IO Path
findSafePath cave@(Cave width height _) =
  let go :: [Path] -> IO Path
      go [] = go ([(0, 0) : []])
      go pps@(p : ps)
        | head p == (width, height) = print p >> return p
        | otherwise =
          let nextPaths = (step cave =<< pps)
              trimmed = trimPaths cave nextPaths
           in go trimmed
   in go []

cheapestPathTo :: Cave -> Set (Int, Int) -> (Int, Int) -> Maybe Path
cheapestPathTo _ _ (0, 0) = Just [(0, 0)]
cheapestPathTo cave@(Cave width height riskLevels) visited coord =
  let validCoord (x, y) = x >= 0 && x <= width && y >= 0 && y <= height
      adj = filter (not . flip Set.member visited) . filter validCoord $ adjacent coord
      nextVisited = Set.union visited $ Set.fromList adj
      parentPaths = mapMaybe (cheapestPathTo cave nextVisited) adj
   in case parentPaths of
        [] -> Nothing
        _ ->
          let best = minimumBy (compare `on` (pathRisk cave)) parentPaths
           in Just $ coord : best

--              hax = List.sortOn (pathRisk cave) $ deduped

-- | otherwise =
--   let betterBranch = step cave p
--    in go betterBranch
findPath :: IO ()
findPath = do
  parsed <- parseStdin parsePuzzle
  let (Right cave) = fmap puzzleToCave parsed
  -- print (cheapestPathTo cave (Set.singleton (2, 2)) (2, 2))
  p <- findSafePath cave
  print $ pathRisk cave p

-- print (step cave =<< step cave =<< step cave =<< step cave [])

-- let (Right cave) = parsed
-- traverse_ print cave
