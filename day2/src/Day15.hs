{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Day15 where

import Control.Applicative
import Control.Arrow (arr, (&&&))
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
import Data.PQueue.Min (MinQueue)
import qualified Data.PQueue.Min as MinQueue
import Data.Ratio
import Data.Set (Set, union, (\\))
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

enlargeCave :: Cave -> Cave
enlargeCave cave@(Cave width height riskLevels) =
  let initial = Map.toList riskLevels
      increaseRisk n i =
        let j = i + n
         in if j > 9 then j - 9 else j
      increaseX x i = (i * (width + 1)) + x
      increaseY y i = (i * (height + 1)) + y
      expanded = do
        ((x, y), r) <- initial
        a <- [0 .. 4]
        b <- [0 .. 4]
        let r' = increaseRisk (a + b) r
        let x' = increaseX x a
        let y' = increaseY y b
        return ((x', y'), r')
      width' = ((width + 1) * 5) - 1
      height' = ((height + 1) * 5) - 1
   in Cave width' height' $ Map.fromList expanded

data Path = Path {cave :: Cave, steps :: [(Int, Int)]} deriving (Eq, Show)

instance Ord Path where
  compare p1 p2 =
    let dist = arr distanceToTarget
        weight = arr pathRisk
        cmp = weight &&& dist
     in compare (cmp p1) (cmp p2)

emptyPath :: Cave -> Path
emptyPath cave = Path cave []

pathEnd :: Path -> (Int, Int)
pathEnd = head . steps

-- Paths are reverse order, so ignore the first element because we don't
-- "enter" (0,0)
pathRisk :: Path -> Int
pathRisk (Path (Cave _ _ c) path) = getSum $ foldMap (Sum . (c !)) (init path)

target :: Cave -> (Int, Int)
target c = ((width c), (height c))

distanceToTarget :: Path -> Int
distanceToTarget (Path cave ((x, y) : _)) =
  let (targetX, targetY) = target cave
   in (targetY - y) + (targetX - x)

adjacent :: (Int, Int) -> [(Int, Int)]
adjacent (x, y) = [((x - 1), y), ((x + 1), y), (x, (y - 1)), (x, (y + 1))]

-- Prioritises exploration by least risk?
step :: Set (Int, Int) -> Path -> ((Set (Int, Int)), MinQueue Path)
step _ (Path cave []) = ((Set.singleton (0, 0)), (MinQueue.singleton $ Path cave [(0, 0)]))
step visited (Path cave@(Cave width height riskLevels) path@(coord : tail))
  | coord == (width, height) = (visited, MinQueue.singleton $ Path cave path)
  | otherwise =
    let validCoord coord@(x, y) = x >= 0 && x <= width && y >= 0 && y <= height && (not . Set.member coord $ visited)
        adj = filter validCoord $ adjacent coord
        expanded = (: coord : tail) <$> adj
        queue = MinQueue.fromList (Path cave <$> expanded)
        nextVisited = visited `union` (Set.fromList adj)
     in (nextVisited, queue)

-- trimPaths :: Cave -> [Path] -> [Path]
-- trimPaths cave paths =
--   -- filter any redundant paths which intersect with some other endpoint
--   let groupedByEndPoint = NonEmpty.groupAllWith head paths
--       deduped = fmap (minimumBy (compare `on` (pathRisk cave))) groupedByEndPoint
--       dist = arr distanceToTarget cave . head
--       weight = arr pathRisk cave
--    in -- noRedundant = filter (all (not . (`Set.member` endPoints)) . tail) deduped
--       -- noRedundant =
--       --   filter
--       --     ( \p ->
--       --         let riskP = pathRisk cave p
--       --          in all
--       --               ( \q ->
--       --                   let end = head p
--       --                       q' = dropWhile (/= end) q
--       --                       riskQ = pathRisk cave q'
--       --                    in p == q || null q' || riskP < riskQ
--       --               )
--       --               deduped
--       --     )
--       --     deduped
--       -- List.sortOn (dist &&& weight) deduped
--       List.sortOn (weight &&& dist) deduped

findSafePath :: Cave -> IO Path
findSafePath cave@(Cave width height _) =
  let go :: Set (Int, Int) -> MinQueue Path -> IO Path
      go visited paths =
        case MinQueue.minView paths of
          Nothing -> go (Set.singleton (0, 0)) (MinQueue.singleton $ Path cave [(0, 0)])
          Just (p, ps)
            | pathEnd p == (width, height) -> print p >> return p
            | otherwise ->
              let (nextVisited, nextPaths) = step visited p
                  -- hax = print $ (fmap steps $ MinQueue.take 3 nextPaths)
                  hax = print $ Set.size nextVisited
               in hax >> (go nextVisited $ MinQueue.union nextPaths ps)
   in go Set.empty MinQueue.empty

findPath :: IO ()
findPath = do
  parsed <- parseStdin parsePuzzle
  let (Right cave) = fmap (enlargeCave . puzzleToCave) parsed
  -- print (cheapestPathTo cave (Set.singleton (2, 2)) (2, 2))
  p <- findSafePath cave
  let foo = (,49) <$> [0 .. 49]
  let blah = riskLevels cave
  print $ fmap (blah !) foo
  print $ pathRisk p

-- print (step cave =<< step cave =<< step cave =<< step cave [])

-- let (Right cave) = parsed
-- traverse_ print cave
