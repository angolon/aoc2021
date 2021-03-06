{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Day22 where

import Control.Arrow (arr, (&&&))
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Loops as Loops
import qualified Control.Monad.State.Lazy as S
import Data.Either (either)
import Data.Foldable
import Data.Function (on)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MMap
import Data.Maybe
import Data.Monoid (Endo (..), Product (..), Sum (..), getSum)
import Data.PQueue.Min (MinQueue)
import qualified Data.PQueue.Min as MinQueue
import Data.Ratio
import Data.Set (Set, member, union, (\\))
import qualified Data.Set as Set
import qualified Data.Vector as V
import Day3 (toInt)
import Lib (MyParser, parseInt, parseStdin)
import Linear.Metric (dot)
import qualified Linear.Quaternion as Q
import Text.Parsec
import Text.Parsec.Char
import Text.Show.Functions

data Action = On | Off deriving (Show, Eq)

data DimensionSegment = DimensionSegment
  { _minD :: Int,
    _maxD :: Int
  }
  deriving (Eq, Show, Ord)

data PlaneSection = PlaneSection
  { _d1 :: DimensionSegment,
    _d2 :: DimensionSegment
  }

data N3Section = N3Section
  { _xs :: DimensionSegment,
    _ys :: DimensionSegment,
    _zs :: DimensionSegment
  }
  deriving (Eq, Show)

data Cuboid = Cuboid
  { _activated :: Action,
    _section :: N3Section
  }
  deriving (Eq, Show)

makeLenses ''DimensionSegment
makeLenses ''PlaneSection
makeLenses ''N3Section
makeLenses ''Cuboid

dimensionMerge :: DimensionSegment -> DimensionSegment -> Maybe DimensionSegment
dimensionMerge s1 s2 =
  let minS = min s1 s2
      maxS = max s1 s2
      maxMin = (minS ^. maxD) + 1 -- discrete dimensions, join if they're adjacent
      minMax = maxS ^. minD
      merged = minS & maxD .~ (maxS ^. maxD)
   in if maxMin >= minMax
        then Just merged
        else Nothing

dimensionIntersect ::
  DimensionSegment ->
  DimensionSegment ->
  Maybe DimensionSegment
dimensionIntersect
  (DimensionSegment previousLower previousUpper)
  (DimensionSegment nextLower nextUpper) =
    if
        | previousUpper < nextLower -> Nothing
        | previousLower > nextUpper -> Nothing
        | otherwise ->
          let lower = max previousLower nextLower
              upper = min previousUpper nextUpper
           in Just $ DimensionSegment lower upper

dContains :: DimensionSegment -> DimensionSegment -> Bool
dContains (DimensionSegment lmin lmax) (DimensionSegment rmin rmax) =
  (rmin >= lmin) && (rmax <= lmax)

n3Contains :: N3Section -> N3Section -> Bool
n3Contains (N3Section lxs lys lzs) (N3Section rxs rys rzs) =
  (lxs `dContains` rxs) && (lys `dContains` rys) && (lzs `dContains` rzs)

dimensionOverwrite ::
  DimensionSegment ->
  DimensionSegment ->
  Maybe (Maybe DimensionSegment, Maybe DimensionSegment)
dimensionOverwrite
  previous@(DimensionSegment previousLower previousUpper)
  next@(DimensionSegment nextLower nextUpper) =
    if
        | previousUpper < nextLower -> Nothing
        | previousLower > nextUpper -> Nothing
        | otherwise ->
          let rhs =
                if previousUpper > nextUpper
                  then Just $ DimensionSegment (nextUpper + 1) previousUpper
                  else Nothing
              lhs =
                if previousLower < nextLower
                  then Just $ DimensionSegment previousLower (nextLower - 1)
                  else Nothing
           in Just (lhs, rhs)

dimensionPoints :: DimensionSegment -> [Int]
dimensionPoints (DimensionSegment min max) = [min .. max]

n3Points :: N3Section -> [(Int, Int, Int)]
n3Points (N3Section xs ys zs) = do
  x <- dimensionPoints xs
  y <- dimensionPoints ys
  z <- dimensionPoints zs
  return (x, y, z)

n3SectionMerge :: N3Section -> N3Section -> Maybe N3Section
n3SectionMerge left@(N3Section lxs lys lzs) right@(N3Section rxs rys rzs)
  | left == right = Just left
  | left `n3Contains` right = Just left
  | right `n3Contains` left = Just right
  | lxs == rxs && lys == rys = N3Section lxs lys <$> dimensionMerge lzs rzs
  | lxs == rxs && lzs == rzs = N3Section lxs <$> dimensionMerge lys rys <*> pure lzs
  | lys == rys && lzs == rzs = N3Section <$> dimensionMerge lxs rxs <*> pure lys <*> pure lzs
  | otherwise = Nothing

mergeN3Sections :: [N3Section] -> [N3Section]
mergeN3Sections [] = []
mergeN3Sections (n3 : []) = n3 : []
mergeN3Sections sections =
  let anyMerged = listToMaybe $ do
        l <- sections
        r <- sections
        guard $ l /= r
        merged <- maybeToList $ n3SectionMerge l r
        return (merged, l, r)
   in case anyMerged of
        Just (merged, l, r) ->
          let unmerged = sections List.\\ [l, r]
           in mergeN3Sections $ merged : unmerged
        Nothing -> sections -- No mergeable sections

n3SectionOverwrite :: N3Section -> N3Section -> Maybe [N3Section]
n3SectionOverwrite
  prior@(N3Section priorXs priorYs priorZs)
  next@(N3Section nextXs nextYs nextZs) =
    let xsOverwrite = dimensionOverwrite priorXs nextXs
        ysOverwrite = dimensionOverwrite priorYs nextYs
        zsOverwrite = dimensionOverwrite priorZs nextZs
        generateQuadrants (lx, ux) (ly, uy) (lz, uz) =
          let xIntersect = dimensionIntersect priorXs nextXs
              yIntersect = dimensionIntersect priorYs nextYs
              zIntersect = dimensionIntersect priorZs nextZs
              allQuadrants = do
                xs <- [lx, ux, xIntersect]
                ys <- [ly, uy, yIntersect]
                zs <- [lz, uz, zIntersect]
                -- ignore the central intersection combination
                guard $ not (xs == xIntersect && ys == yIntersect && zs == zIntersect)
                maybeToList $ N3Section <$> xs <*> ys <*> zs
           in mergeN3Sections allQuadrants
        -- in error . show $ mergeN3Sections allQuadrants
        maybeAllQuadrants = generateQuadrants <$> xsOverwrite <*> ysOverwrite <*> zsOverwrite
     in maybeAllQuadrants

cuboidOverwrite :: Cuboid -> Cuboid -> [Cuboid]
cuboidOverwrite
  prior@(Cuboid priorState priorN3)
  next@(Cuboid _ nextN3) =
    let overwritten = n3SectionOverwrite priorN3 nextN3
     in case overwritten of
          Just [] -> [] -- preserve the prior cuboid if it wasn't effected by the overwrite
          Just splits -> fmap (Cuboid priorState) splits
          Nothing -> [prior] -- preserve the prior cuboid if it wasn't effected by the overwrite

combineCuboids :: [Cuboid] -> [Cuboid]
combineCuboids (c : []) = [c]
combineCuboids (c1 : cns) =
  let go existingCuboids [] = existingCuboids
      go existingCuboids (cuboid : tail) =
        let nextExisting = existingCuboids >>= (`cuboidOverwrite` cuboid)
         in go (cuboid : nextExisting) tail
   in go [c1] cns

segmentLength :: DimensionSegment -> Int
segmentLength (DimensionSegment a b) = 1 + (abs (b - a))

volume :: Cuboid -> Int
volume (Cuboid _ (N3Section xs ys zs)) =
  (segmentLength xs) * (segmentLength ys) * (segmentLength zs)

combinedOnVolume :: [Cuboid] -> Int
combinedOnVolume =
  sum . fmap volume . filter ((== On) . _activated) . combineCuboids

parseCuboid :: MyParser Cuboid
parseCuboid =
  let parseDimensionSegment = DimensionSegment <$> parseInt <* string ".." <*> parseInt
      parseN3Section =
        N3Section <$ string "x=" <*> parseDimensionSegment
          <* string ",y=" <*> parseDimensionSegment
          <* string ",z=" <*> parseDimensionSegment
      parseOn = const On <$> try (string "on")
      parseOff = const Off <$> try (string "off")
      parseAction = parseOn <|> parseOff
   in Cuboid <$> parseAction <* space <*> parseN3Section

parsePuzzle :: MyParser [Cuboid]
parsePuzzle = sepEndBy1 parseCuboid endOfLine <* eof

rebootReactor :: IO ()
rebootReactor = do
  (Right puzzle) <- parseStdin parsePuzzle
  -- traverse_ print puzzle
  -- print . combinedOnVolume $ (take 4 puzzle)
  -- let blah = filter ((== On) . _activated) . combineCuboids $ puzzle
  let blah = combineCuboids puzzle
  -- traverse_ print blah
  print . combinedOnVolume $ puzzle

-- intersection :: Cuboid -> Cuboid -> Maybe Cuboid
