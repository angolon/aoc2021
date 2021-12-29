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

-- data Cuboid = Cuboid
--   { _activated :: Bool,
--     _xs :: (Int, Int),
--     _ys :: (Int, Int),
--     _zs :: (Int, Int)
--   }
--   deriving (Eq, Show)

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

n3SectionOverwrite :: N3Section -> N3Section -> [N3Section]
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
              left = N3Section <$> lx <*> yIntersect <*> zIntersect
              right = N3Section <$> ux <*> yIntersect <*> zIntersect
              down = N3Section <$> xIntersect <*> ly <*> zIntersect
              up = N3Section <$> xIntersect <*> uy <*> zIntersect
              back = N3Section <$> xIntersect <*> yIntersect <*> lz
              forward = N3Section <$> xIntersect <*> yIntersect <*> uz
              -- for these variations:
              -- l, r = left, right
              -- u, d = down, up
              -- b, f == back, forward
              ld = N3Section <$> lx <*> ly <*> zIntersect
              lu = N3Section <$> lx <*> uy <*> zIntersect
              ru = N3Section <$> ux <*> uy <*> zIntersect
              rd = N3Section <$> ux <*> ly <*> zIntersect
              ---
              bld = N3Section <$> lx <*> ly <*> lz
              bl = N3Section <$> lx <*> yIntersect <*> lz
              blu = N3Section <$> lx <*> uy <*> lz
              bu = N3Section <$> xIntersect <*> uy <*> lz
              bru = N3Section <$> ux <*> uy <*> lz
              br = N3Section <$> ux <*> yIntersect <*> lz
              brd = N3Section <$> ux <*> ly <*> lz
              bd = N3Section <$> xIntersect <*> ly <*> lz
              ---
              fld = N3Section <$> lx <*> ly <*> uz
              fl = N3Section <$> lx <*> yIntersect <*> uz
              flu = N3Section <$> lx <*> uy <*> uz
              fu = N3Section <$> xIntersect <*> uy <*> uz
              fru = N3Section <$> ux <*> uy <*> uz
              fr = N3Section <$> ux <*> yIntersect <*> uz
              frd = N3Section <$> ux <*> ly <*> uz
              fd = N3Section <$> xIntersect <*> ly <*> uz
              --
              allMaybeQuadrants =
                [ left,
                  right,
                  up,
                  down,
                  back,
                  forward,
                  ld,
                  lu,
                  ru,
                  rd,
                  bld,
                  bl,
                  blu,
                  bu,
                  bru,
                  br,
                  brd,
                  bd,
                  fld,
                  fl,
                  flu,
                  fu,
                  fru,
                  fr,
                  frd,
                  fd
                ]
              allQuadrants = allMaybeQuadrants >>= maybeToList
           in mergeN3Sections allQuadrants
        -- in error . show $ mergeN3Sections allQuadrants
        maybeAllQuadrants = generateQuadrants <$> xsOverwrite <*> ysOverwrite <*> zsOverwrite
     in join . maybeToList $ maybeAllQuadrants

-- in mergeN3Sections allQuadrants

planeSectionXY :: N3Section -> PlaneSection
planeSectionXY c = PlaneSection (c ^. xs) (c ^. ys)

planeSectionXZ :: N3Section -> PlaneSection
planeSectionXZ c = PlaneSection (c ^. xs) (c ^. zs)

planeSectionYZ :: N3Section -> PlaneSection
planeSectionYZ c = PlaneSection (c ^. ys) (c ^. zs)

addCuboids :: Cuboid -> Cuboid -> [Cuboid]
addCuboids c1 c2 =
  if
      | (c1 ^. section) == (c2 ^. section) -> [c2] -- take rhs state
      | otherwise -> error "blah"

-- intersection :: Cuboid -> Cuboid -> Maybe Cuboid
