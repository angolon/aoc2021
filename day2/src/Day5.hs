{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Day5 where

import Control.Arrow
import Control.Lens
import Control.Monad
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Lib (MyParser, parseInt, parseStdin)
import Text.Parsec
import Text.Parsec.Char

type Point = (Int, Int)

data VentLine = VentLine {_a :: Point, _b :: Point} deriving (Eq, Show)

data LineEquation
  = Horizontal {_y :: Int}
  | Vertical {_x :: Int}
  deriving (Eq, Show)

data Bound = Bound {_lower :: Point, _upper :: Point} deriving (Eq, Show)

-- Constructor that enforces ordering of bounds.
bound :: Point -> Point -> Bound
bound (x1, y1) (x2, y2) =
  Bound (min x1 x2, min y1 y2) (max x1 x2, max y1 y2)

inBound :: Bound -> Point -> Bool
inBound (Bound lower upper) p = lower <= p && p <= upper

-- Duplicates "awesome" stuff done with arrows - could maybe make bounds
-- representable as functions.
boundOverlap :: Bound -> Bound -> Bound
boundOverlap
  (Bound (x1, y1) (x2, y2))
  (Bound (x3, y3) (x4, y4)) =
    let lowX = max x1 x3
        highX = min x2 x4
        lowY = max y1 y3
        highY = min y2 y4
     in Bound (lowX, lowY) (highX, highY)

intersection :: LineEquation -> LineEquation -> Maybe Point
intersection h@(Horizontal _) v@(Vertical _) = intersection v h
intersection (Vertical x) (Horizontal y) = Just (x, y)
intersection _ _ = Nothing

data LineSegment = LineSegment {_line :: LineEquation, _bound :: Bound} deriving (Eq, Show)

-- I have made this function as over-complicated as I possibly could,
-- because... I felt like it? ⁻\_(ツ)_/⁻
segmentIntersection :: LineSegment -> LineSegment -> Maybe Point
segmentIntersection
  (LineSegment l1 b1)
  (LineSegment l2 b2) =
    let inBothArr = arr inBound b1 &&& arr inBound b2
        inBoth = allOf both id . inBothArr
     in mfilter inBoth (intersection l1 l2)

segmentOverlap :: LineSegment -> LineSegment -> Maybe LineSegment
segmentOverlap
  (LineSegment l1 b1)
  (LineSegment l2 b2)
    | l1 == l2 = Just (LineSegment l1 (boundOverlap b1 b2))
    | otherwise = Nothing

segmentPoints :: LineSegment -> [Point]
segmentPoints (LineSegment l b@(Bound (x1, y1) (x2, y2)))
  | (Horizontal y) <- l =
    if y1 <= y && y <= y2
      then (flip (,) $ y) <$> [x1 .. x2]
      else []
  | (Vertical x) <- l =
    if x1 <= x && x <= x2
      then ((,) x) <$> [y1 .. y2]
      else []
  | otherwise = []

segmentsDanger :: LineSegment -> LineSegment -> [Point]
segmentsDanger ls1 ls2
  | ls1 /= ls2 =
    let intersection = Maybe.maybeToList $ segmentIntersection ls1 ls2
        overlap = segmentOverlap ls1 ls2
        overlapPoints = segmentPoints =<< (Maybe.maybeToList overlap)
     in overlapPoints ++ intersection
  | otherwise = []

ventLineToSegment :: VentLine -> Maybe LineSegment
ventLineToSegment (VentLine p1@(x1, y1) p2@(x2, y2))
  | x1 == x2 = Just $ LineSegment (Vertical x1) (bound p1 p2)
  | y1 == y2 = Just $ LineSegment (Horizontal y1) (bound p1 p2)
  | otherwise = Nothing

findDanger :: [LineSegment] -> [Point]
findDanger segments = do
  s1 <- segments
  s2 <- segments
  segmentsDanger s1 s2

countDistinct :: (Ord a) => [a] -> Int
countDistinct = Set.size . Set.fromList

countDanger = countDistinct . findDanger

parsePoint :: MyParser Point
parsePoint = (,) <$> parseInt <* spaces <* char ',' <*> parseInt

parseVentLine :: MyParser VentLine
parseVentLine = VentLine <$> parsePoint <* spaces <* string "->" <* spaces <*> parsePoint

parseLineSegment :: MyParser LineSegment
parseLineSegment = do
  ventLine <- parseVentLine
  let segment = ventLineToSegment ventLine
  case segment of
    Just s -> return s
    Nothing -> parserFail $ "Invalid line segment: " ++ (show ventLine)

parseLineSegments :: MyParser [LineSegment]
parseLineSegments = many1 (parseLineSegment <* endOfLine) <* eof

avoidDanger :: IO ()
avoidDanger = do
  parsed <- parseStdin parseLineSegments
  print $ fmap countDanger parsed
