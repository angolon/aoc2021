{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Day5 where

import Control.Arrow
import Control.Lens
import Control.Monad
import Data.Either (isRight)
import Data.Foldable (traverse_)
import qualified Data.Map.Monoidal as MMap
import qualified Data.Maybe as Maybe
import Data.Monoid (Sum (..))
import Data.Ratio
import qualified Data.Set as Set
import Lib (MyParser, parseInt, parseStdin)
import Text.Parsec
import Text.Parsec.Char

type Dimension = Ratio Int

type Point = (Dimension, Dimension)

data VentLine = VentLine {_a :: Point, _b :: Point} deriving (Eq, Show)

data LineEquation
  = Horizontal {_y :: Dimension}
  | Vertical {_x :: Dimension}
  | Gradient {_m :: Dimension, _c :: Dimension}
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
-- WARNING  - is this fucked?
boundOverlap :: Bound -> Bound -> Maybe Bound
boundOverlap
  (Bound (x1, y1) (x2, y2))
  (Bound (x3, y3) (x4, y4)) =
    let lowX = max x1 x3
        highX = min x2 x4
        lowY = max y1 y3
        highY = min y2 y4
     in if lowX > highX || lowY > highY
          then Nothing
          else Just $ Bound (lowX, lowY) (highX, highY)

xToY :: Dimension -> LineEquation -> Maybe Dimension
xToY _ (Horizontal y) = Just y
xToY _ (Vertical _) = Nothing
xToY x (Gradient m b) = Just $ (m * x) + b

yToX :: Dimension -> LineEquation -> Maybe Dimension
yToX _ (Horizontal _) = Nothing
yToX _ (Vertical x) = Just x
yToX y (Gradient m b) = Just $ (y - b) / m

intersection :: LineEquation -> LineEquation -> Maybe Point
intersection (Horizontal y) h = fmap (,y) . yToX y $ h
intersection (Vertical x) h = fmap (x,) . xToY x $ h
intersection g@(Gradient m b) (Gradient n c)
  | m /= n =
    let numerator = c - b
        denominator = m - n
        x = numerator / denominator
        y = xToY x g
     in (x,) <$> y
  | otherwise = Nothing
intersection g@(Gradient _ _) h = intersection h g

-- intersection h@(Horizontal _) v@(Vertical _) = intersection v h
-- intersection (Vertical x) (Horizontal y) = Just (x, y)
-- intersection h@(Horizontal _) g@(Gradient _ _) = intersection g h
-- intersection v@(Vertical _) g@(Gradient _ _) = intersection g v
-- intersection (Gradient m b) (Horizontal y)
-- intersection _ _ = Nothing

data LineSegment = LineSegment {_line :: LineEquation, _bound :: Bound} deriving (Eq, Show)

inBothArr b1 b2 = arr inBound b1 &&& arr inBound b2

inBoth b1 b2 = allOf both id . inBothArr b1 b2

sanityCheckPoint (Vertical x) (Horizontal y) (px, py) = x == px && y == py
sanityCheckPoint (Gradient m b) (Gradient n c) (px, py) =
  py == (m * px) + b
    && py == (n * px) + c
sanityCheckPoint (Gradient m b) (Horizontal y) (px, py) =
  py == y && ((m * px) + b) == py
sanityCheckPoint (Gradient m b) (Vertical x) (px, py) =
  px == x && ((m * px) + b) == py
sanityCheckPoint (Vertical x1) (Vertical x2) (px, _) = x1 == x2 && x2 == px
sanityCheckPoint (Horizontal y1) (Horizontal y2) (_, py) = y1 == y2 && y2 == py
sanityCheckPoint l1 l2 p = sanityCheckPoint l2 l1 p

sanityCheck (LineSegment l1 b1) (LineSegment l2 b2) (Just p) =
  if (sanityCheckPoint l1 l2 p) && (inBoth b1 b2 p) then Right () else Left "damn it"

sanityCheckOverlap _ _ Nothing = Right ()
sanityCheckOverlap ls1@(LineSegment l1 b1) ls2@(LineSegment l2 b2) (Just ls3@(LineSegment l3 b3)) =
  let equationEquality = l1 == l2 && l2 == l3
      boundIntersection = (Just b3) == (boundOverlap b1 b2)
      points = segmentPoints ls3
      pointsInBounds = all (\p -> inBound b1 p && inBound b2 p && inBound b3 p) points
   in if ls1 == ls2
        then Left "the same things are in here"
        else
          if equationEquality && boundIntersection && pointsInBounds
            then traverse_ (sanityCheck ls1 ls2) (map Just points)
            else Left "overlap is fucked"

boundCheck (((LineSegment _ b1), (LineSegment _ b2)), (Just ls@(LineSegment _ b3))) =
  let ps = segmentPoints ls
      arrowed = fmap (inBoth b1 b2) ps
      blarghed = fmap (inBound b3) ps
   in arrowed == blarghed

-- sanityCheck (LineSegment

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
    | l1 == l2 = LineSegment l1 <$> boundOverlap b1 b2
    | otherwise = Nothing

segmentPoints :: LineSegment -> [Point]
segmentPoints (LineSegment l b@(Bound (x1, y1) (x2, y2)))
  | (Horizontal y) <- l =
    if y1 <= y && y <= y2
      then (,y) <$> [x1 .. x2]
      else []
  | (Vertical x) <- l =
    if x1 <= x && x <= x2
      then ((,) x) <$> [y1 .. y2]
      else []
  | g@(Gradient _ _) <- l =
    let xs = [x1 .. x2]
        maybeYs = xToY <$> [x1 .. x2] <*> pure g
        ys = sequence maybeYs
        points = zip <$> pure xs <*> ys
     in filter (inBound b) (join . Maybe.maybeToList $ points)

segmentsDanger :: LineSegment -> LineSegment -> [Point]
segmentsDanger ls1 ls2
  | ls1 /= ls2 =
    let intersection = Maybe.maybeToList $ segmentIntersection ls1 ls2
        overlap = segmentOverlap ls1 ls2
        overlapSanity = sanityCheckOverlap ls1 ls2 overlap
        overlapPoints = if isRight overlapSanity then segmentPoints =<< (Maybe.maybeToList overlap) else error "fuuuuck"
     in overlapPoints ++ intersection
  | otherwise = []

ventLineToSegment :: VentLine -> LineSegment
ventLineToSegment (VentLine p1@(x1, y1) p2@(x2, y2))
  | x1 == x2 = LineSegment (Vertical x1) (bound p1 p2)
  | y1 == y2 = LineSegment (Horizontal y1) (bound p1 p2)
  | otherwise =
    let rise = y2 - y1
        run = x2 - x1
        m = rise / run
        x = x1
        y = y1
        b = y - (m * x)
     in LineSegment (Gradient m b) (bound p1 p2)

findDanger :: [LineSegment] -> [Point]
findDanger segments = do
  s1 <- segments
  s2 <- segments
  segmentsDanger s1 s2

countDistinct :: (Ord a) => [a] -> Int
countDistinct = Set.size . Set.fromList

countDanger = countDistinct . findDanger

fuckThis :: [LineSegment] -> Int
fuckThis segments =
  let allPoints = segmentPoints =<< segments
      pointCounts :: MMap.MonoidalMap Point (Sum Int)
      pointCounts = foldMap (\p -> MMap.singleton p (Sum 1)) allPoints
      interestingPoints = MMap.keys . MMap.filter (> 1) $ pointCounts
   in length interestingPoints

parseDimension :: MyParser Dimension
parseDimension = (% 1) <$> parseInt

parsePoint :: MyParser Point
parsePoint = (,) <$> parseDimension <* spaces <* char ',' <*> parseDimension

parseVentLine :: MyParser VentLine
parseVentLine = VentLine <$> parsePoint <* spaces <* string "->" <* spaces <*> parsePoint

-- parseLineSegment :: MyParser LineSegment
-- parseLineSegment = do
--   ventLine <- parseVentLine
--   let segment = ventLineToSegment ventLine
--   case segment of
--     Just s -> return s
--     Nothing -> parserFail $ "Invalid line segment: " ++ (show ventLine)

-- parseLineSegments :: MyParser [LineSegment]
-- parseLineSegments = many1 (parseLineSegment <* endOfLine) <* eof

parseLineSegment :: MyParser LineSegment
parseLineSegment = ventLineToSegment <$> parseVentLine

parseLineSegments :: MyParser [LineSegment]
parseLineSegments = many1 (parseLineSegment <* endOfLine) <* eof

avoidDanger :: IO ()
avoidDanger = do
  parsed <- parseStdin (parseLineSegments)
  print $ fmap (fuckThis) parsed
