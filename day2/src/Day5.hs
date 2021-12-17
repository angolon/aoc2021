{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Day5 where

import Control.Arrow
import Control.Lens
import Control.Monad
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

parsePoint :: MyParser Point
parsePoint = (,) <$> parseInt <* spaces <* char ',' <*> parseInt

parseVentLine :: MyParser VentLine
parseVentLine = VentLine <$> parsePoint <* spaces <* string "->" <* spaces <*> parsePoint

parseVentLines :: MyParser [VentLine]
parseVentLines = many1 (parseVentLine <* endOfLine) <* eof

avoidDanger :: IO ()
avoidDanger = do
  parsed <- parseStdin parseVentLines
  print parsed
