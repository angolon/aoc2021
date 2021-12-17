{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Day8 where

import Data.Bits
import Data.Foldable
import qualified Data.List.NonEmpty as NE
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Monoid (Sum (..), getSum)
import Data.Word
import Lib (MyParser, parseStdin)
import qualified SevenSegment as SS
import Text.Parsec
import Text.Parsec.Char

data Display = Display {_patterns :: [SS.SevenSegment], _outputs :: [SS.SevenSegment]} deriving (Eq, Show)

parseNPatterns :: Int -> MyParser [SS.SevenSegment]
parseNPatterns n = (:) <$> SS.parseSevenSegment <*> (count (n - 1) (space *> SS.parseSevenSegment))

parseDisplay :: MyParser Display
parseDisplay = Display <$> parseNPatterns 10 <* space <* char '|' <* space <*> parseNPatterns 4 <* endOfLine

parseDisplays :: MyParser [Display]
parseDisplays = many1 parseDisplay <* eof

solve :: Display -> Int
solve (Display patterns outputs) =
  let numBitsMap = SS.groupByNumBits patterns
      one = NE.head $ numBitsMap ! (popCount SS.one)
      four = NE.head $ numBitsMap ! (popCount SS.four)
      seven = NE.head $ numBitsMap ! (popCount SS.seven)
      eight = NE.head $ numBitsMap ! (popCount SS.eight)
      ne235 = numBitsMap ! (popCount SS.two)
      ne069 = numBitsMap ! (popCount SS.zero)
      (Just three) = find (\x -> all (\y -> x == y || ((SS.numUniqueBits x y) == 1)) ne235) ne235
      (Just two) = find (\x -> [1, 1, 2, 1, 2] == (SS.numUniqueBits <$> [one, three, four, seven, eight] <*> [x])) ne235
      (Just five) = find (\x -> x /= three && x /= two) ne235
      matchRemaining counts =
        find
          ( \x ->
              counts == (SS.numUniqueBits <$> [one, two, three, four, five, seven, eight] <*> [x])
          )
          ne069
      (Just zero) = matchRemaining [0, 1, 1, 1, 1, 0, 1]
      (Just six) = matchRemaining [1, 1, 1, 1, 0, 1, 1]
      (Just nine) = matchRemaining [0, 1, 0, 0, 0, 0, 1]
      descrambler =
        Map.fromList
          [ (zero, 0),
            (one, 1),
            (two, 2),
            (three, 3),
            (four, 4),
            (five, 5),
            (six, 6),
            (seven, 7),
            (eight, 8),
            (nine, 9)
          ]
   in foldl' (\value digit -> (value * 10) + (descrambler ! digit)) 0 outputs

solveMany :: [Display] -> Int
solveMany = getSum . foldMap (Sum . solve)

descrambleDisplays :: IO ()
descrambleDisplays = do
  parsed <- parseStdin parseDisplays
  print $ fmap solveMany parsed
