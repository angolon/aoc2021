{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Day8 where

import Data.Bits
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

descrambleDisplays :: IO ()
descrambleDisplays = do
  parsed <- parseStdin parseDisplays
  print parsed
