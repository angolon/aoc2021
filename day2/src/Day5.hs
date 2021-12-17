{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Day5 where

import Control.Lens
import Lib (MyParser, parseInt, parseStdin)
import Text.Parsec
import Text.Parsec.Char

type Point = (Int, Int)

data VentLine = VentLine {_a :: Point, _b :: Point} deriving (Eq, Show)

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
