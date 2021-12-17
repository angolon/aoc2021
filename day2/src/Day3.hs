module Day3 (parseBinary, test) where

import Data.Bits
import Data.Foldable (foldl')
import Lib (MyParser, parseStdin)
import Text.Parsec
import Text.Parsec.Char

parseBit :: MyParser Int
parseBit =
  let zero = char '0'
      one = char '1'
      toBool c = case c of
        '0' -> 0
        '1' -> 1
      p = zero <|> one
   in toBool <$> p

parseBinary :: MyParser Int
parseBinary =
  let setBitValue True = setBit
      setBitValue False = clearBit
      setBits bools =
        foldl'
          ( \bits b ->
              (shiftL bits 1) .|. b
          )
          0
          bools
   in fmap setBits (many1 parseBit)

parseBinaries :: MyParser [Int]
parseBinaries = many (parseBinary <* endOfLine) <* eof

test :: IO ()
test = do
  parsed <- parseStdin parseBinaries
  print parsed
