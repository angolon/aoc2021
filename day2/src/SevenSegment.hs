module SevenSegment where

import Data.Bits
import Data.Foldable (foldl')
import Data.Word
import Lib (MyParser)
import Text.Parsec
import Text.Parsec.Char

type SevenSegment = Word8

a :: SevenSegment
a = 1

b :: SevenSegment
b = 2

c :: SevenSegment
c = 4

d :: SevenSegment
d = 8

e :: SevenSegment
e = 16

f :: SevenSegment
f = 32

g :: SevenSegment
g = 64

zero = a .|. b .|. c .|. e .|. f .|. g

one = c .|. f

two = a .|. c .|. d .|. e .|. g

three = a .|. c .|. d .|. f .|. g

four = b .|. c .|. d .|. f

five = a .|. b .|. d .|. f .|. g

six = a .|. b .|. d .|. e .|. f .|. g

seven = a .|. c .|. f

eight = a .|. b .|. c .|. d .|. e .|. f .|. g

nine = a .|. b .|. c .|. d .|. f .|. g

lookupBitChar :: Char -> SevenSegment
lookupBitChar ch = case ch of
  'a' -> a
  'b' -> b
  'c' -> c
  'd' -> d
  'e' -> e
  'f' -> f
  'g' -> g
  _ -> error ("Couldn't match char: " ++ show ch)

parseBitChar :: MyParser Char
parseBitChar = char 'a' <|> char 'b' <|> char 'c' <|> char 'd' <|> char 'e' <|> char 'f' <|> char 'g'

parseBit :: MyParser SevenSegment
parseBit = lookupBitChar <$> parseBitChar

parseSevenSegment :: MyParser SevenSegment
parseSevenSegment = foldl' (.|.) 0 <$> many1 parseBit
