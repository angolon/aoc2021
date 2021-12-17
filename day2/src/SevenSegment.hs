module SevenSegment where

import Data.Bits
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
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

digits :: [SevenSegment]
digits = [zero, one, two, three, four, five, six, seven, eight, nine]

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

numCommonBits :: SevenSegment -> SevenSegment -> Int
numCommonBits a b = popCount $ a .&. b

uniqueBits :: SevenSegment -> SevenSegment -> SevenSegment
uniqueBits a b = a .&. (a `xor` b)

numUniqueBits :: SevenSegment -> SevenSegment -> Int
numUniqueBits a b = popCount $ uniqueBits a b

uniqueBitsAll :: SevenSegment -> [SevenSegment] -> SevenSegment
uniqueBitsAll a = foldl' uniqueBits a

keyBy :: (a -> k) -> a -> (k, a)
keyBy f a = (f a, a)

groupByNumBits :: [SevenSegment] -> Map Int (NonEmpty SevenSegment)
groupByNumBits sss = Map.fromList . fmap (keyBy $ popCount . NE.head) . NE.groupAllWith popCount $ sss

numBitsToDigits :: Map Int (NonEmpty SevenSegment)
numBitsToDigits = groupByNumBits digits

-- Maps un-scrambled digits
-- numUniqueBits :: Map SevenSegment Int
-- numUniqueBits =
--   let kds = do
--         k <- digits
--         let otherDigits = filter (/= k) digits
--         let u = uniqueBitsAll k otherDigits
--         return (k, popCount u)
--    in Map.fromList kds
