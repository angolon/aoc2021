module Day3 (test) where

import Control.Applicative (Const (..), ZipList (..))
import Data.Bits
import Data.Foldable (foldl', maximumBy)
import Data.Function (on)
import Data.Functor.Compose
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid (Sum (..))
import Lib (MyParser, parseStdin)
import Text.Parsec
import Text.Parsec.Char

parseBit :: MyParser Bool
parseBit =
  let zero = char '0'
      one = char '1'
      toBool c = case c of
        '0' -> False
        '1' -> True
      p = zero <|> one
   in toBool <$> p

parseBits :: MyParser [Bool]
parseBits = many1 parseBit <* endOfLine

parseBitLists :: MyParser [[Bool]]
parseBitLists = many1 parseBits <* eof

type BitCounts = MonoidalMap Bool (Sum Int)

countBit :: Bool -> BitCounts
countBit b = MMap.singleton b (Sum 1)

type ZippedMaps k = Compose ZipList (MonoidalMap k)

-- bitFrequencies :: [[Bool]] -> [ZippedMaps Bool (Sum Int)]
-- bitFrequencies = foldMap (fmap countBit)
-- zippedBits :: [[Bool]] -> [ZipList Bool]
-- zippedBits = traverse (ZipList . (fmap countBit))
bitFrequencies :: [[Bool]] -> ZipList BitCounts
bitFrequencies =
  let baseFreq = traverse (ZipList . (fmap countBit))
      collapse = fmap (foldMap id)
   in collapse . baseFreq

toInt :: [Bool] -> Int
toInt bs =
  let bitValue True = 1
      bitValue False = 0
   in foldl' (\bits b -> (shiftL bits 1) .|. (bitValue b)) 0 bs

gammaRate :: (ZipList BitCounts) -> Int
gammaRate counts =
  let modalBits = fmap (fst . maximumBy (compare `on` (getSum . snd)) . MMap.toList) counts
   in toInt . getZipList $ modalBits

gammaEpsilon :: (ZipList BitCounts) -> Int
gammaEpsilon counts =
  let len = length counts
      mask = toInt $ replicate len True
      γ = gammaRate counts
      ε = complement γ .&. mask
   in γ * ε

-- parseBinary :: MyParser Int
-- parseBinary =
--   let setBitValue True = setBit
--       setBitValue False = clearBit
--       setBits bools =
--         foldl'
--           ( \bits b ->
--               (shiftL bits 1) .|. b
--           )
--           0
--           bools
--    in fmap setBits (many1 parseBit)

-- parseBinaries :: MyParser [Int]
-- parseBinaries = many (parseBinary <* endOfLine) <* eof

test :: IO ()
test = do
  parsed <- parseStdin parseBitLists
  let result = fmap (gammaEpsilon . bitFrequencies) parsed
  print result

-- print blah
