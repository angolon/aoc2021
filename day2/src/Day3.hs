module Day3 where

import Control.Applicative (Const (..), ZipList (..))
import Data.Bits
import Data.Foldable (foldl', maximumBy, minimumBy)
import Data.Function (on)
import Data.Functor.Compose
import qualified Data.List as List
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
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

modeBit :: BitCounts -> Bool
modeBit counts =
  let countsL = MMap.toList counts
      cmp = compare `on` snd
      (minB, min) = minimumBy cmp countsL
      (maxB, max) = maximumBy cmp countsL
   in if min == max then True else maxB

gammaRate :: (ZipList BitCounts) -> Int
gammaRate counts =
  let modalBits = fmap modeBit counts
   in toInt . getZipList $ modalBits

gammaEpsilon :: (ZipList BitCounts) -> Int
gammaEpsilon counts =
  let len = length counts
      mask = toInt $ replicate len True
      γ = gammaRate counts
      ε = complement γ .&. mask
   in γ * ε

-- if this bit matches the bit condition for the current bit heads
type BitCondition = [Bool] -> Bool -> Bool

o2BitCondition :: BitCondition
o2BitCondition bitHeads bit =
  let baseCounts = MMap.fromList [(True, 0), (False, 0)]
      countHeads = baseCounts <> foldMap countBit bitHeads
      mode = modeBit countHeads
   in bit == mode

co2BitCondition :: BitCondition
co2BitCondition bitHeads bit =
  let countHeads = foldMap countBit bitHeads
      mode = modeBit countHeads
   in bit == not mode

conditionRating :: BitCondition -> [[Bool]] -> Maybe [Bool]
conditionRating bitCondition bits =
  let filterMatches :: (a -> Bool) -> a -> b -> c -> Maybe (b, c)
      filterMatches condition bit digits bitTails =
        if condition bit
          then Just (digits, bitTails)
          else Nothing
      step :: [[Bool]] -> [[Bool]] -> Int -> Maybe [Bool]
      step remainingDigits remainingBits i = do
        (bitHeads, bitTails) <- List.uncons remainingBits
        let transposedTails =
              if null bitTails
                then List.replicate (length bitHeads) []
                else List.transpose bitTails
        let theseBitsCondition = bitCondition bitHeads
        let matches = filterMatches theseBitsCondition <$> ZipList bitHeads <*> ZipList remainingDigits <*> ZipList (transposedTails)
        let fooy = sequence . filter isJust . getZipList $ matches
        if isNothing fooy then error "god damn it" else Just ()
        filtered <- sequence . filter isJust . getZipList $ matches

        let (remainingDigits', remainingBits') = unzip filtered
        if length remainingDigits' == 0
          then error (show (remainingDigits, remainingBits))
          else
            if length remainingDigits' == 1
              then return . head $ (remainingDigits')
              else step remainingDigits' (List.transpose remainingBits') (i + 1)
   in step bits (List.transpose bits) 0

o2Rating = (fmap toInt) . conditionRating o2BitCondition

co2Rating = (fmap toInt) . conditionRating co2BitCondition

-- survivors <- filterMatches mode <$> ZipList bitHeads <*> ZipList bits
-- 7

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

lifeSupportRating :: [[Bool]] -> Maybe Int
lifeSupportRating bits =
  (*) <$> o2Rating bits <*> co2Rating bits

test :: IO ()
test = do
  parsed <- parseStdin parseBitLists
  let result = fmap lifeSupportRating parsed
  print result

-- print blah
