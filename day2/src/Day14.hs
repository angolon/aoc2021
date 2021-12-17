{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Day14 where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Bifunctor.Swap (swap)
import Data.Foldable
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MMap
import Data.Maybe (isNothing, maybeToList)
import Data.Monoid (Sum (..), getSum)
import Data.Set (Set, (\\))
import qualified Data.Set as Set
import Data.Vector ((!?))
import qualified Data.Vector as V
import GHC.Generics
import Lib (MyParser, parseInt, parseStdin)
import Text.Parsec
import Text.Parsec.Char

type Digram = (Char, Char)

type Rules = Map Digram Char

type DigramCounts = MonoidalMap Digram (Sum Integer)

parseTemplate :: MyParser String
parseTemplate = many1 alphaNum <* endOfLine

parseRule :: MyParser (Digram, Char)
parseRule =
  let k = (,) <$> alphaNum <*> alphaNum
   in (,) <$> k <* string " -> " <*> alphaNum

parseRules :: MyParser Rules
parseRules = Map.fromList <$> sepEndBy1 parseRule endOfLine <* eof

parsePuzzle :: MyParser (String, Rules)
parsePuzzle = (,) <$> parseTemplate <* endOfLine <*> parseRules

sliding2 :: (a -> a -> b) -> [a] -> [b]
sliding2 f as =
  let zippedBs = f <$> (ZipList as) <*> (ZipList (drop 1 as))
   in getZipList zippedBs

buildDigrams :: String -> DigramCounts
buildDigrams s =
  let digrams = sliding2 (,) s
      digram1s = fmap (,(Sum 1)) digrams
   in foldMap (uncurry MMap.singleton) digram1s

-- Split every digram into two digrams according to the
-- rules lookup.
insertElements :: Rules -> DigramCounts -> DigramCounts
insertElements rules =
  let insertElement digram@(a, b) count =
        let c = rules ! digram
         in MMap.fromList [((a, c), count), ((c, b), count)]
   in MMap.foldMapWithKey insertElement

synthesizeN :: Rules -> Int -> DigramCounts -> DigramCounts
synthesizeN _ 0 s = s
synthesizeN rules n s = synthesizeN rules (n - 1) (insertElements rules s)

finalizePolymer :: String -> DigramCounts -> Integer
finalizePolymer s digrams =
  let countChars :: Digram -> (Sum Integer) -> MonoidalMap Char (Sum Integer)
      countChars digram count = foldMapOf both (flip MMap.singleton $ count) digram
      charCounts = MMap.foldMapWithKey countChars digrams
      -- Compensate for the fact that the first and last
      -- character are only part of one digram.
      initial = MMap.singleton (head s) (Sum 1)
      fin = MMap.singleton (last s) (Sum 1)
      adjustedCounts = charCounts <> initial <> fin
      max = (getSum $ maximum adjustedCounts) `div` 2
      min = (getSum $ minimum adjustedCounts) `div` 2
   in max - min

synthesizePolymer :: IO ()
synthesizePolymer = do
  parsed <- parseStdin parsePuzzle
  let (Right (template, rules)) = parsed
  let digrams = buildDigrams template
  print . finalizePolymer template $ (synthesizeN rules 40 digrams)
