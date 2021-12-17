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

parseTemplate :: MyParser String
parseTemplate = many1 alphaNum <* endOfLine

type InsertionRule = ((Char, Char), Char)

parseRule :: MyParser InsertionRule
parseRule =
  let k = (,) <$> alphaNum <*> alphaNum
   in (,) <$> k <* string " -> " <*> alphaNum

parseRules :: MyParser (Map (Char, Char) Char)
parseRules = Map.fromList <$> sepEndBy1 parseRule endOfLine <* eof

parsePuzzle :: MyParser (String, Map (Char, Char) Char)
parsePuzzle = (,) <$> parseTemplate <* endOfLine <*> parseRules

sliding2 :: (a -> a -> b) -> [a] -> [b]
sliding2 f as =
  let zippedBs = f <$> (ZipList as) <*> (ZipList (drop 1 as))
   in getZipList zippedBs

insertElements :: Map (Char, Char) Char -> String -> String
insertElements rules s =
  let insert a b = (a, (rules ! (a, b)), b)
      inserted = sliding2 insert s
      smooth ((a, b, c) : []) = [a, b, c]
      smooth ((a, b, c) : ds) = [a, b] ++ smooth ds
      smooth [] = ""
   in smooth inserted

synthesizeN :: Map (Char, Char) Char -> Int -> String -> String
synthesizeN _ 0 s = s
synthesizeN rules n s = synthesizeN rules (n - 1) (insertElements rules s)

finalizePolymer :: String -> Int
finalizePolymer s =
  let grouped = NonEmpty.groupAllWith id s
      counts = fmap NonEmpty.length grouped
      max = maximum counts
      min = minimum counts
   in max - min

synthesizePolymer :: IO ()
synthesizePolymer = do
  parsed <- parseStdin parsePuzzle
  let (Right (template, rules)) = parsed
  print . finalizePolymer $ (synthesizeN rules 10 template)
