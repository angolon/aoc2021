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

type Digraphs = MonoidalMap (Char, Char) (Sum Integer)

buildDigraphs :: String -> Digraphs
buildDigraphs s =
  let dgs = sliding2 (,) s
      dg1s = fmap (,(Sum 1)) dgs
   in foldMap (uncurry MMap.singleton) dg1s

insertElements :: Map (Char, Char) Char -> Digraphs -> Digraphs
insertElements rules digraphs =
  let insertElement digraph@(a, b) count =
        let c = rules ! digraph
         in MMap.fromList [((a, c), count), ((c, b), count)]
      additions = MMap.foldMapWithKey insertElement digraphs
   in additions

synthesizeN :: Map (Char, Char) Char -> Int -> Digraphs -> Digraphs
synthesizeN _ 0 s = s
synthesizeN rules n s = synthesizeN rules (n - 1) (insertElements rules s)

printMap :: (Show k, Show a) => MonoidalMap k a -> IO ()
printMap m = MMap.traverseWithKey (\k a -> print (k, a)) m >>= (\_ -> return ())

finalizePolymer :: String -> Digraphs -> Integer
finalizePolymer s digraphs =
  let cs = MMap.foldMapWithKey (\(a, _) count -> MMap.singleton a count) digraphs
      ds = MMap.foldMapWithKey (\(_, b) count -> MMap.singleton b count) digraphs
      initial = MMap.singleton (head s) (Sum 1)
      fin = MMap.singleton (last s) (Sum 1)
      characters = cs <> ds <> initial <> fin
      max = (getSum $ maximum characters) `div` 2
      min = (getSum $ minimum characters) `div` 2
      r = max - min
   in r

synthesizePolymer :: IO ()
synthesizePolymer = do
  parsed <- parseStdin parsePuzzle
  let (Right (template, rules)) = parsed
  let digraphs = buildDigraphs template
  -- (MMap.traverseWithKey (\k a -> print (k, a)) (synthesizeN rules 10 digraphs))
  -- return ()
  print . finalizePolymer template $ (synthesizeN rules 40 digraphs)
