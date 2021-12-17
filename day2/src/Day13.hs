{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Day13 where

import Control.Lens
import Control.Monad
import Data.Bifunctor.Swap (swap)
import Data.Foldable
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
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

data Fjald = AlongX Int | AlongY Int deriving (Eq, Show)

parseFjald :: MyParser Fjald
parseFjald =
  let x = AlongX <$> (string "y=" *> parseInt)
      y = AlongY <$> (string "x=" *> parseInt)
      preamble = string "fold along "
   in preamble *> (x <|> y)

parseFjalds :: MyParser [Fjald]
parseFjalds = sepEndBy1 parseFjald endOfLine <* eof

parseDot :: MyParser (Int, Int)
parseDot = (,) <$> parseInt <* char ',' <*> parseInt

parseDots :: MyParser [(Int, Int)]
parseDots = sepEndBy1 parseDot endOfLine

parseInstructions :: MyParser ([(Int, Int)], [Fjald])
parseInstructions = (,) <$> parseDots <* endOfLine <*> parseFjalds

reflectAround :: Int -> Int -> Int
reflectAround around a =
  if a <= around
    then a
    else
      let distance = a - around
       in around - distance

runFjald :: Set (Int, Int) -> Fjald -> Set (Int, Int)
runFjald points fjald =
  let ps = Set.toList points
      reflecter field a = ps & (mapped . field) %~ reflectAround a
      os = case fjald of
        (AlongX y) -> reflecter _2 y
        (AlongY x) -> reflecter _1 x
   in Set.fromList os

runFjalds ps fjalds = foldl' runFjald ps fjalds

formatPoints :: Set (Int, Int) -> IO ()
formatPoints ps =
  let maxX = maximum . fmap fst . Set.toList $ ps
      maxY = maximum . fmap snd . Set.toList $ ps
      fmtP (x, y) = if Set.member (x, y) ps then '#' else '.'
      formatLn y = (fmtP . (,y)) <$> [0 .. maxX]
   in traverse_ (print . formatLn) [0 .. maxY]

foldOrigami :: IO ()
foldOrigami = do
  parsed <- parseStdin parseInstructions
  let (Right (points, instructions)) = parsed
  let r = runFjalds (Set.fromList points) instructions
  formatPoints r
