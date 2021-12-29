{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Day20 where

import Control.Arrow (arr, (&&&))
import Control.Lens
import Control.Monad
import Data.Either (either)
import Data.Foldable
import Data.Function (on)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MMap
import Data.Maybe
import Data.Monoid (Endo (..), Sum (..), getSum)
import Data.PQueue.Min (MinQueue)
import qualified Data.PQueue.Min as MinQueue
import Data.Ratio
import Data.Set (Set, member, union, (\\))
import qualified Data.Set as Set
import Data.Vector ((!))
import qualified Data.Vector as V
import Day3 (toInt)
import Lib (MyParser, parseStdin)
import Linear.Metric (dot)
import qualified Linear.Quaternion as Q
import Text.Parsec
import Text.Parsec.Char
import Text.Show.Functions

type Algorithm = V.Vector Bool

type Image = Set (Int, Int)

parsePixel :: MyParser Bool
parsePixel =
  let light = const True <$> char '#'
      dark = const False <$> char '.'
   in light <|> dark

parseAlgorithm :: MyParser Algorithm
parseAlgorithm = V.fromList <$> count 512 parsePixel

parseImage :: MyParser Image
parseImage =
  let parseLine = V.fromList <$> many1 parsePixel
      parseLines = V.fromList <$> sepEndBy1 parseLine endOfLine <* eof
      toImage pss = do
        (y, ps) <- V.indexed pss
        (x, p) <- V.indexed ps
        guard p
        return $ Set.singleton (x, y)
   in fold . toImage <$> parseLines

parsePuzzle :: MyParser (Algorithm, Image)
parsePuzzle = (,) <$> parseAlgorithm <* skipMany1 endOfLine <*> parseImage

data Bounds = Bounds
  { _minX :: Int,
    _maxX :: Int,
    _minY :: Int,
    _maxY :: Int
  }
  deriving (Eq, Show)

makeLenses ''Bounds

expandBounds :: Bounds -> Bounds
expandBounds b =
  b
    & minX -~ 1
    & maxX +~ 1
    & minY -~ 1
    & maxY +~ 1

imageBounds :: Image -> Bounds
imageBounds img =
  fromJust $
    Bounds
      <$> minimumOf (folded . _1) img
      <*> maximumOf (folded . _1) img
      <*> minimumOf (folded . _2) img
      <*> maximumOf (folded . _2) img

-- enhancementWindow x y = (,) <$> [(x - 1) .. (x + 1)] <*> [(y - 1) .. (y + 1)]
enhancementWindow x y = do
  y' <- [(y - 1) .. (y + 1)]
  x' <- [(x - 1) .. (x + 1)]
  return (x', y')

windowBits :: Image -> Int -> Int -> Int
windowBits img x y =
  let bits = (`member` img) <$> enhancementWindow x y
   in toInt bits

enhance :: Algorithm -> Image -> Image
enhance alg img =
  let nextBounds = expandBounds . imageBounds $ img
      bits = windowBits img
      nextLit = do
        x <- [nextBounds ^. minX .. nextBounds ^. maxX]
        y <- [nextBounds ^. minY .. nextBounds ^. maxY]
        let idx = bits x y
        let isLit = alg ! idx
        guard isLit
        return $ Set.singleton (x, y)
   in fold nextLit

printImage :: Image -> IO ()
printImage img =
  let bounds = imageBounds img
      fmtRow y = do
        x <- [bounds ^. minX .. bounds ^. maxX]
        if (x, y) `member` img
          then return '#'
          else return '.'
   in traverse_ (putStrLn . fmtRow) [bounds ^. minY .. bounds ^. maxY]

-- let idxPixel lineOffset columnOffset = do
--       pos <- getPosition
--       let line = (sourceLine pos) - lineOffset
--       let col = (sourceColumn pos) - columnOffset
--       p <- parsePixel
--       return (p, (line, col))
--  in undefined
zoomAndEnhance :: IO ()
zoomAndEnhance = do
  (Right (alg, img)) <- parseStdin parsePuzzle
  let algEnhance = enhance alg
  -- let enhanced = algEnhance . algEnhance $ img
  let enhanced = img
  print alg
  print $ Set.size enhanced
  print $ imageBounds enhanced
  printImage enhanced

-- printImage enhanced
