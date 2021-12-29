{-# LANGUAGE MultiWayIf #-}
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
    & minX -~ 2
    & maxX +~ 2
    & minY -~ 2
    & maxY +~ 2

inBounds :: (Int, Int) -> Bounds -> Bool
inBounds (x, y) (Bounds minX maxX minY maxY) =
  (minX <= x && x <= maxX)
    && (minY <= y && y <= maxY)

outOfBounds :: (Int, Int) -> Bounds -> Bool
outOfBounds xy = not . inBounds xy

type Algorithm = V.Vector Bool

data Image = Image
  { _litPixels :: Set (Int, Int),
    _knownBounds :: Bounds,
    _voidLit :: (Int, Int) -> Bool
  }
  deriving (Show)

makeLenses ''Image

unlitVoid = const False

litVoid = const True

isLit :: (Int, Int) -> Image -> Bool
isLit xy img =
  if xy `inBounds` (img ^. knownBounds)
    then xy `member` (img ^. litPixels)
    else (img ^. voidLit) xy

parsePixel :: MyParser Bool
parsePixel =
  let light = const True <$> char '#'
      dark = const False <$> char '.'
   in light <|> dark

parseAlgorithm :: MyParser Algorithm
parseAlgorithm = V.fromList <$> count 512 parsePixel

pixelBounds :: Set (Int, Int) -> Bounds
pixelBounds img =
  fromJust $
    Bounds
      <$> minimumOf (folded . _1) img
      <*> maximumOf (folded . _1) img
      <*> minimumOf (folded . _2) img
      <*> maximumOf (folded . _2) img

toImage :: V.Vector (V.Vector Bool) -> Image
toImage pss =
  let litPixels = fold $ do
        (y, ps) <- V.indexed pss
        (x, p) <- V.indexed ps
        guard p
        return $ Set.singleton (x, y)
      bounds = pixelBounds litPixels
   in Image litPixels bounds unlitVoid -- the void starts as unlit

parseImage :: MyParser Image
parseImage =
  let parseLine = V.fromList <$> many1 parsePixel
      parseLines = V.fromList <$> sepEndBy1 parseLine endOfLine <* eof
   in toImage <$> parseLines

parsePuzzle :: MyParser (Algorithm, Image)
parsePuzzle = (,) <$> parseAlgorithm <* skipMany1 endOfLine <*> parseImage

enhancementWindow x y = do
  y' <- [(y - 1) .. (y + 1)]
  x' <- [(x - 1) .. (x + 1)]
  return (x', y')

windowBits :: Image -> Int -> Int -> Int
windowBits img x y =
  let bits = (`isLit` img) <$> enhancementWindow x y
   in toInt bits

enhance :: Algorithm -> Image -> Image
enhance alg img =
  let nextBounds = expandBounds . _knownBounds $ img
      bits = windowBits img
      -- expanded bounds expand far enough into the void
      -- that we can test them for whether or not we should
      -- swap the void's state.
      bottomLeft = (nextBounds ^. minX, nextBounds ^. minY)
      currentlyLitVoid = bottomLeft `isLit` img
      nextLit = fold $ do
        x <- [nextBounds ^. minX .. nextBounds ^. maxX]
        y <- [nextBounds ^. minY .. nextBounds ^. maxY]
        let idx = bits x y
        let isLit = alg ! idx
        guard isLit
        return $ Set.singleton (x, y)
      nextLitVoid = bottomLeft `member` nextLit
      nextVoidF =
        if
            | not currentlyLitVoid && nextLitVoid -> litVoid
            | currentlyLitVoid && nextLitVoid -> error "I can't represent non-alternating infinite voids"
            | otherwise -> unlitVoid
   in Image nextLit nextBounds nextVoidF

printImage :: Image -> IO ()
printImage img =
  let bounds = img ^. knownBounds
      fmtRow y = do
        x <- [bounds ^. minX .. bounds ^. maxX]
        if (x, y) `isLit` img
          then return '#'
          else return '.'
   in traverse_ (putStrLn . fmtRow) [bounds ^. minY .. bounds ^. maxY]

iterations = 50

zoomAndEnhance :: IO ()
zoomAndEnhance = do
  (Right (alg, img)) <- parseStdin parsePuzzle
  let algEnhance = Endo $ enhance alg
  let iteratedAlgorithm = appEndo . fold . replicate iterations $ algEnhance
  let enhanced = iteratedAlgorithm img
  printImage enhanced
  print $ enhanced ^. knownBounds
  print $ Set.size (enhanced ^. litPixels)

-- printImage enhanced
