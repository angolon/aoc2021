{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Day18 where

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
import Data.Set (Set, union, (\\))
import qualified Data.Set as Set
import Data.Vector ((!), (!?))
import qualified Data.Vector as V
import Lib (MyParser, parseInt, parseStdin)
import Text.Parsec
import Text.Parsec.Char

data SnailNum a
  = Terminal {_value :: a}
  | Fork {_left :: (SnailNum a), _right :: (SnailNum a)}
  deriving (Eq)

instance (Show a) => Show (SnailNum a) where
  show (Terminal a) = show a
  show (Fork l r) = "[" ++ show l ++ "," ++ show r ++ "]"

makeLenses ''SnailNum

instance Functor SnailNum where
  fmap f (Terminal a) = Terminal $ f a
  fmap f (Fork l r) = Fork (fmap f l) (fmap f r)

instance Foldable SnailNum where
  foldMap f (Terminal a) = f a
  foldMap f (Fork l r) = foldMap f l <> foldMap f r

instance Traversable SnailNum where
  traverse f (Terminal a) = Terminal <$> f a
  traverse f (Fork l r) = Fork <$> traverse f l <*> traverse f r

parseSnailNum :: MyParser (SnailNum Int)
parseSnailNum =
  let terminal = Terminal <$> parseInt
      pair = Fork <$> parseSnailNum <* char ',' <*> parseSnailNum
      fork = between (char '[') (char ']') pair
   in fork <|> terminal

parseSnailNums :: MyParser [SnailNum Int]
parseSnailNums = sepEndBy1 parseSnailNum endOfLine <* eof

annotateDepth :: SnailNum a -> SnailNum (Int, a)
annotateDepth sna =
  let go depth (Terminal a) = Terminal (depth, a)
      go depth (Fork l r) = Fork (go (depth + 1) l) (go (depth + 1) r)
   in go 0 sna

justOr (Just a) _ = Just a
justOr _ b = b

data Step = L | R deriving (Show, Eq)

swap L = R
swap R = L

stepToLens L = left
stepToLens R = right

-- Paths are always in reverse order from the root
type Path = [Step]

type Hackery f a = SnailNum a -> f (SnailNum a)

pathToLens :: (Applicative f) => Path -> Hackery f a -> Hackery f a
pathToLens = appEndo . foldMap (Endo . stepToLens) . reverse

split (Terminal a) =
  Fork (Terminal . floor $ (a % 2)) (Terminal . ceiling $ (a % 2))

downLeft :: Path -> SnailNum a -> Path
downLeft path (Terminal _) = path
downLeft path (Fork l _) = downLeft (L : path) l

downRight :: Path -> SnailNum a -> Path
downRight path (Terminal _) = path
downRight path (Fork _ r) = downRight (R : path) r

downLR L = downLeft
downLR R = downRight

incrementNode root n path =
  root & ((pathToLens path) . value) +~ n

updateLRSibling lr root n path =
  let unwound = dropWhile (== lr) path
      rl = swap lr
      pathToSibling = case unwound of
        (rl) : tail ->
          let swapped = lr : tail
              branch = root ^? (pathToLens swapped)
           in downLR rl swapped <$> branch
        [] -> Nothing
   in incrementNode root n <$> pathToSibling

updateRightSibling = updateLRSibling R

updateLeftSibling = updateLRSibling L

reduceStep :: SnailNum Int -> Maybe (SnailNum Int)
reduceStep sna =
  let explode path a b pair =
        let updatedLeft = updateLeftSibling sna a path
            defaultedLeft = fromMaybe sna updatedLeft
            updatedRight = updateRightSibling defaultedLeft b path
            defaultedRight = fromMaybe defaultedLeft updatedRight
            replaced = defaultedRight & (pathToLens path) .~ (Terminal 0)
         in Just replaced
      goExplode depth path fork@(Fork l@(Terminal a) r@(Terminal b))
        | depth >= 4 =
          let result = explode path a b fork
           in result
      goExplode depth path (Fork l r) =
        let lr = goExplode (depth + 1) (L : path) l
            rr = goExplode (depth + 1) (R : path) r
         in lr `justOr` rr
      goExplode _ _ (Terminal _) = Nothing
      goSplit path (Fork l r) =
        (goSplit (L : path) l) `justOr` (goSplit (R : path) r)
      goSplit path (Terminal a) =
        if a >= 10
          then Just (sna & (pathToLens path) %~ split)
          else Nothing
      exploded = goExplode 0 [] sna
      splitted = goSplit [] sna
   in exploded `justOr` splitted -- thank god for laziness

reduceSnailNum :: SnailNum Int -> SnailNum Int
reduceSnailNum sna =
  case reduceStep sna of
    Just snb -> reduceSnailNum snb
    _ -> sna

appendSnailNum :: SnailNum Int -> SnailNum Int -> SnailNum Int
appendSnailNum l r = reduceSnailNum (Fork l r)

sumSnailNums :: [SnailNum Int] -> SnailNum Int
sumSnailNums [] = error "nope"
sumSnailNums (n : ns) = foldl' appendSnailNum n ns

magnitude :: SnailNum Int -> Int
magnitude (Terminal a) = a
magnitude (Fork l r) = ((3 *) . magnitude $ l) + ((2 *) . magnitude $ r)

largestMagnitude :: [SnailNum Int] -> Int
largestMagnitude ns =
  let combinations = do
        n <- ns
        m <- ns
        guard (m /= n)
        return $ appendSnailNum n m
      magnitudes = fmap magnitude combinations
   in maximum magnitudes

doMathsHomework :: IO ()
doMathsHomework = do
  (Right parsed) <- parseStdin parseSnailNums
  let n = sumSnailNums parsed
  print n
  print $ largestMagnitude parsed
