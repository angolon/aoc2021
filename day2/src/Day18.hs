{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Day18 where

import Control.Lens
import Control.Monad
import Data.Bifunctor.Swap (swap)
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

stepToLens L = left
stepToLens R = right

-- Paths are always in reverse order from the root
type Path = [Step]

type Hackery f a = SnailNum a -> f (SnailNum a)

pathToLens :: (Applicative f) => Path -> Hackery f a -> Hackery f a
pathToLens = appEndo . foldMap (Endo . stepToLens) . reverse

split (Terminal a) =
  Fork (Terminal . floor $ (a % 2)) (Terminal . ceiling $ (a % 2))

downLeft :: SnailNum a -> Path -> Path
downLeft (Terminal _) path = path
downLeft (Fork l _) path = downLeft l (L : path)

downRight :: SnailNum a -> Path -> Path
downRight (Terminal _) path = path
downRight (Fork _ r) path = downRight r (R : path)

reduceStep :: SnailNum Int -> Maybe (SnailNum Int)
reduceStep sna =
  let shouldSplit (Terminal a) = a >= 10
      shouldSplit _ = False
      updateRightSibling _ _ _ [] = Nothing
      updateRightSibling root node n (_ : path) =
        let (Just parent@(Fork l r)) = root ^? (pathToLens path)
         in if l == node
              then
                let path' = downLeft r (R : path)
                    updated = root & ((pathToLens path') . value) +~ n
                 in Just updated
              else updateRightSibling root parent n path
      updateLeftSibling _ _ _ [] = Nothing
      -- take two copies of the path to make lens composition/type
      -- inference magic "work"
      updateLeftSibling root node n (_ : path) =
        let (Just parent@(Fork l r)) = root ^? (pathToLens path)
         in if r == node
              then
                let path' = downRight l (L : path)
                    updated = root & ((pathToLens path') . value) +~ n
                 in Just updated
              else updateLeftSibling root parent n path
      explode path a b pair =
        let updatedLeft = updateLeftSibling sna pair a path
            defaultedLeft = fromMaybe sna updatedLeft
            updatedRight = updateRightSibling defaultedLeft pair b path
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

reduceSnailNum :: SnailNum Int -> IO (SnailNum Int)
reduceSnailNum sna =
  case reduceStep sna of
    Just snb -> print snb >> reduceSnailNum snb
    _ -> return sna

appendSnailNum :: SnailNum Int -> SnailNum Int -> IO (SnailNum Int)
appendSnailNum l r = reduceSnailNum (Fork l r)

sumSnailNums :: [SnailNum Int] -> IO (SnailNum Int)
sumSnailNums [] = error "nope"
sumSnailNums (n : ns) = foldl' (\l r -> l >>= (`appendSnailNum` r)) (return n) ns

doMathsHomework :: IO ()
doMathsHomework = do
  (Right parsed) <- parseStdin parseSnailNums
  n <- sumSnailNums parsed
  print n
