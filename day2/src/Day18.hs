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
  deriving (Eq, Show)

makeLenses ''SnailNum

data Step = L | R deriving (Show, Eq)

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

-- Operates on depth annotated numbers

-- reduceSnailNumber :: SnailNum Int -> SnailNum Int
-- reduceSnailNumber sna =
--   let annotated = annotateDepth sna
--       shouldSplit = (>= 10) . snd
--       shouldExplode = (>= 4) . fst
--       -- findVictim :: SnailNum (Int, Int) -> Maybe (Int, (Int, Int))
--       findVictim = ifind (\_ n -> shouldSplit n || shouldExplode n)
--       explode tree (idx, (depth, n)) =
--         let (Just (_, m)) = tree ^? (traversed . index (idx + 1))
--          in -- blagggggghraoeush
--             -- Can't fucking replace the pair of leaves with a single
--             -- leaf using lenses because forks don't hold a value and
--             -- can't be addressed :sob:
--             tree & (imapped . index (idx - 1) . _2) +~ n
--               & (imapped . index (idx + 2) . _2) +~ m
--    in undefined
traverseWithPath sna =
  let go (Terminal a) depth path results = (depth, path) : results
      go (Fork l r) depth path results =
        let lResults = go l (depth + 1) ((left) : path) results
         in go r (depth + 1) ((right) : path) lResults
   in go sna 0 [] []

justOr (Just a) _ = Just a
justOr _ b = b

type Path f = [(SnailNum Int) -> f (SnailNum Int)]

reduceStep :: SnailNum Int -> Maybe (SnailNum Int)
reduceStep sna =
  let shouldSplit (Terminal a) = a >= 10
      shouldSplit _ = False
      split (Terminal a) =
        Fork (Terminal . floor $ (a % 2)) (Terminal . ceiling $ (a % 2))
      pathToLens path = appEndo . foldMap Endo . reverse $ path
      -- downLeft :: SnailNum a -> Path Identity -> Path Identity
      downLeft (Terminal _) path = path
      downLeft (Fork l _) path = downLeft l (left : path)
      downRight (Terminal _) path = path
      downRight (Fork _ r) path = downRight r (right : path)
      findRightSibling [] _ = Nothing
      findRightSibling (_ : path) node =
        let (Just parent@(Fork l r)) = sna ^? (pathToLens path)
         in if l == node
              then Just $ downRight r path
              else findRightSibling path parent
      -- findLeftSibling :: (Applicative f) => Path f -> SnailNum Int -> Maybe (Path)
      findLeftSibling [] _ = Nothing
      findLeftSibling (_ : path) node =
        let (Just parent@(Fork l r)) = sna ^? (pathToLens path)
         in if r == node
              then Just $ downLeft l path
              else findLeftSibling path parent
      findSiblings (_ : path) node =
        let (Just (Fork l r)) = sna ^? (pathToLens path)
         in if l == node
              then ((findLeftSibling path l), (Just $ downRight r path))
              else ((Just $ downLeft l path), (findRightSibling path r))
      updateSibling n number p = number & ((pathToLens p) . value) +~ n
      explode path a b pair =
        let (leftSibling, rightSibling) = findSiblings path pair
            updatedLeft = fmap (updateSibling a sna) leftSibling
            defaultedLeft = fromMaybe sna updatedLeft
            updatedRight =
              fromMaybe updatedLeft (fmap updateSibling b updatedLeft $ rightSibling)
         in undefined
      go depth path fork@(Fork l@(Terminal a) r@(Terminal b))
        | depth >= 4 = explode path a b fork
      go depth path (Fork l r) =
        let lr = go (depth + 1) (left : path) l
            rr = go (depth + 1) (right : path) r
         in lr `justOr` rr
      go _ path (Terminal a) =
        if a >= 10
          then Just (sna & (pathToLens path) %~ split)
          else Nothing
   in go 0 [] sna

doMathsHomework :: IO ()
doMathsHomework = do
  (Right parsed) <- parseStdin parseSnailNums
  let reducedOnce = fmap reduceStep parsed
  traverse_ print reducedOnce
