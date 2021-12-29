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

hax = fmap (\f -> if ((Fork (Terminal 0) (Terminal 1)) ^? f) == (Just (Terminal 0)) then 'L' else 'R')

goExplodeHax depth path1 fork@(Fork l@(Terminal a) r@(Terminal b))
  | depth >= 4 = Just . hax $ path1
goExplodeHax depth path1 (Fork l r) =
  let lr = goExplodeHax (depth + 1) (left : path1) l
      rr = goExplodeHax (depth + 1) (right : path1) r
   in lr `justOr` rr
goExplodeHax _ _ (Terminal _) = Nothing

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
      updateRightSibling _ _ _ [] [] = Nothing
      updateRightSibling root node n (_ : path1) (_ : path2) =
        let (Just parent@(Fork l r)) = root ^? (pathToLens path1)
         in if l == node
              then
                let path' = downLeft r (right : path2)
                    updated = root & ((pathToLens path') . value) +~ n
                 in Just updated
              else updateRightSibling root parent n path1 path2
      updateLeftSibling _ _ _ [] [] = Nothing
      -- take two copies of the path to make lens composition/type
      -- inference magic "work"
      updateLeftSibling root node n (_ : path1) (_ : path2) =
        let (Just parent@(Fork l r)) = root ^? (pathToLens path1)
         in if r == node
              then
                let path' = downRight l (left : path2)
                    updated = root & ((pathToLens path') . value) +~ n
                 in Just updated
              else updateLeftSibling root parent n path1 path2
      -- findSiblings (_ : path) node =
      --   let (Just (Fork l r)) = sna ^? (pathToLens path)
      --    in if l == node
      --         then ((updateLeftSibling path l), (Just $ downRight r path))
      --         else ((Just $ downLeft l path), (findRightSibling path r))
      updateSibling n number p = number & ((pathToLens p) . value) +~ n
      explode path1 path2 a b pair =
        let updatedLeft = updateLeftSibling sna pair a path1 path2
            defaultedLeft = fromMaybe sna updatedLeft
            updatedRight = updateRightSibling defaultedLeft pair b path1 path2
            defaultedRight = fromMaybe defaultedLeft updatedRight
            replaced = defaultedRight & (pathToLens path2) .~ (Terminal 0)
         in -- hax = fmap (\f -> if (pair ^? f) == (Just (_left pair)) then 'L' else 'R') path1
            -- replaced = error . show $ hax
            Just replaced
      goExplode depth path1 path2 fork@(Fork l@(Terminal a) r@(Terminal b))
        | depth >= 4 =
          let result = explode path1 path2 a b fork
           in result
      -- in error $ hax path1
      goExplode depth path1 path2 (Fork l r) =
        let lr = goExplode (depth + 1) (left : path1) (left : path2) l
            rr = goExplode (depth + 1) (right : path1) (right : path2) r
         in lr `justOr` rr
      goExplode _ _ _ (Terminal _) = Nothing
      goSplit path (Fork l r) =
        (goSplit (left : path) l) `justOr` (goSplit (right : path) r)
      goSplit path (Terminal a) =
        if a >= 10
          then Just (sna & (pathToLens path) %~ split)
          else Nothing
      exploded = goExplode 0 [] [] sna
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
