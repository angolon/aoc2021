{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Day23 where

import Control.Arrow (arr, (&&&))
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Loops as Loops
import qualified Control.Monad.State.Lazy as S
import Data.Either (either)
import Data.Foldable
import Data.Function (on)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map, (!), (!?))
import qualified Data.Map as Map
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MMap
import Data.Maybe
import Data.Monoid (Endo (..), Product (..), Sum (..), getSum)
import Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.PQueue.Prio.Min as MinPQueue
import Data.Ratio
import Data.Set (Set, member, union, (\\))
import qualified Data.Set as Set
import qualified Data.Vector as V
import Lib (MyParser, parseInt, parseStdin)
import Text.Parsec
import Text.Parsec.Char
import Text.Show.Functions

data Amphipod = A | B | C | D deriving (Show, Eq, Ord)

data Cell = Cell {_amphipod :: Maybe Amphipod}

makeLenses ''Cell

type Burrow = Map (Int, Int) (Maybe Amphipod)

emptyBurrow :: Burrow
emptyBurrow =
  Map.fromList . fmap (,Nothing) $
    [ (0, 0),
      (1, 0),
      (2, 0),
      (2, 1),
      (2, 2),
      (3, 0),
      (4, 0),
      (4, 1),
      (4, 2),
      (5, 0),
      (6, 0),
      (6, 1),
      (6, 2),
      (7, 0),
      (8, 0),
      (8, 1),
      (8, 2),
      (9, 0),
      (10, 0)
    ]

targetCells :: Amphipod -> [(Int, Int)]
targetCells A = [(2, 1), (2, 2)]
targetCells B = [(4, 1), (4, 2)]
targetCells C = [(6, 1), (6, 2)]
targetCells D = [(8, 1), (8, 2)]

moveCost :: Amphipod -> Int
moveCost A = 1
moveCost B = 10
moveCost C = 100
moveCost D = 1000

adjacentCoords :: (Int, Int) -> [(Int, Int)]
adjacentCoords (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

atTarget :: Burrow -> (Int, Int) -> Bool
atTarget b (2, 1) =
  allOf (itraversed . indices (`elem` targetCells A)) (== (Just A)) b
atTarget b (2, 2) = b ! (2, 2) == Just A
atTarget b (4, 1) =
  allOf (itraversed . indices (`elem` targetCells B)) (== (Just B)) b
atTarget b (4, 2) = b ! (4, 2) == Just B
atTarget b (6, 1) =
  allOf (itraversed . indices (`elem` targetCells C)) (== (Just C)) b
atTarget b (6, 2) = b ! (6, 2) == Just C
atTarget b (8, 1) =
  allOf (itraversed . indices (`elem` targetCells D)) (== (Just D)) b
atTarget b (8, 2) = b ! (8, 2) == Just D
atTarget _ _ = False

canStop :: Burrow -> (Int, Int) -> Bool
canStop _ (2, 0) = False
canStop b (2, 1) =
  allOf (itraversed . indices (`elem` targetCells A)) (== (Just A)) b
canStop b (2, 2) = b ! (2, 2) == Just A
canStop _ (4, 0) = False
canStop b (4, 1) =
  allOf (itraversed . indices (`elem` targetCells B)) (== (Just B)) b
canStop b (4, 2) = b ! (4, 2) == Just B
canStop _ (6, 0) = False
canStop b (6, 1) =
  allOf (itraversed . indices (`elem` targetCells C)) (== (Just C)) b
canStop b (6, 2) = b ! (6, 2) == Just C
canStop _ (8, 0) = False
canStop b (8, 1) =
  allOf (itraversed . indices (`elem` targetCells D)) (== (Just D)) b
canStop b (8, 2) = b ! (8, 2) == Just D
canStop _ _ = True

occupied :: Burrow -> [(Int, Int)]
occupied b = b ^.. (itraversed . _Just . withIndex . _1)

unoccupied :: Burrow -> [(Int, Int)]
unoccupied b = b ^.. (itraversed . _Nothing . withIndex . _1)

place :: Amphipod -> Burrow -> (Int, Int) -> Burrow
place pod b coord = b & (imapped . indices (== coord)) .~ Just pod

remove :: (Int, Int) -> Burrow -> Burrow
remove coord b = b & (imapped . indices (== coord)) .~ Nothing

nextSteps :: Burrow -> (Int, Int) -> [(Int, Burrow)]
nextSteps b cs =
  let go burrow pod cost visited coords@(x, y)
        | y == 2 && (coords `elem` (targetCells pod)) = []
        | otherwise =
          let adjacent =
                filter (not . (`member` visited))
                  . filter (`elem` unoccupied burrow)
                  $ adjacentCoords coords
              nextCost = cost + (moveCost pod)
              preStep = remove coords burrow
           in -- in (maybeToList . maybePlace pod preStep) =<< adjacent
              do
                adj <- adjacent
                let nextBurrow = place pod preStep adj
                let nextVisited = Set.insert adj visited
                let nextG = go nextBurrow pod nextCost nextVisited adj
                if canStop nextBurrow adj
                  then (nextCost, nextBurrow) : nextG
                  else nextG
      (Just init) = b ! cs
   in go b init 0 (Set.singleton cs) cs

data SolutionSpace = SolutionSpace
  { _queue :: MinPQueue Int Burrow,
    _observedStates :: Set Burrow
  }
  deriving (Eq)

makeLenses ''SolutionSpace

movers :: Burrow -> [(Int, Int)]
movers b = filter (not . atTarget b) . occupied $ b

solved :: Burrow -> Bool
solved = null . movers

solveStep :: SolutionSpace -> S.State SolutionSpace ()
solveStep solutionSpace =
  let ((cost, burrow), nextQueue) = MinPQueue.deleteFindMin . _queue $ solutionSpace
      newState = not . (`Set.member` (_observedStates solutionSpace))
      nextBurrowCosts = do
        mover <- movers burrow
        (movingCost, moved) <- nextSteps burrow mover
        guard $ newState moved
        return (movingCost + cost, moved)
      nextBurrows = Set.fromList $ fmap snd nextBurrowCosts
   in S.put $
        solutionSpace
          & queue .~ MinPQueue.union nextQueue (MinPQueue.fromList nextBurrowCosts)
          & observedStates %~ Set.union nextBurrows

solveBurrow :: Burrow -> (Int, Burrow)
solveBurrow burrow =
  let cheapest = MinPQueue.findMin . _queue <$> S.get
      isSolved = solved . snd <$> cheapest
      step = S.get >>= solveStep
      solvedST = step `Loops.untilM_` isSolved
      finalised = solvedST >> cheapest
      initialQueue = MinPQueue.singleton 0 burrow
      initialSet = Set.singleton burrow
      initialSpace = SolutionSpace initialQueue initialSet
   in S.evalState finalised initialSpace

-- solutions :: Burrow -> [(Int, Burrow)]
-- solutions b =
--   let go burrow cost seenStates =
--         -- TODO: filter out seen states
--         let movers = filter (not . atTarget burrow) $ occupied burrow
--          in if null movers
--               then [(cost, burrow)]
--               else do
--                 mover <- movers
--                 (movingCost, moved) <- nextSteps burrow mover
--                 go moved (cost + movingCost)
--    in go b 0

-- canStop b coors =
--   let occupant = b !? coords

displayBurrow :: Burrow -> String
displayBurrow b =
  let displayRow r = do
        c <- [(-1) .. 11]
        let cell = b !? (c, r)
        return $ case cell of
          Nothing -> '#'
          Just Nothing -> '.'
          Just (Just A) -> 'A'
          Just (Just B) -> 'B'
          Just (Just C) -> 'C'
          Just (Just D) -> 'D'
      rows = fmap displayRow [(-1) .. 3]
   in join . List.intersperse "\n" $ rows

-- #############
-- #...........#
-- ###B#C#B#D###
--   #A#D#C#A#
--   #########
example :: Burrow
example =
  Map.insert (2, 1) (Just B)
    . Map.insert (2, 2) (Just A)
    . Map.insert (4, 1) (Just C)
    . Map.insert (4, 2) (Just D)
    . Map.insert (6, 1) (Just B)
    . Map.insert (6, 2) (Just C)
    . Map.insert (8, 1) (Just D)
    . Map.insert (8, 2) (Just A)
    $ emptyBurrow

organiseAmphipods :: IO ()
organiseAmphipods =
  let (cost, outcome) = solveBurrow example
   in do
        print cost
        putStrLn . displayBurrow $ example
        putStrLn . displayBurrow $ outcome
