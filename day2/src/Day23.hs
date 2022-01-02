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
import Data.Bifunctor.Swap (swap)
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
      (2, 3),
      (2, 4),
      (3, 0),
      (4, 0),
      (4, 1),
      (4, 2),
      (4, 3),
      (4, 4),
      (5, 0),
      (6, 0),
      (6, 1),
      (6, 2),
      (6, 3),
      (6, 4),
      (7, 0),
      (8, 0),
      (8, 1),
      (8, 2),
      (8, 3),
      (8, 4),
      (9, 0),
      (10, 0)
    ]

targetCells :: Amphipod -> [(Int, Int)]
targetCells A = (2,) <$> [1 .. 4]
targetCells B = (4,) <$> [1 .. 4]
targetCells C = (6,) <$> [1 .. 4]
targetCells D = (8,) <$> [1 .. 4]

moveCost :: Amphipod -> Int
moveCost A = 1
moveCost B = 10
moveCost C = 100
moveCost D = 1000

adjacentCoords :: (Int, Int) -> [(Int, Int)]
adjacentCoords (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

targetCellsFullFromBottomTo :: Amphipod -> Int -> Burrow -> Bool
targetCellsFullFromBottomTo _ 0 = const False
targetCellsFullFromBottomTo pod y =
  let is = filter ((>= y) . snd) $ targetCells pod
   in allOf (itraversed . indices (`elem` is)) (== (Just pod))

atTarget :: (Int, Int) -> Burrow -> Bool
atTarget (2, y) = targetCellsFullFromBottomTo A y
atTarget (4, y) = targetCellsFullFromBottomTo B y
atTarget (6, y) = targetCellsFullFromBottomTo C y
atTarget (8, y) = targetCellsFullFromBottomTo D y
atTarget _ = const False

canStop :: (Int, Int) -> Burrow -> Bool
canStop (2, y) = targetCellsFullFromBottomTo A y
canStop (4, y) = targetCellsFullFromBottomTo B y
canStop (6, y) = targetCellsFullFromBottomTo C y
canStop (8, y) = targetCellsFullFromBottomTo D y
canStop _ = const True

occupied :: Burrow -> [(Int, Int)]
occupied b = b ^.. (itraversed . _Just . withIndex . _1)

unoccupied :: Burrow -> [(Int, Int)]
unoccupied b = b ^.. (itraversed . _Nothing . withIndex . _1)

place :: Amphipod -> Burrow -> (Int, Int) -> Burrow
place pod b coord = b & (imapped . indices (== coord)) .~ Just pod

remove :: (Int, Int) -> Burrow -> Burrow
remove coord b = b & (imapped . indices (== coord)) .~ Nothing

nextSteps :: Burrow -> (Int, Int) -> [(Int, Burrow)]
nextSteps b cs@(_, initialY) =
  let go burrow pod cost visited coords
        | atTarget coords burrow = []
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
                -- Amphipods *must* move from the hallway directly
                -- to their target room. They can't shuffle left and
                -- right in the hall.
                -- Thus, if the initial Y coordinate was zero, the only
                -- valid next step is one that ends at the target.
                if
                    | initialY /= 0 && canStop adj nextBurrow -> (nextCost, nextBurrow) : nextG
                    | atTarget adj nextBurrow -> [(nextCost, nextBurrow)]
                    | otherwise -> nextG
      (Just init) = b ! cs
   in go b init 0 (Set.singleton cs) cs

data SolutionSpace = SolutionSpace
  { _queue :: MinPQueue Int [Burrow],
    _observedStates :: Map Burrow Int
  }
  deriving (Eq)

makeLenses ''SolutionSpace

movers :: Burrow -> [(Int, Int)]
movers b = filter (not . (`atTarget` b)) . occupied $ b

solved :: Burrow -> Bool
solved = null . movers

solveStep :: SolutionSpace -> S.State SolutionSpace ()
solveStep solutionSpace =
  let ((cost, burrows), nextQueue) = MinPQueue.deleteFindMin . _queue $ solutionSpace
      burrow = head burrows
      cheaperState otherBurrow otherCost =
        let elem = (_observedStates solutionSpace) !? otherBurrow
         in all (> otherCost) elem
      nextBurrowCosts = do
        mover <- movers burrow
        (movingCost, moved) <- nextSteps burrow mover
        let cost' = cost + movingCost
        guard $ cheaperState moved cost'
        return (cost', moved : burrows)
      nextMinCostStates =
        Map.fromList $
          fmap
            ( \(c, bs) ->
                (head bs, c)
            )
            nextBurrowCosts
   in S.put $
        solutionSpace
          & queue .~ MinPQueue.union nextQueue (MinPQueue.fromList nextBurrowCosts)
          -- Put the newest min cost states on the left of
          -- mappend, so that those values override previous
          -- more costly potentials
          & observedStates %~ (nextMinCostStates <>)

solveBurrow :: Burrow -> (Int, [Burrow])
solveBurrow burrow =
  let cheapest = MinPQueue.findMin . _queue <$> S.get
      isSolved = solved . head . snd <$> cheapest
      -- isSolved = (> 20000) . fst <$> cheapest
      step = S.get >>= solveStep
      solvedST = step `Loops.untilM_` isSolved
      finalised = solvedST >> cheapest
      initialQueue = MinPQueue.singleton 0 [burrow]
      initialCosts = Map.singleton burrow 0
      initialSpace = SolutionSpace initialQueue initialCosts
   in S.evalState finalised initialSpace

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
      rows = fmap displayRow [(-1) .. 5]
   in join . List.intersperse "\n" $ rows

-- #############
-- #...........#
-- ###B#C#B#D###
--   #D#C#B#A#
--   #D#B#A#C#
--   #A#D#C#A#
--   #########
example :: Burrow
example =
  Map.insert (2, 1) (Just B)
    . Map.insert (2, 2) (Just D)
    . Map.insert (2, 3) (Just D)
    . Map.insert (2, 4) (Just A)
    . Map.insert (4, 1) (Just C)
    . Map.insert (4, 2) (Just C)
    . Map.insert (4, 3) (Just B)
    . Map.insert (4, 4) (Just D)
    . Map.insert (6, 1) (Just B)
    . Map.insert (6, 2) (Just B)
    . Map.insert (6, 3) (Just A)
    . Map.insert (6, 4) (Just C)
    . Map.insert (8, 1) (Just D)
    . Map.insert (8, 2) (Just A)
    . Map.insert (8, 3) (Just C)
    . Map.insert (8, 4) (Just A)
    $ emptyBurrow

-- #############
-- #...........#
-- ###A#C#B#D###
--   #D#C#B#A#
--   #D#B#A#C#
--   #B#A#D#C#
--   #########
puzzle :: Burrow
puzzle =
  Map.insert (2, 1) (Just A)
    . Map.insert (2, 2) (Just D)
    . Map.insert (2, 3) (Just D)
    . Map.insert (2, 4) (Just B)
    . Map.insert (4, 1) (Just C)
    . Map.insert (4, 2) (Just C)
    . Map.insert (4, 3) (Just B)
    . Map.insert (4, 4) (Just A)
    . Map.insert (6, 1) (Just B)
    . Map.insert (6, 2) (Just B)
    . Map.insert (6, 3) (Just A)
    . Map.insert (6, 4) (Just D)
    . Map.insert (8, 1) (Just D)
    . Map.insert (8, 2) (Just A)
    . Map.insert (8, 3) (Just C)
    . Map.insert (8, 4) (Just C)
    $ emptyBurrow

-- #############
-- #AA.....B.BD#
-- ###B#C#.#.###
-- ###D#C#.#.###
-- ###D#B#.#C###
-- ###A#D#C#A###
-- #############
borked :: Burrow
borked =
  Map.insert (2, 1) (Just B)
    . Map.insert (2, 2) (Just D)
    . Map.insert (2, 3) (Just D)
    . Map.insert (2, 4) (Just A)
    . Map.insert (4, 1) (Just C)
    . Map.insert (4, 2) (Just C)
    . Map.insert (4, 3) (Just B)
    . Map.insert (4, 4) (Just D)
    . Map.insert (6, 4) (Just C)
    . Map.insert (8, 3) (Just C)
    . Map.insert (8, 4) (Just A)
    . Map.insert (0, 0) (Just A)
    . Map.insert (1, 0) (Just A)
    . Map.insert (7, 0) (Just B)
    . Map.insert (9, 0) (Just B)
    . Map.insert (10, 0) (Just D)
    $ emptyBurrow

-- organiseAmphipods :: IO ()
-- organiseAmphipods =
--   -- let steps = pretendSolveBurrow example
--   let steps = nextSteps borked (4, 1)
--       display (cost, burrow) = do
--         print cost
--         putStrLn . displayBurrow $ burrow
--    in do
--         putStrLn . displayBurrow $ borked
--         traverse_ display steps

organiseAmphipods :: IO ()
organiseAmphipods =
  let (cost, outcome) = solveBurrow puzzle
   in do
        print cost
        putStrLn . displayBurrow $ puzzle
        -- traverse_ (putStrLn . displayBurrow) outcome
        putStrLn . displayBurrow . head $ outcome
