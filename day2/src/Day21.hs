{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Day21 where

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
import Data.Map (Map, (!))
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
import qualified Data.Vector as V
import Day3 (toInt)
import Lib (MyParser, parseStdin)
import Linear.Metric (dot)
import qualified Linear.Quaternion as Q
import Text.Parsec
import Text.Parsec.Char
import Text.Show.Functions

newtype Score = Score {_getScore :: Int} deriving (Show, Eq, Ord, Num)

newtype Position = Position {_getPos :: Int} deriving (Show, Eq, Ord, Num, Enum)

makeLenses ''Score
makeLenses ''Position

toScore :: Position -> Score
toScore p = Score $ p ^. getPos

data Player = Player
  { _position :: Position,
    _score :: Score,
    _nRolls :: Int
  }
  deriving (Eq, Ord, Show)

makeLenses ''Player

newtype Board = Board {_getBoard :: [Position]}

makeLenses ''Board

newtype Die = Die {_getDie :: [Int]} deriving (Eq, Show)

makeLenses ''Die

board :: Board
board = Board $ List.cycle [(Position 1) .. (Position 10)]

finiteBoard :: Board
finiteBoard = board & getBoard %~ take 10

d3 = Die [1 .. 3]

-- tuple of probability and sum of 3 rolls
quantumDieRoll :: Die -> [(Rational, Int)]
quantumDieRoll (Die d) =
  let rollTotals = (\a b c -> a + b + c) <$> d <*> d <*> d
      n = toInteger $ length rollTotals
      groupedRolls = NonEmpty.groupAllWith id rollTotals
      groupProbability = (% n) . toInteger . NonEmpty.length
      groupRoll = NonEmpty.head
   in fmap (groupProbability &&& groupRoll) groupedRolls

nextStepProbabilities :: Map Position [(Rational, Position)]
nextStepProbabilities =
  let rolls = quantumDieRoll d3
   in Map.fromList $ do
        position <- _getBoard finiteBoard
        let continuingBoard = drop ((_getPos position) - 1) . _getBoard $ board
        let nextPositions = rolls & (mapped . _2) %~ head . (`drop` continuingBoard)
        return (position, nextPositions)

type Multiverse = MonoidalMap Player (Sum Rational)

winningScore = Score 21

hasWon player = (player ^. score) >= winningScore

fracture :: Player -> Sum Rational -> Multiverse
fracture player@(Player position score nRolls) (Sum probability)
  | score >= winningScore = MMap.singleton player (Sum probability)
  | otherwise -- Keep fracturing
    =
    let steps = nextStepProbabilities ! position
        nextPlayer (p, nextPosition) =
          let nextScore = score + (toScore nextPosition)
              nextProbability = p * probability
              nextPlayer = Player nextPosition nextScore (nRolls + 3) -- we always roll the die 3 times per turn.
           in MMap.singleton nextPlayer (Sum nextProbability)
     in foldMap nextPlayer steps

fractureMultiverse :: Multiverse -> Multiverse
fractureMultiverse =
  let stoppingCondition :: Multiverse -> Bool
      stoppingCondition = allOf (ifolded . withIndex . _1) hasWon
   in until stoppingCondition $ MMap.foldMapWithKey fracture

initialUniverse :: Position -> Multiverse
initialUniverse position = MMap.singleton (Player position (Score 0) 0) (Sum 1)

-- fightMultiverses :: Multiverse -> Multiverse ->
-- fightMultiverses m1 m2 = do
--   (player1, p) <- MMap.toList m1
--   (player2, q) <- MMap.toList m2
--   let player1Wins = (player1 ^. nRolls) <= (player2 ^. nRolls)

playWithDice :: IO ()
playWithDice =
  let player1Pos = 4 - 1
      player2Pos = 8 - 1
   in putStrLn "lolololol"
