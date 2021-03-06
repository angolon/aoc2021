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
import Data.Monoid (Endo (..), Product (..), Sum (..), getSum)
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
  { _name :: Int,
    _position :: Position,
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

type Multiverse = MonoidalMap (Player, Player) (Sum Rational)

winningScore = Score 21

hasWon player = (player ^. score) >= winningScore

fracture :: (Player, Player) -> Sum Rational -> Multiverse
fracture players (Sum probability)
  | anyOf both hasWon players = MMap.singleton players (Sum probability)
  | otherwise -- Keep fracturing
    =
    let (playerA, playerB) = players
        steps = nextStepProbabilities ! (playerA ^. position)
        nextPlayers (p, nextPosition) =
          let nextScore = (playerA ^. score) + (toScore nextPosition)
              nextProbability = p * probability
              nextPlayerA =
                playerA & position .~ nextPosition
                  & score .~ nextScore
                  & nRolls +~ 3 -- we always roll the die 3 times per turn.
           in MMap.singleton (playerB, nextPlayerA) (Sum nextProbability)
     in foldMap nextPlayers steps

fractureMultiverse :: Multiverse -> Multiverse
fractureMultiverse =
  let eitherHasWon = anyOf both hasWon
      stoppingCondition :: Multiverse -> Bool
      stoppingCondition = allOf (ifolded . withIndex . _1) eitherHasWon
   in until stoppingCondition $ MMap.foldMapWithKey fracture

initialUniverse :: Player -> Player -> Multiverse
initialUniverse player1 player2 = MMap.singleton (player1, player2) (Sum 1)

-- Number of universes generated by the number of rolls from both players
countUniverses :: Player -> Player -> Integer
countUniverses playerA playerB =
  let n = (playerA ^. nRolls) + (playerB ^. nRolls)
   in getProduct . foldMap (const $ Product 3) $ [1 .. n]

playerWinCounts :: Multiverse -> MonoidalMap Int (Sum Rational)
playerWinCounts =
  let winnerUniverses players@(playerA, playerB) (Sum probability) =
        let universeCount = countUniverses playerA playerB
            winCount = Sum $ probability * (toRational universeCount)
            winner =
              if hasWon playerA
                then playerA ^. name
                else playerB ^. name
         in MMap.singleton winner winCount
   in MMap.foldMapWithKey winnerUniverses

-- fightMultiverses :: Multiverse -> Multiverse ->
-- fightMultiverses m1 m2 = do
--   (player1, p) <- MMap.toList m1
--   (player2, q) <- MMap.toList m2
--   let player1Wins = (player1 ^. nRolls) <= (player2 ^. nRolls)

playWithDice :: IO ()
playWithDice =
  let player1 = Player 1 (Position 6) (Score 0) 0
      player2 = Player 2 (Position 7) (Score 0) 0
      start = initialUniverse player1 player2
      end = fractureMultiverse start
      result = playerWinCounts end
   in traverse_ print . MMap.toList $ result
