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

newtype Board = Board {_getBoard :: [Int]}

-- Equality for boards which assumes that if the head is equal, then the whole thing is equal.
-- Won't work for boards of potentially different length.
instance Eq Board where
  (==) (Board (a : _)) (Board (b : _)) = a == b

-- Assumes that the length is 10 :-P
instance Show Board where
  show (Board b) = show $ take 10 b

makeLenses ''Board

newtype Die = Die {_getDie :: [Int]} deriving (Eq, Show)

makeLenses ''Die

data Player = Player
  { _name :: Int,
    _position :: Int,
    _score :: Int,
    _continuingBoard :: Board
  }
  deriving (Eq, Show)

makeLenses ''Player

data Game = Game
  { _nRolls :: Int,
    _players :: (Player, Player),
    _die :: Die
  }
  deriving (Eq, Show)

makeLenses ''Game

board :: Board
board = Board $ List.cycle [1 .. 10]

player :: Int -> Int -> Player
player name startingPosition =
  Player name startingPosition 0 (board & getBoard %~ drop startingPosition)

initialiseGame :: Player -> Player -> Game
initialiseGame player1 player2 =
  Game 0 (player1, player2) (Die [1 .. 3])

move :: Int -> Player -> Player
move n player =
  let withNextBoard = player & (continuingBoard . getBoard) %~ drop n
      (Just nextPosition) = firstOf (continuingBoard . getBoard . folded) withNextBoard
   in withNextBoard & position .~ nextPosition & score +~ nextPosition

type GameState = S.StateT Game []

-- tuple of probability and sum of 3 rolls
quantumDieRoll :: Die -> [(Rational, Int)]
quantumDieRoll (Die d) =
  let rollTotals = (\a b c -> a + b + c) <$> d <*> d <*> d
      n = toInteger $ length rollTotals
      groupedRolls = NonEmpty.groupAllWith id rollTotals
      groupProbability = (% n) . toInteger . NonEmpty.length
      groupRoll = NonEmpty.head
   in fmap (groupProbability &&& groupRoll) groupedRolls

nextStepProbabilities :: Die -> Map Int [(Rational, Int)]
nextStepProbabilities d =
  let rolls = quantumDieRoll d
   in Map.fromList $ do
        i <- [1 .. 10]
        let continuingBoard = drop (i - 1) . _getBoard $ board
        let nextPositions = rolls & (mapped . _2) %~ head . (`drop` continuingBoard)
        return (i, nextPositions)

multiverse :: Int -> [(Rational, Int)]
multiverse startPos =
  let d = Die [1 .. 3]
      nextStepProbs = nextStepProbabilities d
      step :: (Rational, Int, Int) -> [(Rational, Int, Int)]
      step (p, i, score) =
        let steps = nextStepProbs ! i
         in fmap (\(q, j) -> (p * q, j, score + j)) steps
      -- I think there's something magical about the number 6,
      -- due to vague maths.
      blargh =
        step (1, startPos, 0) >>= step >>= step >>= step >>= step >>= step
          >>= step
          >>= step
   in fmap (\(a, b, c) -> (a, c)) blargh

moveStream :: Int -> Int -> [Player]
moveStream name startPos = undefined

roll :: GameState [Int]
roll = do
  game <- S.get
  let d = game ^. die
  (probability, rollTotal) <- S.lift $ quantumDieRoll d
  let game' = game & nRolls +~ 3
  S.put game'
  return [rollTotal]

turn :: GameState ()
turn = do
  rolls <- roll
  game <- S.get
  let (player1, player2) = game ^. players
  let rollTotal = sum rolls
  let nextPlayer1 = move rollTotal player1
  let nextGame = game & players .~ (player2, nextPlayer1)
  S.put nextGame

shouldStop :: GameState Bool
shouldStop = do
  game <- S.get
  return $ anyOf (players . both . score) (>= 21) game

playGame :: GameState ()
playGame = Loops.untilM_ turn shouldStop

winCounts :: GameState (MonoidalMap Int (Sum Integer))
winCounts = do
  game <- S.get
  let (Just winner) = maximumByOf (players . both) (compare `on` (^. score)) game
  let n = winner ^. name
  return $ MMap.singleton n (Sum 1)

playQuantumGames :: Game -> MonoidalMap Int (Sum Integer)
playQuantumGames initial =
  let horrific = playGame >> winCounts
      multiverse = S.evalStateT horrific initial
   in fold multiverse

finalMunge :: Game -> Int
finalMunge game =
  let (Just losingScore) = minimumOf (players . both . score) game
   in losingScore * (game ^. nRolls)

playWithDice :: IO ()
playWithDice =
  let player1Pos = 4 - 1
      player2Pos = 8 - 1
      player1 = player 1 player1Pos
      player2 = player 2 player2Pos
      initial = initialiseGame player1 player2
      finished = playQuantumGames initial
   in print finished
