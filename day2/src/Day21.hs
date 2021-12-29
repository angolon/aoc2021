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

newtype Board = Board {_getBoard :: [Int]}

makeLenses ''Board

newtype Die = Die {_getDie :: [Int]}

makeLenses ''Die

data Player = Player
  { _name :: Int,
    _position :: Int,
    _score :: Int,
    _continuingBoard :: Board
  }

makeLenses ''Player

data Game = Game
  { _nRolls :: Int,
    _players :: (Player, Player),
    _die :: Die
  }

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

quantumDieRoll :: Die -> [(Int, Int, Int)]
quantumDieRoll (Die d) = (,,) <$> d <*> d <*> d

roll :: GameState [Int]
roll = do
  game <- S.get
  let d = game ^. die
  (roll1, roll2, roll3) <- S.lift $ quantumDieRoll d
  let game' = game & nRolls +~ 3
  S.put game'
  return [roll1, roll2, roll3]

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
