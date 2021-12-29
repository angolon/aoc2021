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
  { _name :: String,
    _position :: Int,
    _score :: Int,
    _continuingBoard :: Board
  }

makeLenses ''Player

data Game = Game
  { _nRolls :: Int,
    _players :: (Player, Player),
    _d100 :: Die
  }

makeLenses ''Game

board :: Board
board = Board $ List.cycle [1 .. 10]

player :: String -> Int -> Player
player name startingPosition =
  Player name startingPosition 0 (board & getBoard %~ drop startingPosition)

initialiseGame :: Player -> Player -> Game
initialiseGame player1 player2 =
  Game 0 (player1, player2) (Die $ List.cycle [1 .. 100])

move :: Int -> Player -> Player
move n player =
  let withNextBoard = player & (continuingBoard . getBoard) %~ drop n
      (Just nextPosition) = firstOf (continuingBoard . getBoard . folded) withNextBoard
   in withNextBoard & position .~ nextPosition & score +~ nextPosition

type GameState = S.StateT Game IO

roll :: GameState [Int]
roll = do
  game <- S.get
  let d = game ^. (d100 . getDie)
  let (rolls, d') = List.splitAt 3 d
  let game' = game & (d100 . getDie) .~ d' & nRolls +~ 3
  S.put game'
  return rolls

turn :: GameState ()
turn = do
  rolls <- roll
  game <- S.get
  let (player1, player2) = game ^. players
  let rollTotal = sum rolls
  let nextPlayer1 = move rollTotal player1
  -- Debugging stuff/just for fun.
  let rollShow = List.intercalate "+" $ fmap show rolls
  let moveInfo =
        "Player " ++ (nextPlayer1 ^. name) ++ " rolls " ++ rollShow
          ++ " and moves to space "
          ++ (show $ nextPlayer1 ^. position)
          ++ " for a total score of "
          ++ (show $ nextPlayer1 ^. score)
          ++ "."
  liftIO $ putStrLn moveInfo
  let nextGame = game & players .~ (player2, nextPlayer1)
  S.put nextGame

shouldStop :: GameState Bool
shouldStop = do
  game <- S.get
  return $ anyOf (players . both . score) (>= 1000) game

playGame :: GameState ()
playGame = Loops.untilM_ turn shouldStop

finalMunge :: Game -> Int
finalMunge game =
  let (Just losingScore) = minimumOf (players . both . score) game
   in losingScore * (game ^. nRolls)

playWithDice :: IO ()
playWithDice =
  let player1Pos = 6 - 1
      player2Pos = 7 - 1
      player1 = player "1" player1Pos
      player2 = player "2" player2Pos
      initial = initialiseGame player1 player2
   in do
        finished <- S.execStateT playGame initial
        print . finalMunge $ finished
