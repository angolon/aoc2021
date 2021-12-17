{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Day4 where

import Control.Lens
import Data.Bifunctor
import Data.Bifunctor.Swap
import Data.Either
import Data.Function (on)
import qualified Data.List as List
import Lib (MyParser, parseStdin)
import Text.Parsec
import Text.Parsec.Char

type Cell = Either Int Int

unmarkedCell :: Int -> Cell
unmarkedCell = Left

-- markCell :: Cell -> Cell
-- markCell n c = case c of
--   (Left m) |

markIf :: Int -> Cell -> Cell
markIf n c@(Left m)
  | n == m = Right m
  | otherwise = c
markIf _ c = c

type Row = [Cell]

type Col = Row

data Board = Board {_rows :: [Row]} deriving (Show, Eq)

makeLenses ''Board

columns :: Board -> [Col]
columns = List.transpose . view rows

playNumber :: Int -> Board -> Board
playNumber n =
  rows . mapped . mapped %~ markIf n

-- Left is success, to short-circuit any further computation.
playNumbers :: [Int] -> Board -> Either (Int, Board) Board
playNumbers ns board =
  let tag i eBoard = first (\b -> (i, b)) eBoard
      go [] _ b = Right b
      go (n : ns) i board =
        let played = playNumber n board
            checked = tag i . checkWin $ played
         in checked >>= go ns (i + 1)
   in go ns 0 board

-- The usual sense of Left = failure is inverted here. A Left means that we
-- won the game, meaning that we can stop processing moves.
checkWin :: Board -> Either Board Board
checkWin board =
  let checkCells :: Row -> Either [Int] Int
      checkCells = swap . sequence
      checkCR :: [Row] -> Either [Int] [Int]
      checkCR = traverse checkCells
      checkRows = checkCR . view rows
      checkColumns = checkCR . columns
      checkGame = checkRows board *> checkColumns board
      toBoard _ = board
   in bimap toBoard toBoard checkGame

data Game = Game {_numbers :: [Int], _boards :: [Board]} deriving (Show, Eq)

makeLenses ''Game

-- Find the board that wins in the fewest called numbers
playGame :: Game -> Either String (Int, Board)
playGame (Game ns boards) = do
  winners <-
    first (\_ -> "Didn't find any winners")
      . sequence
      . filter isRight
      . fmap (swap . playNumbers ns) -- swap winners back to the right so we can sequence with them
      $ boards
  let bestWinner = List.minimumBy (compare `on` fst) winners
  return bestWinner

scoreWinner :: Game -> Int -> Board -> Int
scoreWinner (Game ns _) nMoves (Board rows) =
  let unmarkedSum = sumOf (folded . folded . _Left) rows
   in unmarkedSum * (ns !! nMoves)

playToWin :: Game -> Either String Int
playToWin game = do
  winner <- playGame game
  return (uncurry (scoreWinner game) $ winner)

parseInt :: MyParser Int
parseInt = fmap (read @Int) (many1 digit)

parseNumbers :: MyParser [Int]
parseNumbers = sepBy1 parseInt (char ',')

parseCell :: MyParser Cell
parseCell = spaces *> (unmarkedCell <$> parseInt) <* spaces

parseRow :: MyParser Row
parseRow = count 5 parseCell

parseBoard :: MyParser Board
parseBoard = Board <$> count 5 parseRow

parseBoards :: MyParser [Board]
parseBoards = many1 parseBoard

parseGame :: MyParser Game
parseGame = Game <$> parseNumbers <*> parseBoards

playBingo :: IO ()
playBingo = do
  game <- parseStdin parseGame
  print (fmap playToWin game)
