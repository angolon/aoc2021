{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Day4 where

import Control.Lens
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

type Column = Row

data Board = Board {_rows :: [Row]} deriving (Show, Eq)

makeLenses ''Board

playNumber :: Int -> Board -> Board
playNumber n =
  rows . mapped . mapped %~ markIf n

data Game = Game {_numbers :: [Int], _boards :: [Board]} deriving (Show, Eq)

makeLenses ''Game

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
parseBoards = sepBy1 parseBoard endOfLine

parseGame :: MyParser Game
parseGame = Game <$> parseNumbers <*> parseBoards

playBingo :: IO ()
playBingo = do
  parsed <- parseStdin parseGame
  let tested = parsed & mapped . boards . mapped %~ playNumber 22
  print tested
