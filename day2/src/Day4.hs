{-# LANGUAGE TypeApplications #-}

module Day4 where

import Lib (MyParser, parseStdin)
import Text.Parsec
import Text.Parsec.Char

type Cell = Either Int Int

unmarkedCell :: Int -> Cell
unmarkedCell = Left

markCell :: Cell -> Cell
markCell (Left a) = Right a

type Row = [Cell]

type Column = Row

data Board = Board {_rows :: [Row]} deriving (Show, Eq)

data Game = Game {_numbers :: [Int], boards :: [Board]} deriving (Show, Eq)

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
  print parsed
