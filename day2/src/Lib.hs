{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib where

import Control.Lens
import Control.Monad.Free
import Data.Foldable (foldl')
import Text.Parsec
import Text.Parsec.Char

data Move = Forward Int | Down Int | Up Int deriving (Show, Eq)

type MyParser = ParsecT String () IO

parseMoveConstructor :: MyParser (Int -> Move)
parseMoveConstructor =
  fmap (\_ -> Forward) (string "forward")
    <|> fmap (\_ -> Up) (string "up")
    <|> fmap (\_ -> Down) (string "down")

parseNumber :: MyParser Int
parseNumber = fmap (read @Int) $ (many1 digit)

parseMove :: MyParser Move
parseMove =
  parseMoveConstructor
    <* skipMany space
    <*> parseNumber
    <* endOfLine

parseMoves :: MyParser [Move]
parseMoves = many parseMove <* eof

data Position = Position
  { _horizontal :: Int,
    _depth :: Int
  }
  deriving (Show, Eq)

makeLenses ''Position

move :: Position -> Move -> Position
move pos (Forward a) = pos & horizontal +~ a
move pos (Up a) = pos & depth -~ a
move pos (Down a) = pos & depth +~ a

runMoves :: [Move] -> Position
runMoves moves = foldl' move (Position 0 0) moves

finalise :: Position -> Int
finalise (Position x y) = x * y

runMovesStdin :: IO ()
runMovesStdin = do
  input <- getContents
  parsed <- runParserT parseMoves () "" input
  let result = (finalise . runMoves) <$> parsed
  print result
