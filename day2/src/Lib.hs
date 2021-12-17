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
import Data.Monoid (Product (..))
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
  { _aim :: Int,
    _horizontal :: Int,
    _depth :: Int
  }
  deriving (Show, Eq)

makeLenses ''Position

move :: Position -> Move -> Position
move pos (Forward a) =
  pos & horizontal +~ a & depth +~ (a * (view aim pos))
move pos (Up a) = pos & aim -~ a
move pos (Down a) = pos & aim +~ a

runMoves :: [Move] -> Position
runMoves moves = foldl' move (Position 0 0 0) moves

toProduct = to Product

finalise :: Position -> Int
finalise pos =
  let (Product result) = pos ^. ((horizontal . toProduct) <> (depth . toProduct))
   in result

parseInt :: MyParser Int
parseInt = fmap (read @Int) (many1 digit)

parseStdin :: MyParser a -> IO (Either ParseError a)
parseStdin p = getContents >>= runParserT p () ""

runMovesStdin :: IO ()
runMovesStdin = do
  parsed <- parseStdin parseMoves
  let result = (finalise . runMoves) <$> parsed
  print result
