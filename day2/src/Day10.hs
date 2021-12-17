{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Day10 where

import Data.Either (partitionEithers)
import Data.Foldable
import qualified Data.List as List
import Data.Maybe (maybeToList)
import Data.Monoid (Sum (..), getSum)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector ((!?))
import qualified Data.Vector as V
import Lib (MyParser, parseInt, parseStdin)
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Error

type BrokenParser = Parsec String ()

parseParens :: BrokenParser String
parseParens = between (char '(') (char ')') parseChunks

parseSquare :: BrokenParser String
parseSquare = between (char '[') (char ']') parseChunks

parseCurly :: BrokenParser String
parseCurly = between (char '{') (char '}') (parseChunks)

parseAngle :: BrokenParser String
parseAngle = between (char '<') (char '>') parseChunks

parseChunk :: BrokenParser String
parseChunk = parseParens <|> parseSquare <|> parseCurly <|> parseAngle

parseChunks :: BrokenParser String
parseChunks = ((id =<<) <$> many parseChunk)

parseLine s = runParser parseChunks () "" s

-- lolololol
parseErrorMessage :: BrokenParser Char
parseErrorMessage = char '\"' *> (char '}' <|> char ']' <|> char ')' <|> char '>')

interpretErrors :: [ParseError] -> Int
interpretErrors errors =
  let getErrorChar e =
        let message = messageString . head . errorMessages $ e
         in runParser parseErrorMessage () "" message
      scoreErrorChar c = case c of
        (Right ')') -> 3
        (Right ']') -> 57
        (Right '}') -> 1197
        (Right '>') -> 25137
        _ -> 0
   in sum . fmap (scoreErrorChar . getErrorChar) $ errors

doThing :: [Either ParseError String] -> Int
doThing ls =
  let (failures, _) = partitionEithers ls
   in interpretErrors failures

scoreErrors :: IO ()
scoreErrors = do
  input <- getContents
  let ls = lines input
  let parsedLines = fmap parseLine ls
  print parsedLines
  print $ doThing parsedLines
