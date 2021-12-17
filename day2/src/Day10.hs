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

parseCloser :: BrokenParser Char
parseCloser = char ')' <|> char ']' <|> char '}' <|> char '>'

parseIncompleteMessage :: BrokenParser Char
parseIncompleteMessage =
  string "(line "
    *> skipMany1 digit
    *> string ", column "
    *> skipMany1 digit
    *> string "):\nunexpected end of input\nexpecting \"(\", \"[\", \"{\", \"<\" or \""
    *> parseCloser

fixIncomplete :: String -> String
fixIncomplete s =
  let go suffix (Right _) = suffix
      go suffix (Left e) =
        let m = show e
            (Right c) = runParser parseIncompleteMessage () "" m
            nextSuffix = suffix ++ [c]
         in go nextSuffix (parseLine (s ++ nextSuffix))
   in go "" (parseLine s)

scoreFix :: String -> Int
scoreFix s =
  let score c = case c of
        ')' -> 1
        ']' -> 2
        '}' -> 3
        '>' -> 4
   in foldl' (\accum c -> (5 * accum) + (score c)) 0 s

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

doThing :: [String] -> Int
doThing ls =
  let parsedLines = fmap parseLine ls
      (failures, _) = partitionEithers parsedLines
      zipped = zip failures ls -- they all fail, by design
      isIncomplete (e, _) = List.isInfixOf "end of input" . show $ e
      eofFailures = filter isIncomplete zipped
      (_, incompleteLines) = unzip eofFailures
      scores = List.sort $ fmap (scoreFix . fixIncomplete) incompleteLines
      middle = (length scores) `div` 2
   in head . drop middle $ scores

scoreErrors :: IO ()
scoreErrors = do
  input <- getContents
  let ls = lines input
  print $ doThing ls
