{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Day12 where

import Control.Lens
import Control.Monad
import Data.Bifunctor.Swap (swap)
import Data.Foldable
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isNothing, maybeToList)
import Data.Monoid (Sum (..), getSum)
import Data.Set (Set, (\\))
import qualified Data.Set as Set
import Data.Vector ((!?))
import qualified Data.Vector as V
import GHC.Generics
import Lib (MyParser, parseInt, parseStdin)
import Text.Parsec
import Text.Parsec.Char

data Cave = Start | End | Little String | Big String
  deriving (Eq, Ord, Show)

isSmall :: Cave -> Bool
isSmall (Little _) = True
isSmall _ = False

type CaveSystem = Map Cave (NonEmpty Cave)

parseCave :: MyParser Cave
parseCave =
  const Start <$> try (string "start")
    <|> const End <$> try (string "end")
    <|> Little <$> many1 lower
    <|> Big <$> many1 upper

parseConnection :: MyParser (Cave, Cave)
parseConnection = (,) <$> parseCave <* char '-' <*> parseCave

parseConnections :: MyParser [(Cave, Cave)]
parseConnections = many1 (parseConnection <* endOfLine) <* eof

groupConnections :: [(Cave, Cave)] -> CaveSystem
groupConnections cs =
  let bidirectional = cs ++ (fmap swap cs)
      sane = filter ((/= Start) . snd) . filter ((/= End) . fst) $ bidirectional
      grouped = NonEmpty.groupAllWith fst sane
      mapKey :: NonEmpty (Cave, Cave) -> (Cave, NonEmpty Cave)
      mapKey g =
        let k = fst . NonEmpty.head $ g
            vs = fmap snd g
         in (k, vs)
      keyed = fmap mapKey grouped
   in Map.fromList keyed

validatePath :: NonEmpty Cave -> Bool
validatePath caves =
  let smalls = NonEmpty.filter isSmall caves
      grouped = NonEmpty.groupAllWith id smalls
   in all ((== 1) . NonEmpty.length) grouped

type Path = NonEmpty Cave

type Paths = NonEmpty Path

-- Doesn't check for finishedness.
stepPath :: CaveSystem -> Path -> Paths
stepPath system cs@(c :| _) =
  let heads = system Map.! c
   in fmap (NonEmpty.<| cs) heads

findPaths :: CaveSystem -> [Path]
findPaths caves =
  let finished :: Path -> Bool
      finished = (== End) . NonEmpty.head
      go :: [Path] -> [Path]
      go paths =
        let (finishedPaths, unfinished) = List.partition finished $ paths
         in if List.null unfinished
              then paths
              else
                let nextPaths = (NonEmpty.fromList unfinished) >>= stepPath caves
                    sensiblePaths = NonEmpty.filter validatePath nextPaths
                 in go (sensiblePaths ++ finishedPaths)
   in go [Start :| []]

exploreCaves :: IO ()
exploreCaves = do
  (Right parsed) <- parseStdin parseConnections
  let paths = findPaths . groupConnections $ parsed
  traverse_ (print . NonEmpty.reverse) paths
  print . length $ paths
