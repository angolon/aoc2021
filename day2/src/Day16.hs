{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Day16 where

import Control.Applicative
import Control.Arrow (arr, (&&&))
import Control.Lens
import Control.Monad
import Control.Monad.Catch (throwM)
import Data.Bifunctor.Swap (swap)
import Data.Bits (shiftL)
import qualified Data.Bits as Bits
import Data.Foldable
import Data.Function (on)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MMap
import Data.Maybe (isNothing, mapMaybe, maybeToList)
import Data.Monoid (Sum (..), getSum)
import Data.PQueue.Min (MinQueue)
import qualified Data.PQueue.Min as MinQueue
import Data.Ratio
import Data.Set (Set, union, (\\))
import qualified Data.Set as Set
import Data.Text (pack)
import Data.Text.Read (hexadecimal)
import Data.Vector ((!), (!?))
import qualified Data.Vector as V
import Data.Word (Word8)
import Day3 (toInt)
import GHC.Generics
import Lib (MyParser, parseInt, parseStdin)
import Text.Parsec
import Text.Parsec.Char

parseHexDigit :: MyParser [Bool]
parseHexDigit =
  toBits . squelch . hexadecimal . pack . (: []) <$> hexDigit
  where
    squelch (Right (i, _)) = i
    squelch (Left e) = error e
    bs = reverse [0 .. 3]
    toBits x = fmap (Bits.testBit @Word8 x) bs

parseHexDigits :: MyParser [Bool]
parseHexDigits = join <$> many1 parseHexDigit

type BitParser = Parsec [Bool] ()

bitPrim :: BitParser Bool
bitPrim = tokenPrim show (\pos _ _ -> incSourceColumn pos 1) Just

bitTokens :: [Bool] -> BitParser [Bool]
bitTokens bs = tokens show (\pos bs -> incSourceColumn pos $ length bs) bs

nBitsToInt :: Int -> BitParser Int
nBitsToInt n = toInt <$> count n bitPrim

-- parseVersion :: BitParser Word8
-- parseVersion = count 3 bitPrim

parseLiteral :: BitParser Int
parseLiteral =
  let parseChunks cs = do
        h <- bitPrim
        chunk <- count 4 bitPrim
        if h
          then parseChunks $ cs ++ chunk
          else return $ cs ++ chunk
   in toInt <$> parseChunks []

data Header = Header {version :: Int, pid :: Int} deriving (Eq, Show)

data Packet
  = Literal {header :: Header, lit :: Int}
  | Operator {header :: Header, subPackets :: [Packet]}
  deriving (Eq, Show)

parsePacket :: BitParser Packet
parsePacket = do
  version <- nBitsToInt 3
  pid <- nBitsToInt 3
  let header = Header version pid
  if pid == 4
    then Literal header <$> parseLiteral
    else Operator header <$> parseOperator

parseLengthSpecified =
  let go n packets = do
        packet <- parsePacket
        pos <- getPosition
        let nextPackets = packet : packets
        let col = sourceColumn pos
        if (n == col)
          then return $ nextPackets
          else go n nextPackets
   in do
        n <- nBitsToInt 15
        pos <- getPosition
        let col = sourceColumn pos
        go (col + n) []

parseNSpecified = do
  n <- nBitsToInt 11
  count n parsePacket

parseOperator = do
  b <- bitPrim
  if b
    then parseNSpecified
    else parseLengthSpecified

decodePackets :: IO ()
decodePackets = do
  (Right parsedHex) <- parseStdin parseHexDigits
  print parsedHex
  print $ runParser parsePacket () "" parsedHex
