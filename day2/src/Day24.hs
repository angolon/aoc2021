{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Day24 where

import Control.Arrow
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Loops as Loops
import qualified Control.Monad.State.Lazy as S
import Data.Bifunctor.Swap (swap)
import Data.Either (either)
import Data.Foldable
import Data.Function (on)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map, (!), (!?))
import qualified Data.Map as Map
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MMap
import Data.Maybe
import Data.Monoid (Endo (..), Product (..), Sum (..), getSum)
import Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.PQueue.Prio.Min as MinPQueue
import Data.Ratio
import Data.Set (Set, member, union, (\\))
import qualified Data.Set as Set
import qualified Data.Vector as V
import Lib (MyParser, parseInt, parseStdin)
import Text.Parsec
import Text.Parsec.Char
import Text.Show.Functions

data Register = W | X | Y | Z deriving (Eq, Show)

data ALU = ALU
  { _w :: Int,
    _x :: Int,
    _y :: Int,
    _z :: Int
  }
  deriving (Eq, Show)

makeLenses ''ALU

data Variable = Reg {_r :: Register} | Constant {_c :: Int} deriving (Eq, Show)

getR :: Register -> ALU -> Int
getR W = _w
getR X = _x
getR Y = _y
getR Z = _z

setR :: Register -> Int -> ALU -> ALU
setR W = set w
setR X = set x
setR Y = set y
setR Z = set z

getV :: Variable -> ALU -> Int
getV (Reg r) = getR r
getV (Constant c) = const c

data Instruction
  = Inp {_a :: Register}
  | Add {_a :: Register, _b :: Variable}
  | Mul {_a :: Register, _b :: Variable}
  | Div {_a :: Register, _b :: Variable}
  | Mod {_a :: Register, _b :: Variable}
  | Eql {_a :: Register, _b :: Variable}
  deriving (Eq, Show)

parseProgram :: MyParser [Instruction]
parseProgram =
  let parseW = const W <$> char 'w'
      parseX = const X <$> char 'x'
      parseY = const Y <$> char 'y'
      parseZ = const Z <$> char 'z'
      parseRegister = parseW <|> parseX <|> parseY <|> parseZ
      parseVariable =
        Reg <$> parseRegister
          <|> Constant <$> parseInt
      parseInp = Inp <$ try (string "inp ") <*> parseRegister
      instruction constructor name =
        constructor <$ try (string (name ++ " "))
          <*> parseRegister <* char ' '
          <*> parseVariable
      parseInstruction =
        parseInp
          <|> instruction Add "add"
          <|> instruction Mul "mul"
          <|> instruction Div "div"
          <|> instruction Mod "mod"
          <|> instruction Eql "eql"
   in sepEndBy1 parseInstruction endOfLine <* eof

class (Monad m) => IntReader m where
  readInt :: m Int

instance IntReader IO where
  readInt = read <$> readLn

instance IntReader (S.State [Int]) where
  readInt = do
    xs <- S.get
    let (x : tail) = xs
    S.put tail
    return x

runInstruction :: (IntReader m) => Instruction -> ALU -> m ALU
runInstruction (Inp a) alu = do
  i <- readInt
  return $ setR a i alu
runInstruction instr alu =
  let a = _a instr
      b = _b instr
      ab = getR a &&& getV b
      op = case instr of
        (Add _ _) -> (+)
        (Mul _ _) -> (*)
        (Div _ _) -> div
        (Mod _ _) -> mod
        (Eql _ _) -> (\c d -> if c == d then 1 else 0)
      opAB = (uncurry op) . ab
   in return $ setR a (opAB alu) alu

data Inversion
  = Inverted {_r1 :: Register, _values :: [Int]}
  | Codependency
      { _r1 :: Register,
        _r2 :: Register,
        _r1ToR2 :: Int -> [Int],
        _r2ToR1 :: Int -> [Int]
      }

-- invertInstruction :: Int -> Instruction ->
invertInstruction instr z =
  let -- x + y = z ==> x = z - y || y = z - x
      invAddXToY = pure . (z -)
      invAddYToX = invAddXToY
      -- x * y = z ==> x = z / y || y = z / x
      invMulXToY x =
        let y = z `div` x
            check = x * y == z
         in if check then [y] else []
      invMulYToX = invMulXToY
      -- x / y = z ==> y = x / z
      invDivXToY x =
        let y = x `div` z
         in if
                | y < 0 -> error "danger will robinson"
                | y == 0 -> []
                | otherwise -> [y]
      -- x / y = z ==> x = y * z
      invDivYToX y =
        let lower = y * z
            upper = lower + y - 1
         in [lower .. upper]
      invModXToY x =
        let brutes = filter (\y -> (x `mod` y) == z) [(z + 1) .. (x - z)]
         in if z == x then [(z + 1) ..] else brutes
      invModYToX y =
        if z >= y then [] else [z, (z + y) ..]
      (xToY, yToX) = case instr of
        Add _ _ -> (invAddXToY, invAddYToX)
        Mul _ _ -> (invMulXToY, invMulYToX)
        Div _ _ -> (invDivXToY, invDivYToX)
        Mod _ _ -> (invModXToY, invModYToX)
      a = _a instr
      b = _b instr
   in case b of
        Constant c ->
          Inverted a (yToX c)
        Reg r ->
          Codependency a r xToY yToX

runProgram :: (IntReader m) => [Instruction] -> m ALU
runProgram =
  let init = return $ ALU 0 0 0 0
   in foldl (\b a -> b >>= runInstruction a) init

runWithInputs :: [Int] -> [Instruction] -> ALU
runWithInputs inputs instructions =
  S.evalState (runProgram instructions) inputs

data ProgramGraph
  = Fork {_instr :: Instruction, _left :: ProgramGraph, _right :: ProgramGraph}
  | Linear {_instr :: Instruction, _parent :: ProgramGraph}
  | Terminal {_instr :: Instruction}
  | Unset
  deriving (Eq, Show)

graphify :: [Instruction] -> ProgramGraph
graphify instructions =
  let go [] = Unset
      go (i@(Inp _) : _) = Terminal i
      go (i : is) =
        let a = _a i
            b = _b i
            dropIndependents r = dropWhile ((/= r) . _a) is
            lhs = dropIndependents a
         in case b of
              Constant _ -> Linear i $ go lhs
              Reg r ->
                let rhs = dropIndependents r
                 in Fork i (go lhs) (go rhs)
   in go . reverse $ instructions

-- blergh prog =
--   let x =

enterTheMonad :: IO ()
enterTheMonad = do
  (Right parsed) <- parseStdin parseProgram
  let execd = runWithInputs [7] parsed
  print execd
  traverse_ print parsed
