{-# LANGUAGE FlexibleContexts #-}
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
import Control.Lens hiding (contains)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Loops as Loops
import qualified Control.Monad.State.Lazy as S
import Data.Bifunctor.Swap (swap)
import Data.Either (either)
import Data.Foldable hiding (toList)
import Data.Function (on)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map, (!), (!?))
import qualified Data.Map as Map
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MMap
import qualified Data.Maybe as Maybe
import Data.Monoid (Endo (..), Product (..), Sum (..), getSum)
import Data.Ord (Down (..))
import Data.PQueue.Prio.Max (MaxPQueue)
import qualified Data.PQueue.Prio.Max as MaxPQueue
import Data.Ratio
import Data.Set (Set (..), union)
import qualified Data.Set as Set
import qualified Data.Vector as V
import Lib (MyParser, parseInt, parseStdin)
import Text.Parsec
import Text.Parsec.Char
import Text.Show.Functions

data Register = W | X | Y | Z deriving (Eq, Show, Ord)

data ALU = ALU
  { _w :: Int,
    _x :: Int,
    _y :: Int,
    _z :: Int
  }
  deriving (Eq, Ord, Show)

makeLenses ''ALU

data Variable = Reg {_r :: Register} | Constant {_c :: Int} deriving (Eq, Show, Ord)

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
  = Inp {_lineNumber :: Int, _a :: Register}
  | Add {_lineNumber :: Int, _a :: Register, _b :: Variable}
  | Mul {_lineNumber :: Int, _a :: Register, _b :: Variable}
  | Div {_lineNumber :: Int, _a :: Register, _b :: Variable}
  | Mod {_lineNumber :: Int, _a :: Register, _b :: Variable}
  | Eql {_lineNumber :: Int, _a :: Register, _b :: Variable}
  | Lod {_lineNumber :: Int, _a :: Register, _b :: Variable}
  deriving (Eq, Show, Ord)

makeLenses ''Instruction

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
      lineNum = sourceLine <$> getPosition
      parseInp = Inp <$ try (string "inp ") <*> lineNum <*> parseRegister
      instruction constructor name =
        constructor <$ try (string (name ++ " "))
          <*> lineNum
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

instructionOp :: (Integral a) => Instruction -> a -> a -> a
instructionOp (Add _ _ _) = (+)
instructionOp (Mul _ _ _) = (*)
instructionOp (Div _ _ _) = div
instructionOp (Mod _ _ _) = mod
instructionOp (Eql _ _ _) = (\c d -> if c == d then 1 else 0)
instructionOp (Lod _ _ _) = const id

runInstruction :: (IntReader m) => Instruction -> ALU -> m ALU
runInstruction (Inp _ a) alu = do
  i <- readInt
  return $ setR a i alu
runInstruction instr alu =
  let a = _a instr
      b = _b instr
      ab = getR a &&& getV b
      op = instructionOp instr
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
  let representable = [-14 ..]
      -- x + y = z ==> x = z - y || y = z - x
      invAddXToY = pure . (z -)
      invAddYToX = invAddXToY
      -- x * y = z ==> x = z / y || y = z / x
      invMulXToY 0 = representable
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
      invEqlXToY x =
        if z == 1
          then [x]
          else -- else error "I don't want to produce this kind of infinity"
            filter (/= x) representable
      invEqlYToX = invEqlXToY
      invLodXToY x = [x]
      invLodYToX y = representable
      (xToY, yToX) = case instr of
        Add _ _ _ -> (invAddXToY, invAddYToX)
        Mul _ _ _ -> (invMulXToY, invMulYToX)
        Div _ _ _ -> (invDivXToY, invDivYToX)
        Mod _ _ _ -> (invModXToY, invModYToX)
        Eql _ _ _ -> (invEqlXToY, invEqlYToX)
        Lod _ _ _ -> (invLodXToY, invLodYToX)
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

-- runSimplifiedProgram :: (IntReader m) => [Instruction] -> m ALU
-- runSimplifiedProgram prog =
--   let g = simplify . graphify $ prog

data ProgramGraph
  = Fork {_instr :: Instruction, _left :: ProgramGraph, _right :: ProgramGraph}
  | Linear {_instr :: Instruction, _parent :: ProgramGraph}
  | Terminal {_instr :: Instruction}
  deriving (Eq, Ord)

makeLenses ''ProgramGraph

instance Show ProgramGraph where
  show =
    let indent = 2
        prefix 0 s = s ++ "\n"
        prefix depth s =
          let p = replicate ((depth * indent) - 1) ' '
           in p ++ "|- " ++ s ++ "\n"
        go depth (Terminal i) = prefix depth (show i)
        go depth (Linear i p) =
          prefix depth (show i) ++ go (depth + 1) p
        go depth (Fork i l r) =
          prefix depth (show i)
            ++ go (depth + 1) l
            ++ go (depth + 1) r
     in go 0

memoize :: (Ord k) => k -> S.State (Map k a) a -> S.State (Map k a) a
memoize k computeA = do
  memo <- S.get
  case memo !? k of
    Just a -> return a
    Nothing -> do
      a <- computeA
      S.modify $ Map.insert k a
      return a

graphify :: [Instruction] -> ProgramGraph
graphify instructions =
  let go (i@(Inp _ _) : _) = memoize i $ return $ Terminal i
      go (i@(Mul ln a (Constant 0)) : _) =
        -- sets register to zero, has no dependencies
        memoize i $ return $ Terminal (Lod ln a (Constant 0))
      go (i : is) =
        let a = _a i
            b = _b i
            dropIndependents r = dropWhile ((/= r) . _a) is
            definitelyGo r =
              let dependency = maybeGo . dropIndependents $ r
               in case dependency of
                    Just l -> l
                    -- Hack: negative line numbers to shoe-horn in explicit load zero instructions
                    Nothing ->
                      return $ Terminal (Lod (-1) a (Constant 0))
         in memoize i $ case b of
              Constant _ ->
                do
                  parent <- definitelyGo a
                  return $ Linear i parent
              Reg r ->
                do
                  lhs <- definitelyGo a
                  rhs <- definitelyGo r
                  return $ Fork i lhs rhs
      maybeGo [] = Nothing
      maybeGo xs = Just . go $ xs
      graphState = go . reverse $ instructions
   in S.evalState graphState Map.empty

emulateInstruction :: Instruction -> Int -> ProgramGraph
emulateInstruction i arg =
  let c = _c . _b $ i -- the _b value must always be a constant in order to emulate
      a = _a i
      op = instructionOp i
      result = arg `op` c
      ln = _lineNumber i
   in Terminal (Lod ln a (Constant result))

simplify :: ProgramGraph -> ProgramGraph
simplify gg =
  let simplifyM t@(Terminal _) = return t
      -- multiplying by zero just resets the register, so we can ignore all its dependencies
      simplifyM linear@(Linear (Mul ln a (Constant 0)) _) =
        memoize linear $ return $ Terminal (Lod ln a (Constant 0))
      -- Adding zero, or multiplying by 1, leaves the register unchanged, so skip this node entirely
      simplifyM (Linear (Mul _ _ (Constant 1)) p) = simplifyM p
      simplifyM (Linear (Add _ _ (Constant 0)) p) = simplifyM p
      simplifyM linear@(Linear i p) = memoize linear $ do
        simpleP <- simplifyM p
        case simpleP of
          Terminal (Lod _ _ (Constant c)) -> return $ emulateInstruction i c
          _ -> return (Linear i simpleP)
      simplifyM fork@(Fork i l r) =
        let setRhs c = i & b .~ (Constant c)
         in memoize fork $ do
              simpleL <- simplifyM l
              simpleR <- simplifyM r
              case (simpleL, simpleR) of
                (Terminal (Lod _ _ (Constant c)), Terminal (Lod _ _ (Constant d))) -> return $ emulateInstruction (setRhs d) c
                -- Pretending that the rhs is some constant value:
                (_, Terminal (Lod _ _ (Constant d))) -> simplifyM $ Linear (setRhs d) simpleL
                (Terminal (Lod _ _ (Constant 0)), _) ->
                  let reset = Terminal $ Lod (_lineNumber i) (_a i) (Constant 0)
                      simplified = case i of
                        -- Mul, Div, and Mod with zero on the LHS will always result in zero, so we can
                        -- replace these instances with `Lod 0`
                        (Mul _ _ _) -> reset
                        (Div _ _ _) -> reset
                        (Mod _ _ _) -> reset
                        -- Adding to zero is equivalent to a load instruction
                        (Add ln a b) -> Linear (Lod ln a b) simpleR
                        -- Otherwise, we can't simplify any further:
                        _ -> Fork i simpleL simpleR
                   in return simplified
                _ -> return $ Fork i simpleL simpleR
   in S.evalState (simplifyM gg) Map.empty

linearise :: ProgramGraph -> [Instruction]
linearise =
  let merge xs [] = xs
      merge [] ys = ys
      merge xs@(x : xs') ys@(y : ys') =
        let lnX = _lineNumber x
            lnY = _lineNumber y
         in if
                | x == y -> xs -- the paths have merged, and should now be identical.
                | lnX > lnY -> x : merge xs' ys
                | lnY > lnX -> y : merge xs ys'

      go (Terminal instr) = [instr]
      go (Linear instr parent) = (instr :) . go $ parent
      go (Fork instr left right) =
        let leftL = go left
            rightL = go right
            all = merge leftL rightL
         in instr : all
   in reverse . go

data Bound = Range Int Int | Elements (Set Int)
  deriving (Show, Eq, Ord)

size :: Bound -> Int
size (Range a b) = b - a + 1
size (Elements es) = Set.size es

contains :: Bound -> Int -> Bool
contains (Range b c) a = b <= a && a <= c
contains (Elements es) a = Set.member a es

toList :: Bound -> [Int]
toList (Range a b) = [a .. b]
toList (Elements es) = Set.toList es

clamp :: ProgramGraph -> Bound
clamp (Terminal (Inp _ _)) = Range 1 9
clamp (Linear (Eql _ _ _) _) = Range 0 1
clamp (Fork (Eql _ _ _) _ _) = Range 0 1
clamp (Terminal (Lod _ _ (Constant c))) = Range c c
clamp (Linear (Lod _ _ _) p) = clamp p
clamp (Linear i p) =
  let op = instructionOp i
      c = _c . _b $ i
      (Range lower upper) = clamp p
      a = lower `op` c
      b = upper `op` c
   in Range (min a b) (max a b)
clamp (Fork i l r) =
  let (Range lowerL upperL) = clamp l
      (Range lowerR upperR) = clamp r
      op = instructionOp i
      results = op <$> [lowerL, upperL] <*> [lowerR, upperR]
   in Range (minimum results) (maximum results)

-- The 'N' stands for non-deterministic :-P
type NALU = Map Register (Set Int)

possibleOutputs :: NALU -> ProgramGraph -> Bound
possibleOutputs nalu g =
  let maybeOutputs = (fmap Elements) . (nalu !?) . _a . _instr $ g
      clampOutputs = clamp g
   in Maybe.fromMaybe clampOutputs maybeOutputs

solve :: ProgramGraph -> NALU -> NALU
solve (Fork instr l r) nalu =
  let a = _a instr
      b = _r . _b $ instr
      -- We can't infer the possible input A register values from the current
      -- state of the NALU, so we always need to use the `clamp` function to
      -- guess what they could be.
      as = clamp l
      -- The possible B register values may already be known in the NALU.
      -- (they won't change as a result of this instruction)
      bs =
        let maybeBs = fmap Elements $ nalu !? b
            clampedBs = clamp r
         in Maybe.fromMaybe clampedBs maybeBs
      outputAs = Set.toList $ nalu ! a
      -- Eliminate any infinite inversion output lists
      rangeInBounds :: Bound -> [Int] -> [Int]
      rangeInBounds bounds = takeWhile (contains bounds) . dropWhile (not . contains bounds)
      solveYtoX graphX boundsX graphY boundsY cyToX cxToY =
        let ys = toList boundsY
            xs = rangeInBounds boundsX $ (cyToX <*> ys) >>= id
            -- Some `y`s in the range of `ys` may produce
            -- an `x` value which is not in the range of `xs`.
            -- They may even produce no `x` at all.
            -- Filter these `ys` out.
            ys' = rangeInBounds boundsY $ (cxToY <*> xs) >>= id
         in (Set.fromList xs, Set.fromList ys')
      -- find the smaller range to examine possibilities over:
      aSpaceSize = size as
      bSpaceSize = size bs
      lToR = _r1ToR2 . invertInstruction instr <$> outputAs
      rToL = _r2ToR1 . invertInstruction instr <$> outputAs
      (as', bs') =
        if aSpaceSize >= bSpaceSize
          then solveYtoX l as r bs rToL lToR
          else -- Inverting in the other direction, so we need to swap
          -- the answers for the left register back into the left
          -- element of the tuple.
            swap $ solveYtoX r bs l as lToR rToL
   in Map.fromList [(a, as'), (b, bs')] <> nalu

runWithInputs :: [Int] -> [Instruction] -> ALU
runWithInputs inputs instructions =
  S.evalState (runProgram instructions) inputs

runWithInputs2 :: [Int] -> [Instruction] -> ALU
runWithInputs2 inputs instructions =
  S.evalState (runProgram . linearise . simplify . graphify $ instructions) inputs

-- enterTheMonad :: IO ()
-- enterTheMonad = do
--   (Right parsed) <- parseStdin parseProgram
--   let execd = runWithInputs [7] parsed
--   print execd
--   traverse_ print parsed

enterTheMonad :: IO ()
enterTheMonad = do
  (Right parsed) <- parseStdin parseProgram
  let simple1 = simplify . graphify $ parsed
  let p1 = linearise simple1
  let initial = Map.singleton Z $ Set.singleton 0
  traverse_ print p1
  print $ solve simple1 initial
  print $ clamp simple1
