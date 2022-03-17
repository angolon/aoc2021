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
import Control.Exception (assert)
import Control.Lens hiding (contains)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Loops as Loops
import qualified Control.Monad.State.Lazy as S
import Data.Bifunctor.Swap (swap)
import Data.Either (either)
import Data.Foldable hiding (find, toList)
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

truncateDiv :: (Integral a) => a -> a -> a
truncateDiv a b = truncate $ (toRational a) / (toRational b)

instructionOp :: (Integral a) => Instruction -> a -> a -> a
instructionOp (Add _ _ _) = (+)
instructionOp (Mul _ _ _) = (*)
instructionOp (Div _ _ _) = truncateDiv
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
        let ay = abs y
            az = abs z
            lower = ay * az
            upper = lower + ay - 1
            sign = (signum y) * (signum z)
         in if
                | z == 0 -> [(- upper) .. upper]
                | sign == 1 -> [lower .. upper]
                | otherwise -> [(- upper) .. (- lower)]
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

toSet :: Bound -> Set Int
toSet (Elements es) = es
toSet range = Set.fromList . toList $ range

intersection :: Bound -> Bound -> Bound
intersection a b = Elements $ Set.intersection (toSet a) (toSet b)

lowerBound :: Bound -> Int
lowerBound (Range a _) = a
lowerBound (Elements es) = fst . Maybe.fromJust . Set.minView $ es

upperBound :: Bound -> Int
upperBound (Range _ b) = b
upperBound (Elements es) = fst . Maybe.fromJust . Set.maxView $ es

rangeInBounds :: Bound -> [Int] -> [Int]
rangeInBounds _ [] = []
rangeInBounds bounds as@(a : _) =
  let lower = lowerBound bounds
      upper = upperBound bounds
   in if a > upper
        then []
        else takeWhile (contains bounds) . dropWhile (< lower) $ as

clamp :: ProgramGraph -> Bound
clamp (Terminal (Inp _ _)) = Range 1 9
clamp (Linear (Eql _ _ _) _) = Range 0 1
clamp (Fork (Eql _ _ _) _ _) = Range 0 1
clamp (Terminal (Lod _ _ (Constant c))) = Range c c
clamp (Linear (Lod _ _ _) p) = clamp p
clamp (Fork (Mod _ _ _) l r) =
  let (Range _ upperL) = clamp l
      (Range _ upperR) = clamp r
      upperest = if upperL >= upperR then upperR - 1 else upperL
   in Range 0 upperest
clamp (Linear (Mod _ _ (Constant c)) p) =
  let (Range _ upperL) = clamp p
      upperest = if upperL >= c then c - 1 else upperL
   in Range 0 upperest
clamp (Fork (Div _ _ _) l r) =
  let rangeL@(Range lowerL upperL) = clamp l
      rangeR@(Range lowerR upperR) = clamp r
      -- We need to account for smaller magnitude numerators
      -- and divisors.
      numerators = lowerL : upperL : (filter (contains rangeL) [(-1), 0, 1])
      divisors = lowerR : upperR : (filter (contains rangeR) [(-1), 1])
      results = truncateDiv <$> numerators <*> divisors
   in Range (minimum results) (maximum results)
clamp (Linear (Div _ _ (Constant c)) p) =
  let rangeP@(Range lowerP upperP) = clamp p
      numerators = lowerP : upperP : (filter (contains rangeP) [(-1), 0, 1])
      results = truncateDiv <$> numerators <*> pure c
   in Range (minimum results) (maximum results)
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

-- Attempting to workaround excessively large interval problems
-- created by the standard "clamp".
-- We can only use the NALU to reason about the value of the
-- B register (except for Inp instructions, where we can reason
-- about A)
powerClamp :: NALU -> ProgramGraph -> Bound
powerClamp nalu term@(Terminal (Inp _ a)) =
  let regular = clamp term
      powerful = Elements <$> nalu !? a
      intersected = intersection regular <$> powerful
   in Maybe.fromMaybe regular intersected
-- clamp (Linear (Eql _ _ _) _) = Range 0 1
-- clamp (Fork (Eql _ _ _) _ _) = Range 0 1
powerClamp nalu (Linear (Lod _ a (Reg b)) p) =
  let bs = Elements <$> nalu !? b
   in Maybe.fromMaybe (clamp p) bs
powerClamp _ g = clamp g

-- clamp (Fork (Mod _ _ _) l r) =
--   let (Range _ upperL) = clamp l
--       (Range _ upperR) = clamp r
--       upperest = if upperL >= upperR then upperR - 1 else upperL
--    in Range 0 upperest
-- clamp (Linear (Mod _ _ (Constant c)) p) =
--   let (Range _ upperL) = clamp p
--       upperest = if upperL >= c then c - 1 else upperL
--    in Range 0 upperest
-- clamp (Linear i p) =
--   let op = instructionOp i
--       c = _c . _b $ i
--       (Range lower upper) = clamp p
--       a = lower `op` c
--       b = upper `op` c
--    in Range (min a b) (max a b)
-- clamp (Fork i l r) =
--   let (Range lowerL upperL) = clamp l
--       (Range lowerR upperR) = clamp r
--       op = instructionOp i
--       results = op <$> [lowerL, upperL] <*> [lowerR, upperR]
--    in Range (minimum results) (maximum results)

possibleValues :: NALU -> ProgramGraph -> Bound
possibleValues nalu g =
  let maybeOutputs = (fmap Elements) . (nalu !?) . _a . _instr $ g
      clampOutputs = powerClamp nalu g
   in Maybe.fromMaybe clampOutputs maybeOutputs

solve :: ProgramGraph -> NALU -> NALU
-- Inp and Lod instructions don't allow us to infer anything about the value that might
-- have been in register A before it was executed.
-- We have to clear the possible values of register A before moving onto
-- the previous instruction.
solve (Terminal (Inp _ a)) nalu = Map.delete a nalu
solve (Terminal (Lod _ a (Constant c))) nalu =
  let as = nalu ! a
      nalu' = Map.delete a nalu
   in assert (Set.member c as) nalu'
solve (Linear (Lod _ a (Reg b)) p) nalu =
  -- Weird case that is constructed through simplification of the program graph.
  -- This simply copies the value of register B into register A
  -- For such an instruction, we should be able to get away with setting the
  -- new values B to the intersection of their current possible
  -- values.
  -- Like the other `Lod` case, we will still need to clear the possible values
  -- for register A, as we can't infer anything about its previous value from
  -- a load instruction.
  let as = nalu ! a
      bs = toSet $ possibleValues nalu p
      bs' = Set.intersection as bs
      nalu' = Map.delete a . Map.insert b bs' $ nalu
   in assert (not . Set.null $ bs') nalu'
solve (Linear instr p) nalu =
  let a = _a instr
      (Constant b) = _b instr
      outputAs = Set.toList $ nalu ! a
      aBounds = powerClamp nalu p
      as = rangeInBounds aBounds . _values . invertInstruction instr <$> outputAs
      as' = foldMap Set.fromList as
   in Map.insert a as' nalu
solve (Fork instr l r) nalu =
  let a = _a instr
      b = _r . _b $ instr
      -- We can't infer the possible input A register values from the current
      -- state of the NALU, so we always need to use the `clamp` function to
      -- guess what they could be.
      as = clamp l
      -- The possible B register values may already be known in the NALU.
      -- (they won't change as a result of this instruction)
      bs = possibleValues nalu r
      outputAs = Set.toList $ nalu ! a
      -- Eliminate any infinite inversion output lists
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

solveBackwards :: ProgramGraph -> NALU -> [(Int, NALU)]
solveBackwards program nalu =
  let go nalu Nothing = []
      go nalu (Just (g, queue)) =
        let nalu' = solve g nalu
            ln = g ^. instr . lineNumber
            queue' = case g of
              Terminal _ -> queue
              Linear _ p ->
                let lineNo = p ^. instr . lineNumber
                 in Map.insert lineNo p queue
              Fork _ l r ->
                let lLine = l ^. instr . lineNumber
                    rLine = r ^. instr . lineNumber
                 in Map.insert lLine l . Map.insert rLine r $ queue
         in (ln, nalu') : (go nalu' $ Map.maxView queue')
   in go nalu $ Just (program, Map.empty)

maybeOr :: Maybe a -> Maybe a -> Maybe a
maybeOr a@(Just _) _ = a
maybeOr _ a = a

find :: (Instruction -> Bool) -> ProgramGraph -> Maybe ProgramGraph
find p g@(Terminal inst) = if p inst then Just g else Nothing
find p g@(Linear inst parent) = if p inst then Just g else find p parent
find p g =
  let inst = _instr g
   in if p inst
        then Just g
        else case g of
          Terminal _ -> Nothing
          Linear _ parent -> find p parent
          Fork _ l r -> (find p l) `maybeOr` (find p r)

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
  -- traverse_ print p1
  let nalu1 = Map.singleton Z $ Set.singleton 0
  let nalu2 = solve simple1 nalu1
  let nalu3 = solve (_right simple1) nalu2
  let nalu4 = solve (_left . _right $ simple1) nalu3
  let nalu5 = solve (_parent . _left . _right $ simple1) nalu4
  let nalu6 = solve (_left simple1) nalu5
  let nalu7 = solve (_right . _left $ simple1) nalu6
  let nalu8 = solve (_parent . _right . _left $ simple1) nalu7
  let nalu9 = solve (_left . _parent . _right . _left $ simple1) nalu8
  let nalu10 = solve (_right . _parent . _right . _left $ simple1) nalu9
  let nalu11 = solve (_parent . _right . _parent . _right . _left $ simple1) nalu10
  let nalu12 = solve (_left . _parent . _right . _parent . _right . _left $ simple1) nalu11
  let nalu13 = solve (_left . _left $ simple1) nalu12

  let problemChild = _parent . _left . _parent . _right . _parent . _right . _left $ simple1
  let nalu14 = solve problemChild nalu13
  print nalu1
  print nalu2
  print nalu3
  print nalu4
  print nalu5
  print nalu6
  print nalu7
  print nalu8
  print nalu9
  print nalu10
  print nalu11
  print nalu12
  print nalu13
  print nalu14
  putStrLn "============="
  let blargh = solveBackwards simple1 nalu1
  -- let bloop = takeWhile ((>= 185) . fst) blargh
  -- print . last $ blargh
  traverse_ print blargh
  print $ clamp problemChild
