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

import Control.Arrow hiding (left)
import Control.Exception (assert)
import Control.Lens hiding (contains, (...))
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Loops as Loops
import qualified Control.Monad.State.Lazy as S
import Data.Bifunctor.Swap (swap)
import Data.Either (either)
import Data.Foldable hiding (find, null, toList)
import Data.Function (on)
import Data.Int (Int32 (..))
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Monoidal as MMap
import Data.Map.Strict (Map, (!), (!?))
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Monoid (Endo (..), Product (..), Sum (..), getSum)
import Data.Ord (Down (..))
import Data.PQueue.Prio.Max (MaxPQueue)
import qualified Data.PQueue.Prio.Max as MaxPQueue
import Data.Ratio
import Data.Set (Set (..), union)
import qualified Data.Set as Set
import qualified Data.Vector as V
import Debug.Trace
import Lib (MyParser, parseInt, parseStdin)
import MultiInterval
import Text.Parsec
import Text.Parsec.Char
import Text.Show.Functions
import Prelude hiding (null)

-- Aliasing the integer type to use, for ease of refactoring yet again:
type IntR = Int

data Register = W | X | Y | Z deriving (Eq, Show, Ord)

data BaseALU a = ALU
  { _w :: a,
    _x :: a,
    _y :: a,
    _z :: a
  }
  deriving (Eq, Ord, Show)

makeLenses ''BaseALU

data Variable = Reg {_r :: Register} | Constant {_c :: IntR} deriving (Eq, Show, Ord)

getR :: Register -> BaseALU a -> a
getR W = _w
getR X = _x
getR Y = _y
getR Z = _z

setR :: Register -> a -> BaseALU a -> BaseALU a
setR W = set w
setR X = set x
setR Y = set y
setR Z = set z

getV :: Num a => Variable -> BaseALU a -> a
getV (Reg r) = getR r
getV (Constant c) = const $ fromIntegral c

type ALU = BaseALU IntR

-- The 'N' stands for non-deterministic :-P
type NALU = BaseALU (MultiInterval IntR)

data Instruction
  = Inp {_lineNumber :: IntR, _a :: Register}
  | Add {_lineNumber :: IntR, _a :: Register, _b :: Variable}
  | Mul {_lineNumber :: IntR, _a :: Register, _b :: Variable}
  | Div {_lineNumber :: IntR, _a :: Register, _b :: Variable}
  | Mod {_lineNumber :: IntR, _a :: Register, _b :: Variable}
  | Eql {_lineNumber :: IntR, _a :: Register, _b :: Variable}
  | Lod {_lineNumber :: IntR, _a :: Register, _b :: Variable}
  deriving (Eq, Show, Ord)

makeLenses ''Instruction

isInp :: Instruction -> Bool
isInp (Inp _ _) = True
isInp _ = False

parseProgram :: MyParser [Instruction]
parseProgram =
  let parseW = const W <$> char 'w'
      parseX = const X <$> char 'x'
      parseY = const Y <$> char 'y'
      parseZ = const Z <$> char 'z'
      parseRegister = parseW <|> parseX <|> parseY <|> parseZ
      parseVariable =
        Reg <$> parseRegister
          <|> Constant . fromIntegral <$> parseInt
      lineNum = fromIntegral . sourceLine <$> getPosition
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
  readInt :: m IntR

instance IntReader IO where
  readInt = read <$> readLn

instance IntReader (S.State [IntR]) where
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
  = Inverted {_r1 :: Register, _values :: MultiInterval IntR}
  | Codependency
      { _r1 :: Register,
        _r2 :: Register,
        _r1ToR2 :: IntR -> MultiInterval IntR,
        _r2ToR1 :: IntR -> MultiInterval IntR
      }

invertInstruction :: Instruction -> MultiInterval IntR -> MultiInterval IntR -> IntR -> Inversion
invertInstruction instr xBounds yBounds z =
  let -- x + y = z ==> x = z - y || y = z - x
      invAdd = singleton . (z -)
      invAddXToY = (`intersection` yBounds) . invAdd
      invAddYToX = (`intersection` xBounds) . invAdd
      -- x * y = z ==> x = z / y || y = z / x
      invMul x =
        let y = z `div` x
            check = x * y == z
         in if check then singleton y else empty
      invMulXToY 0 = yBounds
      invMulXToY x = (`intersection` yBounds) $ invMul x
      invMulYToX 0 = xBounds
      invMulYToX y = (`intersection` xBounds) $ invMul y
      -- x / y = z ==> y = x / z
      invDivXToY x =
        let y = x `div` z
         in if
                | y < 0 -> error "danger will robinson"
                | y == 0 -> empty
                | otherwise -> singleton y `intersection` yBounds
      -- x / y = z ==> x = y * z
      invDivYToX y =
        let ay = abs y
            az = abs z
            lower = ay * az
            upper = lower + ay - 1
            sign = (signum y) * (signum z)
            xs =
              if
                  | z == 0 -> (- upper) ... upper
                  | sign == 1 -> lower ... upper
                  | otherwise -> (- upper) ... (- lower)
         in xs `intersection` xBounds
      xMax = upperBound xBounds
      yMax = upperBound yBounds
      invModXToY x
        | z > x = empty
        | x == z && yMax > z = (yBounds `intersection`) $ (z + 1) ... yMax
        | x == z = empty
        | otherwise = (yBounds `intersection`) . foldMap singleton $ filter (\y -> (x `mod` y) == z) [(z + 1) .. (x - z)]
      invModYToX y
        | z < y = (xBounds `intersection`) . foldMap singleton $ takeWhile (<= xMax) [z, (z + y) ..]
        | z >= y = empty
      invEql bounds operand
        | operand == z = singleton operand `intersection` bounds
        | operand /= z = bounds `diff` singleton operand
      invEqlXToY = invEql yBounds
      invEqlYToX = invEql xBounds
      invLodXToY x = singleton x `intersection` yBounds
      invLodYToX y = xBounds -- x could have been anything before it was smashed with y
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
      go (i@(Lod _ _ (Constant _)) : _) =
        memoize i $ return $ Terminal i
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
         in memoize i $ case (i, b) of
              (Lod _ _ (Reg b), _) ->
                do
                  parent <- definitelyGo b
                  return $ Linear i parent
              (_, Constant _) ->
                do
                  parent <- definitelyGo a
                  return $ Linear i parent
              (_, Reg r) ->
                do
                  lhs <- definitelyGo a
                  rhs <- definitelyGo r
                  return $ Fork i lhs rhs
      maybeGo [] = Nothing
      maybeGo xs = Just . go $ xs
      graphState = go . reverse $ instructions
   in S.evalState graphState Map.empty

emulateInstruction :: Instruction -> IntR -> ProgramGraph
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
        case (i, simpleP) of
          (Lod _ _ _, Terminal (Lod _ _ (Constant c))) -> return . Terminal $ i & b .~ (Constant c)
          (_, Terminal (Lod _ _ (Constant c))) -> return $ emulateInstruction i c
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

rangeInBounds :: MultiInterval IntR -> [IntR] -> [IntR]
rangeInBounds _ [] = []
rangeInBounds bounds as@(a : _) =
  let lower = lowerBound bounds
      upper = upperBound bounds
   in if a > upper
        then []
        else takeWhile (bounds `contains`) . dropWhile (< lower) $ as

nRunInstruction :: NALU -> Instruction -> NALU
nRunInstruction nalu instr =
  let aReg = instr ^. a
      as = getR aReg nalu
      bs = getV (_b instr) nalu
      as' = case instr of
        (Inp _ _) -> 1 ... 9
        (Lod _ _ _) -> bs
        (Add _ _ _) -> as + bs
        (Mul _ _ _) -> as * bs
        (Div _ _ _) -> as `iquot` bs
        (Mod _ _ _) -> as `imod` bs
        (Eql _ _ _) ->
          let abs = as `intersection` bs
           in if
                  | null abs -> singleton 0
                  | isSingleton as && isSingleton bs && as == bs -> singleton 1
                  | otherwise -> 0 ... 1
   in setR aReg as' nalu

type InstructionBounds = Map IntR NALU

boundsFromProgram :: [Instruction] -> InstructionBounds
boundsFromProgram =
  let initialNALU = ALU (0 ... 0) (0 ... 0) (0 ... 0) (0 ... 0)
      go _ accum [] = accum
      go nalu accum (instr : program) =
        let nalu' = nRunInstruction nalu instr
            lineNumber = _lineNumber instr
            accum' = Map.insert lineNumber nalu' accum
         in go nalu' accum' program
   in go initialNALU $ Map.singleton 0 initialNALU

-- Attempting to workaround excessively large interval problems
-- created by the standard "clamp".
-- We can only use the NALU to reason about the value of the
-- B register (except for Inp instructions, where we can reason
-- about A)
-- powerClamp :: NALU -> ProgramGraph -> MultiInterval IntR
-- powerClamp nalu term@(Terminal (Inp _ a)) =
--   let regular = clamp term
--       powerful = getR a nalu
--       intersected = intersection regular <$> powerful
--    in Maybe.fromMaybe regular intersected
-- clamp (Linear (Eql _ _ _) _) = Range 0 1
-- clamp (Fork (Eql _ _ _) _ _) = Range 0 1
-- powerClamp nalu (Linear (Lod _ a (Reg b)) p) =
--   let bs = nalu !? b
--    in Maybe.fromMaybe (clamp p) bs
-- powerClamp nalu (Linear (Eql _ a (Constant c)) p) =
--   let maybeAs = Elements <$> nalu !? a
--       as = Maybe.fromMaybe (clamp p) maybeAs
--       inclusive = contains as c
--       exclusive = upperBound as == c && lowerBound as == c
--    in if
--           | exclusive -> Range 1 1
--           | inclusive -> Range 0 1
--           | otherwise -> Range 0 0
-- powerClamp _ g = clamp g

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

invertNode :: InstructionBounds -> NALU -> ProgramGraph -> Maybe NALU
invertNode instructionBounds nalu node =
  let instruction = node ^. instr
      priorBounds =
        let ln = instruction ^. lineNumber
            earlier = Map.takeWhileAntitone (< ln) instructionBounds
            (Just (prior, _)) = Map.maxView earlier
         in prior
      aReg = instruction ^. a
      bReg = _r . _b $ instruction
      possibleValues r =
        let rs1 = getR r priorBounds
            rs2 = getR r nalu
         in if
                | null rs2 -> rs1
                | null rs1 -> rs2
                | otherwise -> rs1 `intersection` rs2
      outputAs = getR aReg nalu
      as = getR aReg priorBounds
      bs = possibleValues bReg
      pInstruction = _instr . _parent $ node
      pReg = pInstruction ^. a
      ps = getR pReg priorBounds
      invertFork (Add _ _ _) = invertAdd as bs outputAs
      invertFork (Mul _ _ _) = invertMul as bs outputAs
      invertFork (Div _ _ _) = invertDiv as bs outputAs
      invertFork (Mod _ _ _) = invertMod as bs outputAs
      invertFork (Eql _ _ _)
        | outputAs == singleton 1 = (ab, ab)
        | otherwise = (as, bs)
        where
          ab = as `intersection` bs
      invertLinear (Add _ _ (Constant c)) = fst $ invertAdd ps (singleton c) outputAs
      invertLinear (Mul _ _ (Constant c)) = fst $ invertMul ps (singleton c) outputAs
      invertLinear (Div _ _ (Constant c)) = fst $ invertDiv ps (singleton c) outputAs
      invertLinear (Mod _ _ (Constant c)) = fst $ invertMod ps (singleton c) outputAs
      invertLinear (Eql _ _ (Constant c))
        | outputAs == singleton 1 = ps `intersection` (singleton c)
        | outputAs == singleton 0 = ps `diff` (singleton c)
        | otherwise = ps
   in case node of
        Fork i _ _ ->
          let bReg = _r . _b $ i
              (as', bs') = invertFork i
              updated = setR aReg as' . setR bReg bs' $ nalu
           in if null as' || null bs' then Nothing else Just updated
        Linear (Lod _ _ (Reg b)) p ->
          let bs' = outputAs `intersection` bs
              as' = empty
              updated = setR aReg as' . setR b bs' $ nalu
           in if null bs' then Nothing else Just updated
        Linear i _ ->
          let ps' = invertLinear i
              updated = setR aReg ps' nalu
           in if null ps' then Nothing else Just updated
        Terminal (Inp _ _) ->
          let check = (not . null . (`intersection` (1 ... 9)) $ outputAs)
              updated = setR aReg empty nalu
           in if check then Just updated else Nothing
        Terminal (Lod _ _ (Constant c)) ->
          let updated = setR aReg empty nalu
           in if outputAs `contains` c then Just updated else Nothing

-- possibleValues :: NALU -> ProgramGraph -> MultiInterval IntR
-- possibleValues nalu g =
--   let maybeOutputs = (nalu !?) . _a . _instr $ g
--       clampOutputs = powerClamp nalu g
--    in Maybe.fromMaybe clampOutputs maybeOutputs

-- solve :: NALU -> ProgramGraph -> NALU
-- -- Inp and Lod instructions don't allow us to infer anything about the value that might
-- -- have been in register A before it was executed.
-- -- We have to clear the possible values of register A before moving onto
-- -- the previous instruction.
-- solve nalu (Terminal (Inp _ a)) = Map.delete a nalu
-- solve nalu (Terminal (Lod _ a (Constant c))) =
--   let as = nalu ! a
--       nalu' = Map.delete a nalu
--    in assert (as `contains` c) nalu'
-- solve nalu (Linear (Lod _ a (Reg b)) p) =
--   -- Weird case that is constructed through simplification of the program graph.
--   -- This simply copies the value of register B into register A
--   -- For such an instruction, we should be able to get away with setting the
--   -- new values B to the intersection of their current possible
--   -- values.
--   -- Like the other `Lod` case, we will still need to clear the possible values
--   -- for register A, as we can't infer anything about its previous value from
--   -- a load instruction.
--   let as = nalu ! a
--       bs = possibleValues nalu p
--       bs' = MultiInterval.intersection as bs
--       nalu' = Map.delete a . Map.insert b bs' $ nalu
--    in assert (not . MultiInterval.null $ bs') nalu'
-- solve nalu (Linear instr p) =
--   let a = _a instr
--       (Constant b) = _b instr
--       outputAs = toList $ nalu ! a
--       aBounds = powerClamp nalu p

--       as = (`intersection` aBounds) . _values . invertInstruction instr aBounds (singleton b) <$> outputAs
--       as' = fold as
--    in Map.insert a as' nalu
-- solve nalu (Fork instr l r) =
--   let a = _a instr
--       b = _r . _b $ instr
--       -- We can't infer the possible input A register values from the current
--       -- state of the NALU, so we always need to use the `clamp` function to
--       -- guess what they could be.
--       as = clamp l
--       -- The possible B register values may already be known in the NALU.
--       -- (they won't change as a result of this instruction)
--       bs = possibleValues nalu r
--       outputAs = toList $ nalu ! a
--       -- Eliminate any infinite inversion output lists
--       solveYtoX boundsY cyToX =
--         let ys = MultiInterval.toList boundsY
--             xss = cyToX <*> ys
--             xs = fold xss
--          in (xs, boundsY)
--       -- find the smaller range to examine possibilities over:
--       aSpaceSize = size as
--       bSpaceSize = size bs
--       inv = invertInstruction instr as bs
--       lToR = _r1ToR2 . inv <$> outputAs
--       rToL = _r2ToR1 . inv <$> outputAs
--       (as', bs') =
--         if aSpaceSize >= bSpaceSize
--           then solveYtoX bs rToL
--           else -- Inverting in the other direction, so we need to swap
--           -- the answers for the left register back into the left
--           -- element of the tuple.
--             swap $ solveYtoX as lToR
--    in Map.fromList [(a, as'), (b, bs')] <> nalu

graphToList :: ProgramGraph -> [ProgramGraph]
graphToList root =
  let go Nothing = []
      go (Just (g, queue)) =
        let queue' = case g of
              Terminal _ -> queue
              Linear _ p ->
                let lineNo = p ^. instr . lineNumber
                 in Map.insert lineNo p queue
              Fork _ l r ->
                let lLine = l ^. instr . lineNumber
                    rLine = r ^. instr . lineNumber
                 in Map.insert lLine l . Map.insert rLine r $ queue
         in g : (go $ Map.maxView queue')
   in go (Just (root, Map.empty))

solveBackwards :: InstructionBounds -> ProgramGraph -> NALU -> Maybe [(IntR, NALU)]
solveBackwards instructionBounds program nalu =
  let go nalu Nothing = Just []
      go nalu (Just (g, queue)) =
        let ln = g ^. instr . lineNumber
            queue' = case g of
              Terminal _ -> queue
              Linear _ p ->
                let lineNo = p ^. instr . lineNumber
                 in Map.insert lineNo p queue
              Fork _ l r ->
                let lLine = l ^. instr . lineNumber
                    rLine = r ^. instr . lineNumber
                 in Map.insert lLine l . Map.insert rLine r $ queue
         in do
              nalu' <- invertNode instructionBounds nalu g
              went <- go nalu' $ Map.maxView queue'
              return $ (ln, nalu) : went
   in go nalu $ Just (program, Map.empty)

solveForwards :: NALU -> ProgramGraph -> Maybe ([IntR], ProgramGraph)
solveForwards nalu = gogo []
  where
    gogo guesses g =
      let program = linearise g
          (prefix, suffix) = List.span (not . isInp) program
          instructionBounds = boundsFromProgram program
          solutionBounds = solveBackwards instructionBounds g nalu
          go _ [] = Just (guesses, g) -- no input instructions, no further rewriting needed
          go Nothing _ = Nothing
          go (Just bounds) ((Inp lineNumber register) : suffix') =
            let (Just (_, inputNalu)) = List.find ((== lineNumber) . fst) bounds
                inputBounds = MultiInterval.toList . getR register $ inputNalu
                attempt c =
                  let guesses' = c : guesses
                      load = (Lod lineNumber register (Constant c))
                      rewritten = prefix ++ (load : suffix')
                      regraphed = traceShow guesses' $ (simplify . graphify $ rewritten)
                   in gogo guesses' regraphed
                successful = Maybe.catMaybes . fmap attempt $ inputBounds
             in fst <$> List.uncons successful
       in go solutionBounds suffix

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

runWithInputs :: [IntR] -> [Instruction] -> ALU
runWithInputs inputs instructions =
  S.evalState (runProgram instructions) inputs

runWithInputs2 :: [IntR] -> [Instruction] -> ALU
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
  let nalu1 = ALU empty empty empty (0 ... 0)
  let instructionBounds = boundsFromProgram p1
  let (Just (solution, simplified)) = solveForwards nalu1 simple1
  -- let solution = solveForwards nalu1 simple1
  print . reverse $ solution
  traverse_ print $ linearise simplified

-- print $ runWithInputs [5, 1, 9, 8, 3, 9, 9, 9, 9, 4, 7, 9, 9, 9] parsed

-- print $ clamp problemChild
