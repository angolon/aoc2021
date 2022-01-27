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
import Data.Ord (Down (..))
import Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.PQueue.Prio.Min as MinPQueue
import Data.Ratio
import Data.Set (union)
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

instructionOp :: Instruction -> Int -> Int -> Int
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
      invEqlXToY x =
        if z == 1
          then [x]
          else error "I don't want to produce this kind of infinity"
      invLodXToY x = [x]
      invLodYToX y = error "Too many possibilities"
      invEqlYToX = invEqlXToY
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

data GraphifyS = GraphifyS
  { _order1 :: MonoidalMap Register [ProgramGraph -> ProgramGraph],
    _order2 :: MonoidalMap (Register, Register) [ProgramGraph -> ProgramGraph -> ProgramGraph],
    _completed :: [ProgramGraph]
  }

makeLenses ''GraphifyS

graphify :: [Instruction] -> ProgramGraph
graphify instructions =
  let go2 ::
        [Instruction] -> S.State GraphifyS ()
      go2 (i : is) =
        let a = _a i
            b = _b i
            blah :: S.State GraphifyS ()
            blah = do
              afs <- (^.. order1 . itraversed . index a . traversed) <$> S.get
              bfs <- (^.. order2 . itraversed . indices ((== a) . fst) . withIndex) <$> S.get
              cfs <- (^.. order2 . itraversed . indices ((== a) . snd) . withIndex) <$> S.get
              let cfs' = cfs & mapped . _2 . mapped %~ flip
              let bfs' = bfs & mapped . _1 %~ fst
              let cfs'' = cfs & mapped . _1 %~ snd
              let order2fs = foldMap (uncurry MMap.singleton) (bfs' ++ cfs'')
              let dfs = bfs ++ cfs'
              let dKeys = fmap fst dfs
              -- Regardless of what kind of instruction we're graphifying, any order1 constructors
              -- depending on register a will be subsumed.
              -- We'll need to replace all matching order2 constructors with order1 constructors
              -- which we've partially applied?
              let staleOrder2Keys = MMap.fromList . fmap (,()) $ dKeys
              S.modify
                ( \st ->
                    st & order1 %~ MMap.delete a
                      & order2 %~ (MMap.\\ staleOrder2Keys)
                )
              -- TODO: fixup the state.
              let finishers =
                    let t = Terminal i
                        finished = fmap ($ t) $ afs
                        unfinished = MMap.map (fmap ($ t)) order2fs
                     in S.modify
                          ( \st ->
                              st & completed %~ (finished ++)
                                & order1 %~ (unfinished <>)
                          )
              case (i, b) of
                ((Inp _ _), _) -> finishers
                ((Mul ln a (Constant 0)), _) -> finishers
                (_, (Constant _)) ->
                  let constructor = Linear i
                      afs' = fmap (. constructor) afs
                      order2fs' =
                        MMap.foldMapWithKey
                          ( \k fs ->
                              let fs' =
                                    fmap
                                      ( \f ->
                                          ( \a' b' ->
                                              f (constructor a') b'
                                          )
                                      )
                                      fs
                               in MMap.singleton (a, k) fs'
                          )
                          order2fs
                   in S.modify
                        ( \st ->
                            st & order1 %~ MMap.insert a afs'
                              & order2 %~ (order2fs' <>)
                        )
                (_, (Reg r)) ->
                  let constructor = Fork i
                      afs' =
                        fmap
                          ( \f ->
                              (\a' b' -> f $ Fork i a' b')
                          )
                          afs
                   in S.modify (& order2 %~ MMap.insert (a, r) afs')

              return ()
         in -- bfs = filter ((== a) . fst . fst) order2
            -- cfs = filter ((== a) . snd . fst) $ order2
            -- newOrder1s = case i of
            --   (Inp _ _) -> Map.delete a order1
            --   (Mul ln a (Constant 0)) -> Map.delete a order1
            undefined
      go (i@(Inp _ _) : _) = Terminal i
      go ((Mul ln a (Constant 0)) : _) = Terminal (Lod ln a (Constant 0)) -- sets register to zero, has no dependencies
      go (i : is) =
        let a = _a i
            b = _b i
            dropIndependents r = dropWhile ((/= r) . _a) is
            definitelyGo r =
              let dependency = maybeGo . dropIndependents $ r
               in case dependency of
                    Just l -> l
                    -- Hack: negative line numbers to shoe-horn in explicit load zero instructions
                    Nothing -> Terminal (Lod (-1) a (Constant 0))
         in case b of
              Constant _ -> Linear i $ definitelyGo a
              Reg r -> Fork i (definitelyGo a) (definitelyGo r)
      maybeGo [] = Nothing
      maybeGo xs = Just . go $ xs
   in go . reverse $ instructions

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
  let simplifyM :: ProgramGraph -> S.State (Map ProgramGraph ProgramGraph) ProgramGraph
      simplifyM t@(Terminal _) = return t
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
  traverse_ print . linearise . simplify . graphify $ parsed
