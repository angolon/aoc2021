{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Day17 where

import Control.Applicative
import Control.Arrow (arr, (&&&))
import Control.Lens
import Control.Monad
import Data.Bifunctor.Swap (swap)
import Data.Either (either)
import Data.Foldable
import Data.Function (on)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MMap
import Data.Maybe (isNothing, listToMaybe, mapMaybe, maybeToList)
import Data.Monoid (Sum (..), getSum)
import Data.PQueue.Min (MinQueue)
import qualified Data.PQueue.Min as MinQueue
import Data.Ratio
import Data.Set (Set, union, (\\))
import qualified Data.Set as Set
import Data.Vector ((!), (!?))
import qualified Data.Vector as V
import GHC.Generics
import Lib (MyParser, parseInt, parseStdin)
import Text.Parsec
import Text.Parsec.Char

data Vector = Vector {_x :: Int, _y :: Int} deriving (Eq, Show)

newtype Velocity = Velocity {_velocityV :: Vector} deriving (Eq, Show)

newtype Position = Position {_positionV :: Vector} deriving (Eq, Show)

data ProbeState = ProbeState {_position :: Position, _velocity :: Velocity} deriving (Eq, Show)

makeLenses ''Vector
makeLenses ''Velocity
makeLenses ''Position
makeLenses ''ProbeState

stepVelocity :: Velocity -> Velocity
stepVelocity v =
  let stepX x = x + (negate . signum $ x)
      stepY y = y - 1
   in v & (velocityV . x) %~ stepX & (velocityV . y) %~ stepY

stepPosition :: Velocity -> Position -> Position
stepPosition (Velocity (Vector xx yy)) p =
  p & (positionV . x) +~ xx & (positionV . y) +~ yy

stepProbe :: ProbeState -> ProbeState
stepProbe probe =
  let vel = probe ^. velocity
   in probe & position %~ (stepPosition vel) & velocity %~ stepVelocity

steps :: ProbeState -> [ProbeState]
steps init = iterate stepProbe init

pastBound :: (Int, Int) -> (Int, Int) -> ProbeState -> Bool
pastBound (minX, maxX) (minY, maxY) probe =
  let xx = probe ^. (position . positionV . x)
      yy = probe ^. (position . positionV . y)
   in xx > maxX || yy < minY

stepsUntilTarget :: (Int, Int) -> (Int, Int) -> ProbeState -> [ProbeState]
stepsUntilTarget targetX@(minX, maxX) targetY@(minY, maxY) init =
  let px probe = probe ^. (position . positionV . x)
      py probe = probe ^. (position . positionV . y)
      inBound probe =
        let x = px probe
            y = py probe
         in minX <= x && x <= maxX
              && minY <= y
              && y <= maxY
      -- path = take 10 (steps init)
      path = takeWhile (not . pastBound targetX targetY) (steps init)
      onTarget = inBound . last $ path
   in path

-- in if onTarget then Just path else Nothing

initialProbeVelocity :: (Int, Int) -> ProbeState
initialProbeVelocity v =
  ProbeState (Position (Vector 0 0)) (Velocity (uncurry Vector $ v))

-- Assumes that X is positive...
clampXVelocity :: (Int, Int) -> Maybe Int
clampXVelocity (minX, maxX) =
  let findV :: Int -> Either Int Int -- Left is an overshot, Right is right on target
      findV x =
        let xs = scanl (-) x [1 ..]
            terminal = head . dropWhile (> 0) $ xs
            firstStep = take 2 . reverse . takeWhile (>= terminal) $ xs
         in case firstStep of
              0 : a : [] -> Right a
              a : b : [] -> Left (b - a)
              _ -> error $ "something went horribly wrong: " ++ (show firstStep)
      lowerVE = findV minX
      lowerV = either id id lowerVE
      upperVE = findV maxX
      upperV = either (\x -> x - 1) id upperVE
      potentialVs = [lowerV .. upperV]
   in listToMaybe potentialVs

calculateOptimalTrajectory :: (Int, Int) -> (Int, Int) -> Velocity
calculateOptimalTrajectory xBounds@(minX, maxX) yBounds@(minY, maxY) =
  let calcvy 0 = error "potentially infinite solution"
      calcvy a
        | a < 0 = (abs a) - 1
        | a > 0 = a
      vy = calcvy minY
      intermediateSteps = stepsUntilTarget xBounds yBounds $ initialProbeVelocity (0, vy)
      -- probably should handle the case where you need to move horizontally quicker
      timeToKill = length intermediateSteps
      vx = case clampXVelocity xBounds of
        Just v -> v
        otherwise -> error "couldn't solve x velocity"
   in if vx > timeToKill then error "blargh" else Velocity (Vector vx vy)

launchProbe :: IO ()
launchProbe =
  let targetX = (207, 263)
      targetY = (-115, -63)
      optimal = calculateOptimalTrajectory targetX targetY
      highestY =
        let init = ProbeState (Position (Vector 0 0)) optimal
            ps = stepsUntilTarget targetX targetY init
         in maximumOf (traverse . position . positionV . y) ps
   in print highestY
