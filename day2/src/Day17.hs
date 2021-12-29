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

inBound :: (Int, Int) -> (Int, Int) -> ProbeState -> Bool
inBound (minX, maxX) (minY, maxY) probe =
  let px = probe ^. (position . positionV . x)
      py = probe ^. (position . positionV . y)
   in minX <= px && px <= maxX
        && minY <= py
        && py <= maxY

stepsUntilTarget :: (Int, Int) -> (Int, Int) -> ProbeState -> [ProbeState]
stepsUntilTarget targetX@(minX, maxX) targetY@(minY, maxY) init =
  takeWhile (not . pastBound targetX targetY) (steps init)

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

allValidTrajectories :: (Int, Int) -> (Int, Int) -> [Velocity]
allValidTrajectories xBounds@(_, maxX) yBounds@(minY, _) =
  let (Velocity (Vector minXV maxYV)) = calculateOptimalTrajectory xBounds yBounds
      minYV = minY
      maxXV = maxX
      initialVs = (,) <$> [minXV .. maxXV] <*> [minYV .. maxYV]
      initialProbes = fmap initialProbeVelocity initialVs
      paths = fmap (stepsUntilTarget xBounds yBounds) initialProbes
      pathInBound = inBound xBounds yBounds
      validPaths = filter (pathInBound . last) paths
   in fmap (_velocity . head) validPaths

trickShot :: (Int, Int) -> (Int, Int) -> Maybe Int
trickShot targetX targetY =
  let optimal = calculateOptimalTrajectory targetX targetY
      init = ProbeState (Position (Vector 0 0)) optimal
      ps = stepsUntilTarget targetX targetY init
   in maximumOf (traverse . position . positionV . y) ps

launchProbe :: IO ()
launchProbe =
  let -- targetX = (20, 30)
      -- targetY = (-10, -5)
      targetX = (207, 263)
      targetY = (-115, -63)
      trajectories = allValidTrajectories targetX targetY
   in print $ length trajectories
