{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Day19 where

import Control.Lens
import Control.Monad
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
import Data.Maybe
import Data.Monoid (Endo (..), Sum (..), getSum)
import Data.PQueue.Min (MinQueue)
import qualified Data.PQueue.Min as MinQueue
import Data.Ratio
import Data.Set (Set, union, (\\))
import qualified Data.Set as Set
import Data.Vector ((!), (!?))
import qualified Data.Vector as V
import Lib (MyParser, parseStdin)
import Linear.Metric (dot)
import qualified Linear.Quaternion as Q
import Linear.V3 (V3 (..), _x, _y, _z)
import Text.Parsec
import Text.Parsec.Char

data Scanner = Scanner {_identifier :: Int, _beacons :: (V.Vector (V3 Int))} deriving (Eq, Show)

makeLenses ''Scanner

xAxis :: (Floating a) => V3 a
xAxis = V3 1 0 0

yAxis :: (Floating a) => V3 a
yAxis = V3 0 1 0

zAxis :: (Floating a) => V3 a
zAxis = V3 0 0 1

-- Code from my clojure keyboard project that I'm translating/misappropriating:
-- (defn angle-between [v1 v2]
--   (let [mag-v1 (v/magnitude v1)
--         mag-v2 (v/magnitude v2)
--         denom  (* mag-v1 mag-v2)
--         numer  (v/dot v1 v2)]
--     (->> (/ numer denom)
--          Math/acos
--          )))
magnitude :: (Floating a) => V3 a -> a
magnitude (V3 x y z) = sqrt (x ** 2 + y ** 2 + z ** 2)

angleBetween :: (Floating a) => V3 a -> V3 a -> a
angleBetween v1 v2 =
  let m1 = magnitude v1
      m2 = magnitude v2
      denom = m1 * m2
      numer = v1 `dot` v2
   in acos (numer / denom)

-- (defn plane-align [plane-normal shape]
--   (let [x-y-projection (v/vector (get plane-normal 0) (get plane-normal 1) 0)
--         yaw-angle (angle-between x-axis x-y-projection)
--         ; offset the angle by 2π if the rotation should be more than π radians
--         ; (90 degrees)
--         yaw-α (if (<= 0 (get plane-normal 1)) yaw-angle (- (* 2 π) yaw-angle))
--         yaw-q (q/from-angle-axis yaw-α z-axis)
--         yawed-x-axis (q/rotate yaw-q x-axis)
--         roll-angle (angle-between yawed-x-axis plane-normal)
--         ; offset the angle by 2π if the rotation should be more than π radians
--         ; (90 degrees)
--         roll-β (if (<= 0 (get plane-normal 2)) roll-angle (- (* 2 π) roll-angle))
--         yawed-y-axis (q/rotate yaw-q (v/vector 0 -1 0))]
--     (->> shape
--          (rotate yaw-α z-axis)
--          (rotate roll-β yawed-y-axis)
--          )))
--

planeAlign planeNormal v =
  let xyProjection = planeNormal & _z .~ 0
      yawAngle =
        if (sum xyProjection) /= 0
          then angleBetween xAxis xyProjection
          else 0.0 -- no yaw needed
          -- Offset the angle by 2π if the rotation should be more than π radians
      yawα = if (planeNormal ^. _y) <= 0 then yawAngle else (2 * pi) - yawAngle
      yawQ = Q.axisAngle zAxis yawα
      yawedXAxis = Q.rotate yawQ xAxis
      rollAngle = angleBetween yawedXAxis planeNormal
      -- Offset again by 2π
      rollβ = if (planeNormal ^. _z) <= 0 then rollAngle else (2 * pi) - rollAngle
      yawedYAxis = Q.rotate yawQ yAxis
      rollQ = Q.axisAngle yawedYAxis rollβ
   in Q.rotate rollQ . Q.rotate yawQ $ v

orientations = [0, pi / 2, pi, pi * (3 / 2)]

planes :: [V3 Int]
planes =
  let zero = V3 0 0 0
      posNeg l = [zero & l .~ (-1), zero & l .~ 1]
   in posNeg _x ++ posNeg _y ++ posNeg _z

data FacingOrientation = FacingOrientation {_normal :: V3 Int, _α :: Double}

_reorient :: FacingOrientation -> V3 Int -> V3 Int
_reorient (FacingOrientation normal α) =
  let dNormal :: V3 Double
      dNormal = fmap fromIntegral normal
      q = Q.axisAngle dNormal α
   in fmap round . Q.rotate q . planeAlign dNormal . fmap fromIntegral

reorient :: Getter FacingOrientation (V3 Int -> V3 Int)
reorient = to _reorient

allFacingOrientations = FacingOrientation <$> planes <*> orientations

parseInt :: MyParser Int
parseInt =
  let negate = option "" (string "-")
      digits = (++) <$> negate <*> many1 digit
   in (read @Int) <$> digits

parseV3 :: MyParser (V3 Int)
parseV3 = V3 <$> parseInt <* char ',' <*> parseInt <* char ',' <*> parseInt

parseScanners :: MyParser [Scanner]
parseScanners =
  let beacons = V.fromList <$> sepEndBy1 parseV3 endOfLine
      scanner = Scanner <$ string "--- scanner " <*> parseInt <* string " ---" <* endOfLine <*> beacons
   in sepEndBy1 scanner endOfLine

bestest = maximumBy (compare `on` (V.length . snd))

-- Finds a set of vectors which have the most common/modal distance between each other
mostCommonDistance :: V.Vector (V3 Int) -> V.Vector (V3 Int) -> (V3 Int, V.Vector (V3 Int, V3 Int))
mostCommonDistance vs us =
  let distanceUnit v u = MMap.singleton (v - u) $ V.singleton (v, u)
      combinations = fold (distanceUnit <$> vs <*> us)
   in bestest $ MMap.toList combinations

findCommonBeacons :: Scanner -> Scanner -> (V3 Int, V.Vector (V3 Int, V3 Int))
findCommonBeacons (Scanner _ referenceFrame) (Scanner _ freeScanner) =
  -- I've spent a lot of time worrying about having to reconstruct the relative
  -- orientation up to this point, but I'm giving up for the moment because it's
  -- just slowing me down :'(
  let mostCommonForFO facingOrientation =
        let reoriented = fmap (facingOrientation ^. reorient) freeScanner
         in mostCommonDistance referenceFrame reoriented
   in bestest . fmap mostCommonForFO $ allFacingOrientations

orientateScanners :: IO ()
orientateScanners = do
  (Right parsed) <- parseStdin parseScanners
  let ds = findCommonBeacons (head parsed) (parsed !! 1)
  -- _ <- MMap.traverseWithKey (\k v -> print (k, v)) ds
  print . fst $ ds
  traverse_ print $ snd ds
  return ()
