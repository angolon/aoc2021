{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Day19 where

import Control.Arrow (arr, (&&&))
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
import Data.Set (Set, member, union, (\\))
import qualified Data.Set as Set
import qualified Data.Vector as V
import Lib (MyParser, parseInt, parseStdin)
import Linear.Metric (dot)
import qualified Linear.Quaternion as Q
import Linear.V3 (V3 (..), _x, _y, _z)
import Text.Parsec
import Text.Parsec.Char
import Text.Show.Functions

data Scanner = Scanner {_identifier :: Int, _beacons :: (V.Vector (V3 Int))} deriving (Eq, Show)

makeLenses ''Scanner

xAxis :: (Floating a) => V3 a
xAxis = V3 1 0 0

yAxis :: (Floating a) => V3 a
yAxis = V3 0 1 0

zAxis :: (Floating a) => V3 a
zAxis = V3 0 0 1

magnitude :: (Floating a) => V3 a -> a
magnitude (V3 x y z) = sqrt (x ** 2 + y ** 2 + z ** 2)

angleBetween :: (Floating a) => V3 a -> V3 a -> a
angleBetween v1 v2 =
  let m1 = magnitude v1
      m2 = magnitude v2
      denom = m1 * m2
      numer = v1 `dot` v2
   in acos (numer / denom)

yawAngle normal =
  let xyProjection = normal & _z .~ 0
      yawAngle =
        if (sum xyProjection) /= 0
          then angleBetween xAxis xyProjection
          else 0.0 -- no yaw needed
          -- Offset the angle by 2π if the rotation should be more than π radians
   in if (normal ^. _y) <= 0 then yawAngle else (2 * pi) - yawAngle

yawAxis = zAxis

rollAngleAxis normal yawAngle =
  let yawQ = Q.axisAngle yawAxis yawAngle
      yawedX = Q.rotate yawQ xAxis
      rollAngle = angleBetween yawedX normal
      -- Offset again by 2π
      rollβ = if (normal ^. _z) <= 0 then rollAngle else (2 * pi) - rollAngle
      rollAxis = Q.rotate yawQ yAxis
   in (rollβ, rollAxis)

planeAlign planeNormal =
  let yawα = yawAngle planeNormal
      (rollβ, rollAxis) = rollAngleAxis planeNormal yawα
      yawQ = Q.axisAngle yawAxis yawα
      rollQ = Q.axisAngle rollAxis rollβ
   in Q.rotate rollQ . Q.rotate yawQ

invertPlaneAlign normal =
  let yawα = yawAngle normal
      (rollβ, rollAxis) = rollAngleAxis normal yawα
      invertRollQ = Q.axisAngle rollAxis (- rollβ)
      invertYawQ = Q.axisAngle yawAxis (- yawα)
   in Q.rotate invertYawQ . Q.rotate invertRollQ

orientations = [0, pi / 2, pi, pi * (3 / 2)]

planes :: [V3 Int]
planes =
  let zero = V3 0 0 0
      posNeg l = [zero & l .~ (-1), zero & l .~ 1]
   in posNeg _x ++ posNeg _y ++ posNeg _z

data FacingOrientation = FacingOrientation {_normal :: V3 Int, _α :: Double}
  deriving (Eq, Show)

_reorient :: FacingOrientation -> V3 Int -> V3 Int
_reorient (FacingOrientation normal α) =
  let dNormal :: V3 Double
      dNormal = fmap fromIntegral normal
      q = Q.axisAngle dNormal α
   in fmap round . Q.rotate q . planeAlign dNormal . fmap fromIntegral

reorient :: Getter FacingOrientation (V3 Int -> V3 Int)
reorient = to _reorient

invert :: FacingOrientation -> V3 Int -> V3 Int
invert (FacingOrientation normal α) =
  let dNormal :: V3 Double
      dNormal = fmap fromIntegral normal
      q = Q.axisAngle dNormal (- α)
   in fmap round . invertPlaneAlign dNormal . Q.rotate q . fmap fromIntegral

allFacingOrientations = FacingOrientation <$> planes <*> orientations

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

-- Finds a set of vectors which have the most common/modal distance between each other
-- and returns the orientation and distance that can be used to translate the `freeScanner`
-- vectors to the same frame of reference as the `referenceFrame` scanner.
-- (so long as the number of common points exceeds some threshold (12 by default)
threshold = 12

data RelativeScanners = RelativeScanners
  { _from :: Scanner,
    _to :: Scanner,
    _distance :: V3 Int,
    _orientation :: FacingOrientation
  }
  deriving (Eq, Show)

makeLenses ''RelativeScanners

areRelative :: Scanner -> Scanner -> Maybe RelativeScanners
areRelative fixed@(Scanner _ referenceFrame) free@(Scanner _ freeBeacons) =
  let mostCommonForFO facingOrientation =
        let reoriented = fmap (facingOrientation ^. reorient) freeBeacons
            best@(dist, common) = mostCommonDistance referenceFrame reoriented
            len = V.length common
         in if len >= threshold
              then Just (facingOrientation, dist, len)
              else Nothing
      allBest = fmap mostCommonForFO allFacingOrientations
   in do
        (orientation, dist, _) <- maximumByOf (folded . _Just) (compare `on` (view _3)) allBest
        return $ RelativeScanners fixed free dist orientation

justOr (Just a) _ = Just a
justOr _ b = b

-- Semigroup on values associates to the right/appends to the left as we descend the graph,
-- i.e. a3 <> (a2 <> a1)
-- Done so because we're only using this for endomorphism composition a3 . (a2 . a1)
recurseLookup :: forall a k. (Semigroup a, Ord k) => MonoidalMap k [(k, a)] -> k -> k -> Maybe a
recurseLookup m targetKey k1 =
  let go :: (Maybe [(k, a)]) -> Maybe a -> Set k -> Maybe a
      go Nothing _ _ = Nothing
      go (Just []) _ _ = Nothing
      go (Just kas) priorA visited =
        let terminalKA = List.find ((== targetKey) . fst) kas
            composed = (fmap snd terminalKA) <> priorA
            down (k, a) = go (MMap.lookup k m) (Just a <> priorA) (Set.insert k visited)
            unvisitedKAs = filter (not . (`member` visited) . fst) kas
            recurse = mapMaybe down unvisitedKAs
         in if isJust terminalKA then composed else listToMaybe recurse
   in go (MMap.lookup k1 m) Nothing Set.empty

findAllRelative :: [Scanner] -> MonoidalMap Int [RelativeScanners]
findAllRelative scanners =
  let combinations = do
        l@(Scanner lid _) <- scanners
        r@(Scanner rid _) <- scanners
        guard $ lid > rid
        relative <- maybeToList $ areRelative l r
        return $ MMap.singleton lid [relative]
   in fold combinations

findAllCommon scanners =
  let combinations = do
        relations <- MMap.elems $ findAllRelative scanners
        (RelativeScanners from to dist orientation) <- relations
        let lid = from ^. identifier
        let rid = to ^. identifier
        let orientL = Endo $ invert orientation . subtract dist
        let orientR = Endo $ (+ dist) . _reorient orientation
        let lm = MMap.singleton lid [(rid, orientL)]
        let rm = MMap.singleton rid [(lid, orientR)]
        return $ lm <> rm
      lookup = fold combinations
      referenceFrame = (head scanners)
      reorientate (Scanner id beacons) =
        let fixer =
              recurseLookup
                lookup
                (referenceFrame ^. identifier)
                id
            jFixer =
              if (isJust fixer)
                then fromJust fixer
                else error "missing link in chain"
            fixed = appEndo jFixer <$> beacons
         in Set.fromList . V.toList $ fixed
      reorientedTails = foldMap reorientate $ tail scanners
      referenceSet =
        Set.fromList . V.toList $ (referenceFrame ^. beacons)
   in do
        MMap.traverseWithKey
          ( \k a ->
              let as = fmap fst a
               in print (k, as)
          )
          lookup
        return $ referenceSet <> reorientedTails

findLargestDistance :: [Scanner] -> IO (Maybe Int)
findLargestDistance scanners@(reference : rest) =
  let combinations = do
        relations <- MMap.elems $ findAllRelative scanners
        (RelativeScanners from to dist orientation) <- relations
        let lid = from ^. identifier
        let rid = to ^. identifier
        let orientL = Endo $ invert orientation . subtract dist
        let orientR = Endo $ (+ dist) . _reorient orientation
        let lm = MMap.singleton lid [(rid, orientL)]
        let rm = MMap.singleton rid [(lid, orientR)]
        return $ lm <> rm
      lookup = fold combinations
      referenceFrame = (head scanners)
      origin = V3 0 0 0
      reorientateOrigin (Scanner id beacons) =
        let fixer =
              recurseLookup
                lookup
                (referenceFrame ^. identifier)
                id
            jFixer =
              if (isJust fixer)
                then fromJust fixer
                else error "missing link in chain"
         in appEndo jFixer origin
      reorientedTails = fmap reorientateOrigin $ tail scanners
      scannerCoords = origin : reorientedTails
      manhattanMagnitude = getSum . foldMap (Sum . abs)
      distances = do
        v1 <- scannerCoords
        v2 <- scannerCoords
        guard $ v1 > v2
        let dist = v1 - v2
        return $ manhattanMagnitude dist
   in do
        MMap.traverseWithKey
          ( \k a ->
              let as = fmap fst a
               in print (k, as)
          )
          lookup
        return $ maximumOf traversed distances

-- lookup = fold combinations
-- distances = relations ^.. (traversed . traversed . distance)
-- manhattanDistances = distances & mapped %~ getSum . foldMap (Sum . abs)
-- in maximumOf traversed manhattanDistances

orientateScanners :: IO ()
orientateScanners = do
  (Right parsed) <- parseStdin parseScanners
  -- let blah = findCommonBeacons (head parsed) (parsed !! 1)
  -- print blah
  -- let blah = findAllCommon $ take 2 parsed
  -- blah <- findAllCommon parsed
  -- traverse_ print blah
  -- print $ Set.size blah
  d <- findLargestDistance parsed
  print d
  return ()
