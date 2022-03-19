{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module MultiInterval where

import qualified Data.Foldable as Foldable
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Maybe as Maybe
import Data.Semigroup (Max (..), Min (..))
import Data.Set (Set (..))
import qualified Data.Set as Set
import Numeric.Interval.NonEmpty (Interval (..), width, (<!), (<=!), (<=?), (<?), (>?))
import qualified Numeric.Interval.NonEmpty as Interval

-- import NE ((:|))

data MultiInterval a = MI
  { _intervals :: Set (Interval a)
  }
  deriving (Eq, Show)

singleton :: a -> MultiInterval a
singleton = MI . Set.singleton . Interval.singleton

empty :: MultiInterval a
empty = MI Set.empty

null :: MultiInterval a -> Bool
null = Set.null . _intervals

fromInterval :: Interval a -> MultiInterval a
fromInterval = MI . Set.singleton

(...) :: Ord a => a -> a -> MultiInterval a
(...) a b = MI . Set.singleton $ (Interval....) a b

-- Should be a private helper function.
mergeLast :: Integral a => MultiInterval a -> Interval a -> MultiInterval a
mergeLast (MI as) a
  | Set.null as = fromInterval a
  | otherwise =
    let (lastA, prefixAs) = Set.deleteFindMax as
        lastA' = Interval.hull lastA a
        d = Interval.distance lastA a
     in if d <= 1
          then -- They're touching/overlapping.
            MI $ Set.insert lastA' prefixAs
          else MI $ Set.insert a as

union :: Integral a => MultiInterval a -> MultiInterval a -> MultiInterval a
union (MI as) (MI bs)
  | Set.null as = MI bs
  | Set.null bs = MI as
  | otherwise =
    let cs = as `Set.union` bs
        (seed, cs') = Set.deleteFindMin cs
     in Set.foldl' mergeLast (fromInterval seed) cs'

intersection :: Integral a => MultiInterval a -> MultiInterval a -> MultiInterval a
intersection (MI as) (MI bs)
  | Set.null as = empty
  | Set.null bs = empty
  | otherwise =
    let go Nothing _ accum = accum
        go _ Nothing accum = accum
        go (Just (a, as')) (Just (b, bs')) accum =
          let advanceA = go (Set.minView as') $ Just (b, bs')
              advanceB = go (Just (a, as')) (Set.minView bs')
              advanceAB = go (Set.minView as') (Set.minView bs')
           in case Interval.intersection a b of
                Nothing
                  | a <! b -> advanceA accum
                  | b <! a -> advanceB accum
                Just ab ->
                  let accum' = mergeLast accum ab
                   in if
                          | a == b -> advanceAB accum'
                          | a `Interval.contains` b -> advanceB accum'
                          | b `Interval.contains` a -> advanceA accum'
                          | a < b -> advanceA accum'
                          | b < a -> advanceB accum'
     in go (Set.minView as) (Set.minView bs) empty

diff :: (Integral a) => MultiInterval a -> MultiInterval a -> MultiInterval a
diff (MI as) (MI bs)
  | Set.null as = empty
  | Set.null bs = MI as
  | otherwise =
    let go Nothing _ accum = accum
        go (Just (a, as')) Nothing accum =
          go (Set.minView as') Nothing $ mergeLast accum a
        go (Just (a, as')) (Just (b, bs')) accum =
          let advanceA = go (Set.minView as') $ Just (b, bs')
              advanceB = go (Just (a, as')) (Set.minView bs')
              advanceAB = go (Set.minView as') (Set.minView bs')
              minA = Interval.inf a
              minB = Interval.inf b
              maxA = Interval.sup a
              maxB = Interval.sup b
              ab = minA Interval.... (minB - 1)
              ba = (maxB + 1) Interval.... maxA
              accum' = mergeLast accum ab
           in if
                  | a <! b -> advanceA $ mergeLast accum a
                  | b <! a -> advanceB accum
                  | a < b ->
                    if
                        | maxA <= maxB -> go (Set.minView as') (Just (b, bs')) accum'
                        | maxA > maxB -> go (Just (ba, as')) (Set.minView bs') accum'
                  | maxA > maxB -> go (Just (ba, as')) (Set.minView bs') accum
                  | b `Interval.contains` a -> advanceA accum
     in go (Set.minView as) (Set.minView bs) empty

size :: (Num a) => MultiInterval a -> a
size (MI is) = Set.foldl' (\a b -> a + (width b)) (fromInteger 0) is

-- Unsafe, but I'm hoping my constructors/operators eliminate
-- the possibility of an empty inner set.
lowerBound :: Ord a => MultiInterval a -> a
lowerBound (MI is) = getMin . Maybe.fromJust . foldMap (Just . Min . Interval.inf) $ is

upperBound :: Ord a => MultiInterval a -> a
upperBound (MI is) = getMax . Maybe.fromJust . foldMap (Just . Max . Interval.sup) $ is

contains :: Ord a => a -> MultiInterval a -> Bool
contains a (MI as) =
  let ai = Interval.singleton a
      as' = Set.dropWhileAntitone (<! ai) as
      head = Set.lookupMin as'
      contained = Interval.member a <$> head
   in Maybe.fromMaybe False contained

-- toList :: Bound -> [Int32]
-- toList (Range a b) = [a .. b]
-- toList (Elements es) = Set.toList es
-- toList (Tree bounds) = foldMap toList bounds

-- toSet :: Bound -> Set Int32
-- toSet (Elements es) = es
-- toSet range = Set.fromList . toList $ range
--
instance Integral a => Semigroup (MultiInterval a) where
  (<>) = union

instance Integral a => Monoid (MultiInterval a) where
  mempty = empty

instance Integral a => Num (MultiInterval a) where
  (MI as) + (MI bs) = Foldable.fold . fmap fromInterval $ (+) <$> Set.toList as <*> Set.toList bs

  fromInteger = singleton . fromInteger

  -- I choose not to.
  abs = undefined
  negate = undefined

  signum (MI as) = Foldable.fold . fmap (fromInterval . signum) . Set.toList $ as

  (*) = undefined
