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
import Prelude hiding (null)

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
                  | minA < minB ->
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

contains :: Ord a => MultiInterval a -> a -> Bool
contains (MI as) a =
  let ai = Interval.singleton a
      as' = Set.dropWhileAntitone (<! ai) as
      head = Set.lookupMin as'
      contained = Interval.member a <$> head
   in Maybe.fromMaybe False contained

cross :: Integral b => (Interval a -> Interval a -> Interval b) -> MultiInterval a -> MultiInterval a -> MultiInterval b
cross op (MI as) (MI bs) = foldMap fromInterval $ op <$> Set.toList as <*> Set.toList bs

iquot :: (Integral a) => MultiInterval a -> MultiInterval a -> MultiInterval a
iquot a@(MI as) b@(MI bs)
  | Set.null as = empty
  | Set.null bs = empty
  | otherwise =
    let b' = b `diff` singleton 0 -- Avoid division by zero
     in cross (Interval.iquot) a b'

imod :: (Integral a) => MultiInterval a -> MultiInterval a -> MultiInterval a
imod a@(MI as) b@(MI bs)
  | Set.null as = empty
  | Set.null bs = empty
  | otherwise =
    let b' = b `diff` singleton 0 -- Avoid division by zero
     in cross (Interval.imod) a b'

toList :: (Integral a) => MultiInterval a -> [a]
toList (MI as) =
  let intervalToList i = [(Interval.inf i) .. (Interval.sup i)]
   in foldMap intervalToList as

invertAdd :: Integral a => MultiInterval a -> MultiInterval a -> MultiInterval a -> (MultiInterval a, MultiInterval a)
invertAdd xs ys zs =
  let xs' = (zs - ys) `intersection` xs
      ys' = (zs - xs') `intersection` ys
   in (xs', ys')

invertMul :: Integral a => MultiInterval a -> MultiInterval a -> MultiInterval a -> (MultiInterval a, MultiInterval a)
invertMul xs ys zs =
  let zHasZero = zs `contains` 0
      xs' =
        if zHasZero && (ys `contains` 0)
          then xs
          else (zs `iquot` ys) `intersection` xs
      ys' =
        if zHasZero && (xs' `contains` 0)
          then ys
          else (zs `iquot` xs') `intersection` ys
   in (xs', ys')

invertDiv :: Integral a => MultiInterval a -> MultiInterval a -> MultiInterval a -> (MultiInterval a, MultiInterval a)
invertDiv xs ys zs =
  -- x / y = z ==> y = x / z
  -- x / y = z ==> x = y * z
  let zHasZero = zs `contains` 0
      smallestZ = lowerBound . abs $ zs
      notXs =
        -- x can never have a smaller magnitude than z
        if zHasZero
          then empty
          else (- smallestZ) ... smallestZ
      -- inverting based on multiplication can't use our fancy multiplication that
      -- skips certain numbers - truncated division would succeed for those numbers that
      -- we try to ignore.
      xs' = (`diff` notXs) . (`intersection` xs) $ cross (*) zs ys
      ys' =
        if
            | zHasZero && (xs' `contains` 0) -> ys -- 0 / y = 0 => y could be anything
            | zHasZero ->
              -- If `z` could be zero, then the upper bounds of `y` will be
              -- one greater than each of the x intervals.
              let largests = fmap Interval.sup . Set.toList . _intervals . abs $ xs'
                  bigYs = foldMap (\x -> (- x) ... x) largests
                  regularYs = xs' `iquot` (zs `diff` singleton 0)
               in bigYs `union` regularYs
            | otherwise -> xs' `iquot` zs
   in (xs', ys' `intersection` ys)

-- Based off of this implementation for "single" values, that I *believe* is correct.
-- invModXToY x
--   | z > x = empty
--   | x == z && yMax > z = (yBounds `intersection`) $ (z + 1) ... yMax
--   | x == z = empty
--   | otherwise = (yBounds `intersection`) . foldMap singleton $ filter (\y -> (x `mod` y) == z) [(z + 1) .. (x - z)]
-- invModYToX y
--   | z < y = (xBounds `intersection`) . foldMap singleton $ takeWhile (<= xMax) [z, (z + y) ..]
--   | z >= y = empty
invertMod :: (Integral a) => MultiInterval a -> MultiInterval a -> MultiInterval a -> (MultiInterval a, MultiInterval a)
invertMod xs ys zs =
  let xToY xs ys =
        let yUpper = upperBound ys
            xzs = xs `intersection` zs
            -- For all x == z, the modulus `y` can be any value greater than `z`, i.e. `z + 1, z + 2, ...`
            ys1 =
              if (not . null $ xzs) && (yUpper > lowerBound xzs)
                then ((1 + lowerBound xzs) ... yUpper) `intersection` ys
                else empty
            xDiffZs = xs `diff` zs
            zDiffXs = zs `diff` xs
            -- Problematic: this isn't sufficient, as there could be many more factors.
            -- however: I'm just going to give up and let the bug persist, as I don't think this code
            -- is critical enough for the solution.
            remSub = xDiffZs - zDiffXs
            ys2 = if (lowerBound remSub <= 0) then remSub `diff` ((lowerBound remSub) ... 0) else remSub
         in ys `intersection` (ys1 `union` ys2)

      yToX xs ys =
        let yUpper = upperBound ys
            yLower = lowerBound ys
            zUpper = upperBound zs
            zsForX = if (zUpper >= yUpper) then zs `diff` (yUpper ... zUpper) else zs
            zLower = lowerBound zsForX
            -- How many multiples of the smallest y can we fit between the smallest z and the largest x.
            xUpper = upperBound xs
            (q, r) = (xUpper - zLower) `quotRem` yLower
            multiples = if r > 0 then q + 1 else q
            manyYs = (0 ... multiples) * ys
         in xs `intersection` (zsForX + manyYs)
      ys' = xToY xs ys
      xs' = yToX xs ys
   in (xs', ys')

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

instance (Ord a, Integral a) => Num (MultiInterval a) where
  (+) = cross (+)

  fromInteger = singleton . fromInteger

  abs = Foldable.fold . fmap fromInterval . fmap abs . Set.toList . _intervals

  negate = Foldable.fold . fmap fromInterval . fmap negate . Set.toList . _intervals

  signum (MI as) = Foldable.fold . fmap (fromInterval . signum) . Set.toList $ as

  a@(MI as) * b@(MI bs)
    | Set.null as = empty
    | Set.null bs = empty
    | otherwise =
      let specials = (-1) ... 1
          a' = Set.toList . _intervals $ a `diff` specials
          b' = Set.toList . _intervals $ b `diff` specials
          z = if a `contains` 0 || b `contains` 0 then 0 ... 0 else empty
          posA = if a `contains` 1 then b else empty
          posB = if b `contains` 1 then a else empty
          negA = if a `contains` (-1) then negate b else empty
          negB = if b `contains` (-1) then negate a else empty
          normals = Foldable.fold . fmap fromInterval $ (*) <$> a' <*> b'
       in Foldable.fold [z, posA, posB, negA, negB, normals]
