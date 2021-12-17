{-# LANGUAGE TypeApplications #-}

module Lib
  ( increases,
    slidingCo,
  )
where

import Control.Applicative
import Control.Comonad
import qualified Data.List.NonEmpty as NE
import Data.Monoid (Sum)

parseInput :: (Read a) => String -> [a]
parseInput = (fmap read) . lines

sliding2 :: (a -> a -> b) -> [a] -> [b]
sliding2 f as =
  let zippedBs = f <$> (ZipList as) <*> (ZipList (drop 1 as))
   in getZipList zippedBs

sliding3 :: (a -> a -> a -> b) -> [a] -> [b]
sliding3 f as =
  let zippedBs = f <$> (ZipList as) <*> (ZipList (drop 1 as)) <*> (ZipList (drop 2 as))
   in getZipList zippedBs

slidingCo :: Int -> NE.NonEmpty a -> (ZipList [a])
slidingCo n as =
  let nTails = (NE.take n) . duplicate $ as
   in traverse (ZipList . NE.toList) nTails

-- sliding2Co :: (a -> a -> b) -> [a] -> [b]
-- sliding2Co _ [] = []
-- sliding2Co f as =
--   let zippedBs = f <$> (ZipList as) <*> (ZipList (drop 1 as))
--    in getZipList zippedBs

countIncreases :: (Ord a) => [a] -> Int
countIncreases = length . (filter id) . (sliding2 (<))

sumWindows3 :: (Semigroup a) => [a] -> [a]
sumWindows3 as = sliding3 (\a1 a2 a3 -> a1 <> a2 <> a3) as

increases :: IO ()
increases = do
  input <- getContents
  let xs = (parseInput @Int) input
  -- let c = countIncreases . sumWindows3 $ (fmap Sum xs)
  let c = countIncreases xs
  print c
