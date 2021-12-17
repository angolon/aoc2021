{-# LANGUAGE TypeApplications #-}

module Lib
  ( increases,
  )
where

import Control.Applicative

readInput :: IO String
readInput = getContents

parseInput :: (Read a) => String -> [a]
parseInput = (fmap read) . lines

sliding2 :: (a -> a -> b) -> [a] -> [b]
sliding2 f as =
  let zippedBs = f <$> (ZipList as) <*> (ZipList (drop 1 as))
   in getZipList zippedBs

countIncreases = length . (filter id) . (sliding2 (<))

increases :: IO ()
increases = do
  input <- getContents
  let xs = (parseInput @Int) input
  let c = countIncreases xs
  print c
