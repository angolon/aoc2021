{-# LANGUAGE TypeApplications #-}

module Lib
  ( increases,
  )
where

readInput :: IO String
readInput = getContents

parseInput :: (Read a) => String -> [a]
parseInput = (fmap read) . lines

thing :: (Ord a) => [a] -> [Bool]
thing xs = (zipWith (>)) (tail xs) xs

countIncreases = length . (filter id)

-- parseInput = readList . linesk

increases :: IO ()
increases = do
  input <- getContents
  let xs = (parseInput @Int) input
  let c = countIncreases . thing $ xs
  print c
