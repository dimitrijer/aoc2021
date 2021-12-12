module Day03 where

import Debug.Trace
import Data.Bits (complement)
import Data.Char (digitToInt)
import Data.List (transpose, foldl')

-- Part One --------------------------------------------------------------------

readGamma :: [String] -> Int
readGamma = foldl' (\acc b -> acc * 2 + mcb b) 0

readEps :: [String] -> Int
readEps = foldl' (\acc b -> acc * 2 + (1 - mcb b)) 0

mcb :: String -> Int
mcb bs = if ones > zeroes
    then 1
    else 0
  where
    ones = sum . map digitToInt $ bs
    zeroes = length bs - ones

partOne :: String -> Int
partOne input = gamma * epsilon
  where
    bitMap = transpose . lines $ input
    gamma = readGamma bitMap
    epsilon = readEps bitMap

-- Part Two --------------------------------------------------------------------

partTwo :: String -> Maybe Int
partTwo = const $ Just 1
