module Day03 where

import Debug.Trace
import Data.Bits (complement)
import Data.Char (digitToInt)
import Data.List (transpose, foldl')

-- Part One --------------------------------------------------------------------

readGamma :: Bitmap -> Int
readGamma = foldl' (\acc b -> acc * 2 + mcb 0 b) 0

readEps :: Bitmap -> Int
readEps = foldl' (\acc b -> acc * 2 + (1 - mcb 0 b)) 0

mcb :: Int -> String -> Int
mcb tie b = case compare ones zeroes of
               GT -> 1
               LT -> 0
               EQ -> tie
  where
    ones = sum . map digitToInt $ b
    zeroes = length b - ones

partOne :: String -> Int
partOne input = gamma * epsilon
  where
    b = transpose . lines $ input
    gamma = readGamma b
    epsilon = readEps b

-- Part Two --------------------------------------------------------------------

type Bitmap = [String]

mcbN :: Bitmap -> Int -> Int
mcbN b i = mcb 1 . (!! i) . transpose $ b

readBin :: String -> Int
readBin = foldl' (\acc b -> acc * 2 + digitToInt b) 0

readOxy :: Bitmap -> Int -> Int
readOxy [x] _ = readBin x
readOxy b i = readOxy b' (i + 1)
  where b' = filter (\x -> digitToInt (x !! i) == mcbi) b
        mcbi = mcbN b i

readCO2 :: Bitmap -> Int -> Int
readCO2 [x] _ = readBin x
readCO2 b i = readCO2 b' (i + 1)
  where b' = filter (\x -> digitToInt (x !! i) == lcbi) b
        lcbi = 1 - mcbN b i

partTwo :: String -> Int
partTwo input = oxy * co2
  where b = lines input
        oxy = readOxy b 0
        co2 = readCO2 b 0
