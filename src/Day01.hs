module Day01 where

import Data.List.Split (chunksOf, divvy)
import Debug.Trace (trace)
import Text.Read (readMaybe)

-- Part One --------------------------------------------------------------------

type Depth = Int

parseDepths :: String -> Maybe [Depth]
parseDepths = mapM (readMaybe :: String -> Maybe Depth) . lines

partOne :: String -> Maybe Int
partOne input = do
  depths <- parseDepths input
  let pairs = divvy 2 1 depths
   in return (length . filter (\(x0 : x1 : _) -> x1 > x0) $ pairs)

-- Part Two --------------------------------------------------------------------

partTwo :: String -> Maybe Int
partTwo input = do
  depths <- parseDepths input
  let pairs = divvy 2 1 . map sum . divvy 3 1 $ depths
   in return $ length . filter (\(x0 : x1 : _) -> x1 > x0) $ pairs
