{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Day04 where

import Data.Array
import Data.List (break, find, findIndex, sortBy)
import Data.List.Split (chunksOf, splitOn)
import Debug.Trace
import Text.Printf (printf)
import Text.Read (readMaybe)

-- Part One --------------------------------------------------------------------

boardBounds = ((0, 0), (4, 4))

newtype Board = Board (Array (Int, Int) (Int, Bool))
  deriving (Eq)

instance Show Board where
  show (Board b) = unlines . map unwords . chunksOf 5 . map showNumHits $ elems b
    where
      showNumHits = \(n, h) -> if h then printf "[%2d]" n else printf " %2d " n

parseBoard :: String -> Maybe Board
parseBoard input = Board . listArray boardBounds <$> fmap (`zip` repeat False) vals
  where
    vals = mapM (readMaybe :: String -> Maybe Int) . words $ input

isBingo :: Board -> Bool
isBingo (Board b) = isHit rows || isHit cols
  where
    -- Row-major order by default.
    rows = chunksOf 5 . map snd . elems $ b
    -- Sort for column-major order.
    cols = chunksOf 5 . map (snd . snd) . sortBy (\((i1, j1), _) ((i2, j2), _) -> compare j1 j2) . assocs $ b
    isHit = any and

mark :: Int -> Board -> Board
mark num (Board b) =
  case find (\((i, j), (v, _)) -> v == num) $ assocs b of
    Just ((i, j), (v, _)) -> Board (b // [((i, j), (v, True))])
    Nothing -> Board b

score :: Board -> Int
score (Board b) = sum . map (\(n, h) -> if h then 0 else n) $ elems b

partOne :: String -> Maybe Int
partOne input = do
  (cmds, boards) <- parseCmdsBoards input
  let (pfx, sfx) = break (any isBingo) $ updateBoards cmds boards
      lastNum = cmds !! (length pfx - 1)
   in do
        b <- find isBingo . head $ sfx
        return $ lastNum * score b

-- Part One --------------------------------------------------------------------

parseCmdsBoards :: String -> Maybe ([Int], [Board])
parseCmdsBoards input =
  let (cmdStr : boardsStr) = splitOn "\n\n" input
   in do
        cmds <- mapM (readMaybe :: String -> Maybe Int) . splitOn "," $ cmdStr
        boards <- mapM parseBoard boardsStr
        return (cmds, boards)

updateBoards :: [Int] -> [Board] -> [[Board]]
updateBoards cmds boards = scanl update boards cmds
  where
    update :: [Board] -> Int -> [Board]
    update b i = map (mark i) b

partTwo :: String -> Maybe Int
partTwo input = do
  (cmds, boards) <- parseCmdsBoards input
  let (pfx, sfx) = break (all isBingo) $ updateBoards cmds boards
      lastNum = cmds !! (length pfx - 1)
   in do
        bi <- findIndex (not . isBingo) . last $ pfx
        return $ lastNum * score (head sfx !! bi)
