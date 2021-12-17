{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Day04 where

import Data.Array
import Data.List (break, find, sortBy)
import Data.List.Split (chunksOf, splitOn)
import Debug.Trace
import Text.Printf (printf)
import Text.Read (readMaybe)

-- Part One --------------------------------------------------------------------

boardBounds = ((0, 0), (4, 4))

type NumBoard = Array (Int, Int) Int

type HitBoard = Array (Int, Int) Bool

parseNumBoard :: String -> Maybe NumBoard
parseNumBoard input = listArray boardBounds <$> elems
  where
    elems = mapM readMaybe . words $ input

hasBingo :: HitBoard -> Bool
hasBingo hits = isHit rows || isHit cols
  where
    -- Row-major order by default.
    rows = chunksOf 5 . elems $ hits
    -- Sort for column-major order.
    cols = chunksOf 5 . map snd . sortBy (\((i1, j1), _) ((i2, j2), _) -> compare j1 j2) . assocs $ hits
    isHit = any and

data Bingo = Bingo
  { nums :: NumBoard,
    hits :: HitBoard
  }
  deriving (Eq)

instance Show Bingo where
  show b = unlines . map unwords . chunksOf 5 $ zipNumsHits showNumHits b
    where
      showNumHits = \n h -> if h then printf "[%2d]" n else printf " %2d " n

mkBingo :: NumBoard -> Bingo
mkBingo nums =
  Bingo
    { nums,
      hits = listArray (bounds nums) $ repeat False
    }

zipNumsHits :: (Int -> Bool -> a) -> Bingo -> [a]
zipNumsHits f Bingo {nums, hits} = zipWith f (elems nums) (elems hits)

mark :: Int -> Bingo -> Bingo
mark num Bingo {nums, hits} =
  case find (\((i, j), v) -> v == num) $ assocs nums of
    Just ((i, j), _) -> Bingo {nums, hits = hits // [((i, j), True)]}
    Nothing -> Bingo {nums, hits}

score :: Bingo -> Int
score = sum . zipNumsHits (\n h -> if h then 0 else n)

partOne :: String -> Maybe Int
partOne input =
  let (cmdStr : boardsStr) = splitOn "\n\n" input
      update :: [Bingo] -> Int -> [Bingo]
      update b i = map (mark i) b
   in do
        cmds <- mapM (readMaybe :: String -> Maybe Int) . splitOn "," $ cmdStr
        boards <- mapM (fmap mkBingo . parseNumBoard) boardsStr
        let (pfx, sfx) = break (any (hasBingo . hits)) . scanl update boards $ cmds
            lastNum = cmds !! (length pfx - 1)
         in do
              b <- find (hasBingo . hits) . head $ sfx
              return $ lastNum * score b

-- Part One --------------------------------------------------------------------

partTwo :: String -> Maybe Int
partTwo = const $ Just 1
