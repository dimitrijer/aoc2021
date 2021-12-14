module Main where

import Day01
import Day02
import Day03
import System.Environment
import Text.Printf

runDay :: String -> Int -> IO ()
runDay input day = do
  case day of
    1 -> do print $ Day01.partOne input; print $ Day01.partTwo input
    2 -> do print $ Day02.partOne input; print $ Day02.partTwo input
    3 -> do print $ Day03.partOne input; print $ Day03.partTwo input
    _ -> do putStrLn $ "Unknown day: " ++ show day

main :: IO ()
main = do
  args <- getArgs
  if length args == 1
    then do
      let day = read $ head args
      input <- readFile $ printf "resources/day%02d.txt" day
      runDay input day
    else putStrLn "Please input day number."
