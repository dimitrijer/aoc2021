module Main where

import System.Environment
import Text.Printf

runDay :: String -> Int -> IO ()
runDay input day = do
  case day of
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
