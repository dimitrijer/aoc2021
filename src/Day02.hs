module Day02 where

import Text.Read (readMaybe)
import Debug.Trace

-- Part One --------------------------------------------------------------------

data Command = Forward Int | Down Int | Up Int
type Pos = (HPos, VPos)

parseCmd :: String -> Maybe Command
parseCmd input = do
    delta <- (readMaybe :: String -> Maybe Int) deltaStr
    case dir of
        "forward" -> Just (Forward delta)
        "down" -> Just (Down delta)
        "up" -> Just (Up delta)
        _ -> Nothing
    where (dir : deltaStr : _) = words input

applyCmd :: Command -> Pos -> Pos
applyCmd (Forward d) (x, y) = (x + d, y)
applyCmd (Down d) (x, y) = (x, y + d)
applyCmd (Up d) (x, y) = (x, y - d)

partOne :: String -> Maybe Int
partOne input = do
    cmds <- parseCmds input
    let pos = foldr applyCmd (0, 0) cmds
        in return $ uncurry (*) pos

-- Part Two --------------------------------------------------------------------

type HPos = Int
type VPos = Int
type Aim = Int
type Pos' = (HPos, VPos, Aim)

applyCmd' :: Command -> Pos' -> Pos'
applyCmd' (Forward d) (x, y, a) = (x + d, y + d * a, a)
applyCmd' (Down d) (x, y, a) = (x, y, a + d)
applyCmd' (Up d) (x, y, a) = (x, y, a - d)

-- | The `parseCmds` parses commands and reverses the list, as `foldr` reduces
-- the list from right to left.
parseCmds :: String -> Maybe [Command]
parseCmds = fmap reverse . mapM parseCmd . lines

partTwo :: String -> Maybe Int
partTwo = fmap ((\(x, y, _) -> x * y) . foldr applyCmd' (0, 0, 0)) . parseCmds
