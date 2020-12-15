module Days.Day15 where

import Data.Attoparsec.Text hiding (take)
import qualified Data.IntMap.Strict as IntMap
import qualified Program.RunDay as R (runDay)

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = decimal `sepBy` char ','

------------ TYPES ------------
type Input = [Int]

------------ PART A ------------
initialState :: [Int] -> IntMap.IntMap Int
initialState i = IntMap.fromList $ zip i [1 ..]

nthNumber :: Input -> Int -> Int
nthNumber i n = go (initialState (take (length i - 1) i)) (last i) (length i)
  where
    go m !cur !ind
      | ind == n = cur
      | otherwise = case IntMap.lookup cur m of
        Nothing -> go (IntMap.insert cur ind m) 0 (ind + 1)
        Just v -> let diff = ind - v in go (IntMap.insert cur ind m) diff (ind + 1)

partA :: Input -> Int
partA i = nthNumber i 2020

------------ PART B ------------
partB :: Input -> Int
partB i = nthNumber i 30000000
