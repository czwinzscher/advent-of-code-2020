module Days.Day10 where

import Data.Attoparsec.Text
import Data.List
import qualified Program.RunDay as R (runDay)

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = decimal `sepBy` endOfLine

------------ TYPES ------------
type Input = [Int]

------------ PART A ------------
partA :: Input -> Int
partA i = go (0, 0) (0 : sort i)
  where
    go (!a, !b) (x : y : ys)
      | (y - x) == 1 = go (a + 1, b) (y : ys)
      | (y - x) == 3 = go (a, b + 1) (y : ys)
      | otherwise = go (a, b) (y : ys)
    go (!a, !b) (_ : _) = a * (b + 1)

------------ PART B ------------
partB :: Input -> Int
partB = error "Not implemented yet!"
