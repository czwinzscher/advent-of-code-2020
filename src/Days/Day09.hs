module Days.Day09 where

import Data.Attoparsec.Text
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import qualified Program.RunDay as R (runDay)

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  preamble <- count 25 (decimal <* endOfLine)
  rest <- decimal `sepBy` endOfLine
  return (preamble, rest)

------------ TYPES ------------
type Input = ([Int], [Int])

------------ PART A ------------
isSum :: Int -> [Int] -> Bool
isSum x xs = any (\n -> any (\m -> m /= n && x == n + m) xs) xs

partA :: Input -> Int
partA (pr@(_ : ps), (x : xs)) = if isSum x pr then partA (ps ++ [x], xs) else x

------------ PART B ------------
partB :: Input -> Int
partB i@(x, y) =
  let num = partA i
      allNumbers = x <> y
      subSeqs = concatMap inits . tails $ allNumbers
      ns = sort $ fromJust $ find (\l -> sum l == num) subSeqs
   in head ns + last ns
