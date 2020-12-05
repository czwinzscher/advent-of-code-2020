module Days.Day03 where

import Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Program.RunDay as R (runDay)

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = takeWhile1 (not . isEndOfLine) `sepBy` endOfLine

------------ TYPES ------------
type Input = [T.Text]

------------ PART A ------------
isTree :: Char -> Bool
isTree = (==) '#'

numTrees :: Input -> Int -> Int -> Int
numTrees [] _ _ = 0
numTrees i right down = go 0 0 0
  where
    go :: Int -> Int -> Int -> Int
    go n x y =
      let nextIndex l = (x + right) `mod` T.length l
          nextLineIndex = y + down
          newCount l = if isTree (T.index l (nextIndex l)) then n + 1 else n
       in if nextLineIndex < length i
            then
              let nextLine = (i !! nextLineIndex)
               in go (newCount nextLine) (nextIndex nextLine) nextLineIndex
            else n

partA :: Input -> Int
partA i = numTrees i 3 1

------------ PART B ------------
partB :: Input -> Int
partB i =
  numTrees i 1 1 * numTrees i 3 1 * numTrees i 5 1 * numTrees i 7 1 * numTrees i 1 2
