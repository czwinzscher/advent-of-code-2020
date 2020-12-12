module Days.Day12 where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Functor
import Data.List
import qualified Program.RunDay as R (runDay)

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  let lineParser = do
        a <-
          (char 'N' $> N)
            <|> (char 'S' $> S)
            <|> (char 'E' $> E)
            <|> (char 'W' $> W)
            <|> (char 'L' $> L)
            <|> (char 'R' $> R)
            <|> (char 'F' $> F)
        a <$> decimal
  lineParser `sepBy` endOfLine

------------ TYPES ------------
data Action = N Int | S Int | E Int | W Int | L Int | R Int | F Int
  deriving (Show)

data Direction = East | South | West | North deriving (Enum, Bounded, Show)

type Input = [Action]

------------ PART A ------------
manhattanDistance :: (Int, Int) -> Int
manhattanDistance (x, y) = abs x + abs y

nextA :: (Direction, Int, Int) -> Action -> (Direction, Int, Int)
nextA (d, e, s) a = case a of
  N n -> (d, e, s + n)
  S n -> (d, e, s - n)
  E n -> (d, e + n, s)
  W n -> (d, e - n, s)
  L n -> (toEnum ((fromEnum d - (n `div` 90)) `mod` 4), e, s)
  R n -> (toEnum ((fromEnum d + (n `div` 90)) `mod` 4), e, s)
  F n -> case d of
    East -> (d, e + n, s)
    South -> (d, e, s - n)
    West -> (d, e - n, s)
    North -> (d, e, s + n)

partA :: Input -> Int
partA i =
  let (_, x, y) = foldl' nextA (East, 0, 0) i
   in manhattanDistance (x, y)

------------ PART B ------------
nextB :: ((Int, Int), (Int, Int)) -> Action -> ((Int, Int), (Int, Int))
nextB (w@(wx, wy), f@(fx, fy)) a = case a of
  N n -> ((wx, wy + n), f)
  S n -> ((wx, wy - n), f)
  E n -> ((wx + n, wy), f)
  W n -> ((wx - n, wy), f)
  L n -> let w' = foldl' (\(x, y) _ -> (-y, x)) w [1..(n `div` 90)] in (w', f)
  R n -> let w' = foldl' (\(x, y) _ -> (y, -x)) w [1..(n `div` 90)] in (w', f)
  F n -> (w, (fx + wx * n, fy + wy * n))

partB :: Input -> Int
partB i =
  let (_, xy) = foldl' nextB ((10, 1), (0, 0)) i
   in manhattanDistance xy
