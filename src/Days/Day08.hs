module Days.Day08 where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Functor
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Program.RunDay as R (runDay)

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  let lineParser = do
        instruction <-
          (string "acc" $> Acc)
            <|> (string "jmp" $> Jmp)
            <|> (string "nop" $> Nop)
        _ <- char ' '
        n <- signed decimal
        return (instruction, n)

  l <- lineParser `sepBy` endOfLine
  return $ Map.fromList (zip [0 ..] l)

------------ TYPES ------------
data Instruction = Acc | Jmp | Nop
  deriving (Show)

type Input = Map.Map Int (Instruction, Int)

------------ PART A ------------
partA :: Input -> Int
partA input = go 0 0 Set.empty
  where
    go !i !n !s
      | Set.member i s = n
      | otherwise =
        let newSet = Set.insert i s
         in case input Map.! i of
              (Acc, a) -> go (i + 1) (n + a) newSet
              (Jmp, j) -> go (i + j) n newSet
              (Nop, _) -> go (i + 1) n newSet

------------ PART B ------------
accOnTermination :: Input -> Maybe Int
accOnTermination input = go 0 0 Set.empty
  where
    inputSize = Map.size input
    go !i !n !s
      | i == inputSize = Just n
      | Set.member i s = Nothing
      | otherwise =
        let newSet = Set.insert i s
         in case Map.lookup i input of
            Nothing -> Nothing
            Just (Acc, a) -> go (i + 1) (n + a) newSet
            Just (Jmp, j) -> go (i + j) n newSet
            Just (Nop, _) -> go (i + 1) n newSet

partB :: Input -> Int
partB input = go 0
  where
    go i = case input Map.! i of
      (Acc, _) -> go (i + 1)
      (Jmp, j) -> case accOnTermination (Map.insert i (Nop, j) input) of
        Just x -> x
        Nothing -> go (i + 1)
      (Nop, n) -> case accOnTermination (Map.insert i (Jmp, n) input) of
        Just x -> x
        Nothing -> go (i + 1)
