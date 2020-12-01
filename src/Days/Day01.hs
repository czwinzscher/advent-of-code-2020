module Days.Day01 (runDay, partA, partB) where

import Data.Attoparsec.Text
import Data.Functor ((<&>))
import Data.Maybe
import qualified Data.Set as S
import qualified Program.RunDay as R (runDay)

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

inputParser :: Parser (S.Set Int)
inputParser = decimal `sepBy` endOfLine <&> S.fromList

partA :: S.Set Int -> Int
partA i =
  let is = S.toList i
      (x, y) = head [(a, b) | a <- is, let b = 2020 - a, b `S.member` i]
   in x * y

partB :: S.Set Int -> Int
partB i = head $ do
    x <- S.toList i
    let (_, ys) = S.split x i
        b = 2020 - x 
        x' = S.toList ys
    case listToMaybe [(a, b') | a <- x', let b' = b - a, b' `S.member` ys] of
      Nothing -> []
      Just (y, z) -> pure (x * y * z)
