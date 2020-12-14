module Days.Day13 where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Functor
import Data.List
import Data.Maybe
import qualified Program.RunDay as R (runDay)

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  earliest <- decimal
  endOfLine
  timestamps <- ((T <$> decimal) <|> (char 'x' $> X)) `sepBy` char ','
  return (earliest, timestamps)

------------ TYPES ------------
data BusID = T Int | X deriving (Show)

type Input = (Int, [BusID])

------------ PART A ------------
partA :: Input -> Int
partA (t, ids) =
  let nums =
        ids >>= \case
          T n -> [n]
          _ -> []
      vals =
        fmap
          (\n -> (n, ceiling ((fromIntegral t / fromIntegral n) :: Double) * n))
          nums
      (i, time) =
        foldr1 (\x@(_, ts) x'@(_, ts') -> if ts' < ts then x' else x) vals
   in i * (time - t)

------------ PART B ------------

-- extended euclidean algorithm
-- a*x + b*y = gcd(a, b)
egcd :: Int -> Int -> (Int, Int, Int)
egcd a b = aux a b 1 0 0 1
  where
    aux r 0 x y _ _ = (r, x, y)
    aux r r' x y x' y' = aux r' r'' x' y' x'' y''
      where
        r'' = r `rem` r'
        q = r `div` r'
        x'' = x - q * x'
        y'' = y - q * y'

chineseRemainder :: [(Int, Int)] -> Int
chineseRemainder i = undefined

partB :: Input -> Int
partB (_, ids) =
  let ids' = zip [0 ..] ids
   in fromJust $
        find
          ( \n ->
              all
                ( \(i, x) -> case x of
                    T t -> (n + i) `mod` t == 0
                    _ -> True
                )
                ids'
          )
          [1 ..]
