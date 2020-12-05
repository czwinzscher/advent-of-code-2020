module Days.Day05 where

import Data.Attoparsec.Text
import Data.Foldable (find)
import Data.Maybe (fromJust)
import qualified Data.Set as S
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
seatID :: (Int, Int) -> Int
seatID (row, col) = row * 8 + col

getSeat :: T.Text -> (Int, Int)
getSeat i =
  let rowPart = T.take 7 i
      colPart = T.drop 7 i
      (r, _) =
        T.foldl'
          ( \(l, u) -> \case
              'F' -> (l, u - ceiling (fromIntegral (u - l) / 2 :: Double))
              'B' -> (l + ceiling (fromIntegral (u - l) / 2 :: Double), u)
              x -> error [x]
          )
          ((0, 127) :: (Int, Int))
          rowPart
      (c, _) =
        T.foldl'
          ( \(l, u) -> \case
              'R' -> (l + ceiling (fromIntegral (u - l) / 2 :: Double), u)
              'L' -> (l, u - ceiling (fromIntegral (u - l) / 2 :: Double))
              x -> error [x]
          )
          ((0, 7) :: (Int, Int))
          colPart
   in (r, c)

partA :: Input -> Int
partA i = maximum $ seatID . getSeat <$> i

------------ PART B ------------
partB :: Input -> Int
partB i =
  let seats = S.fromList $ getSeat <$> i
      seatIDs = S.map seatID seats
      possibleSeats = [(x, y) | x <- [0 .. 127] :: [Int], y <- [0 .. 7] :: [Int]]
   in seatID $
        fromJust $
          find
            ( \s ->
                let sid = seatID s
                 in not (S.member s seats)
                      && (sid + 1) `S.member` seatIDs
                      && (sid - 1) `S.member` seatIDs
            )
            possibleSeats
