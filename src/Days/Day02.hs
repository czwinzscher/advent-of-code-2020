module Days.Day02 (runDay) where

import Data.Attoparsec.Text as AP
import Data.Bits (xor)
import qualified Data.Text as T
import qualified Program.RunDay as R (runDay)

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  let lineParser = do
        mi <- decimal
        _ <- char '-'
        ma <- decimal
        _ <- space
        ch <- anyChar
        _ <- string ": "
        pw <- AP.takeWhile (not . isEndOfLine)
        return (ch, mi, ma, pw)

  lineParser `sepBy` endOfLine

------------ TYPES ------------
type Input = [(Char, Int, Int, T.Text)]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA i =
  length $
    filter
      ( \(c, mi, ma, pw) ->
          let oc = T.length $ T.filter (== c) pw in oc >= mi && oc <= ma
      )
      i

------------ PART B ------------
partB :: Input -> OutputB
partB i =
  length $
    filter
      ( \(c, mi, ma, pw) ->
          let f = T.index pw (mi - 1)
              s = T.index pw (ma - 1)
           in (f == c) `xor` (s == c)
      )
      i
