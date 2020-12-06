module Days.Day06 where

import Data.Attoparsec.Text
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Program.RunDay as R (runDay)

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
groupParser :: Parser [T.Text]
groupParser = takeWhile1 (not . isEndOfLine) `sepBy` endOfLine

inputParser :: Parser Input
inputParser = groupParser `sepBy` string "\n\n"

------------ TYPES ------------
type Input = [[T.Text]]

------------ PART A ------------
partA :: Input -> Int
partA i =
  sum $
    fmap
      ( ( fst
            . T.foldr
              ( \c (a, s) ->
                  if c `S.member` s
                    then (a, s)
                    else (a + 1, S.insert c s)
              )
              (0, S.empty)
        )
          . T.concat
      )
      i

------------ PART B ------------
partB :: Input -> Int
partB i =
  sum $ S.size . foldr1 S.intersection . fmap (S.fromList . T.unpack) <$> i
