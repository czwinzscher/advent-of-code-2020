module Days.Day16 where

import Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Program.RunDay as R (runDay)

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  let rulesLineParser = do
        name <- takeTill (== ':')
        _ <- string ": "
        x1 <- decimal
        _ <- char '-'
        y1 <- decimal
        _ <- string " or "
        x2 <- decimal
        _ <- char '-'
        y2 <- decimal
        return (name, (x1, y1), (x2, y2))
  let rulesParser = rulesLineParser `sepBy` endOfLine
  let myTicketParser = string "your ticket:\n" *> (decimal `sepBy` char ',')
  let nearbyTicketsParser =
        string "nearby tickets:\n" *> ((decimal `sepBy` char ',') `sepBy` endOfLine)
  rules <- rulesParser
  _ <- count 2 endOfLine
  myTicket <- myTicketParser
  _ <- count 2 endOfLine
  nearbyTickets <- nearbyTicketsParser
  return (rules, myTicket, nearbyTickets)

------------ TYPES ------------
type Rules = [(T.Text, (Int, Int), (Int, Int))]

type Input = (Rules, [Int], [[Int]])

------------ PART A ------------
isValidTicket :: Rules -> Int -> Bool
isValidTicket rules n =
  any
    ( \(_, (x1, y1), (x2, y2)) ->
        (n >= x1 && n <= y1)
          || (n >= x2 && n <= y2)
    )
    rules

partA :: Input -> Int
partA (rules, _, t) =
  sum $
    filter
      ( not . isValidTicket rules
      )
      (concat t)

------------ PART B ------------
partB :: Input -> Int
partB (rules, myTicket, nearby) =
  let validTickets = filter (all (isValidTicket rules)) nearby
   in undefined
