module Days.Day07 where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Functor
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Program.RunDay as R (runDay)

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  let contentParser = do
        n <- decimal
        _ <- char ' '
        c1 <- takeTill (' ' ==)
        _ <- char ' '
        c2 <- takeTill (' ' ==)
        _ <- string " bags" <|> string " bag"
        return (c1 <> " " <> c2, n)
  let lineParser = do
        c1 <- takeTill (' ' ==)
        _ <- char ' '
        c2 <- takeTill (' ' ==)
        _ <- string " bags contain "
        content <-
          (contentParser `sepBy1` string ", ")
            <|> (string "no other bags" $> [])
        _ <- char '.'
        return (c1 <> " " <> c2, content)

  lineParser `sepBy` endOfLine <&> Map.fromList

------------ TYPES ------------
type Input = Map.Map T.Text [(T.Text, Int)]

------------ PART A ------------
shinyGoldBagName :: T.Text
shinyGoldBagName = "shiny gold"

canHoldShinyGoldBag :: [(T.Text, Int)] -> Input -> Bool
canHoldShinyGoldBag n i =
  any
    (\(t, _) -> t == shinyGoldBagName || canHoldShinyGoldBag (i Map.! t) i)
    n

partA :: Input -> Int
partA i = Map.size $ Map.filter (\v -> canHoldShinyGoldBag v i) i

------------ PART B ------------
numBags :: [(T.Text, Int)] -> Input -> Int
numBags l i = foldr (\(t, n) b -> b + n + n * numBags (i Map.! t) i) 0 l

partB :: Input -> Int
partB i = numBags (i Map.! shinyGoldBagName) i
