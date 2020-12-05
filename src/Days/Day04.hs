module Days.Day04 where

import Data.Attoparsec.Text
import Data.Char
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Program.RunDay as R (runDay)

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ TYPES ------------
type Input = [Map.Map T.Text T.Text]

------------ PARSER ------------
pair :: Parser (T.Text, T.Text)
pair = do
  k <- takeTill (\c -> c == ':' || isEndOfLine c)
  _ <- char ':'
  v <- takeTill (\c -> c == ' ' || isEndOfLine c)
  return (k, v)

passportLine :: Parser [(T.Text, T.Text)]
passportLine = pair `sepBy1` char ' '

passport :: Parser (Map.Map T.Text T.Text)
passport = do
  passportLines <- passportLine `sepBy` endOfLine
  return $ Map.fromList (concat passportLines)

inputParser :: Parser Input
inputParser = passport `sepBy` count 2 endOfLine

heightParser :: Parser (Int, T.Text)
heightParser = do
  n <- decimal
  t <- takeText
  return (n, t)

hairColorParser :: Parser T.Text
hairColorParser = char '#' >> takeText

------------ PART A ------------
requiredFields :: [T.Text]
requiredFields = ["ecl", "pid", "eyr", "hcl", "byr", "iyr", "hgt"]

partA :: Input -> Int
partA i = length $ filter (\m -> all (\k -> k `Map.member` m) requiredFields) i

------------ PART B ------------
validate :: T.Text -> T.Text -> Bool
validate "ecl" v = v `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
validate "pid" v = T.length v == 9 && T.all isDigit v
validate "eyr" v =
  T.length v == 4 && case parseOnly decimal v of
    Left _ -> False
    Right (a :: Int) -> a >= 2020 && a <= 2030
validate "hcl" v = case parseOnly hairColorParser v of
  Left _ -> False
  Right a -> T.length a == 6 && T.all isHexDigit a
validate "byr" v =
  T.length v == 4 && case parseOnly decimal v of
    Left _ -> False
    Right (a :: Int) -> a >= 1920 && a <= 2002
validate "iyr" v =
  T.length v == 4 && case parseOnly decimal v of
    Left _ -> False
    Right (a :: Int) -> a >= 2010 && a <= 2020
validate "hgt" v = case parseOnly heightParser v of
  Left _ -> False
  Right (n, t) -> case t of
    "cm" -> n >= 150 && n <= 193
    "in" -> n >= 59 && n <= 76
    _ -> False
validate _ _ = True

partB :: Input -> Int
partB i =
  length $
    filter
      (\m -> all (\k -> maybe False (validate k) (Map.lookup k m)) requiredFields)
      i
