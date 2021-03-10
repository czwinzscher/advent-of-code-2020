module Days.Day19 where

import Control.Applicative
import Data.Attoparsec.Text
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Program.RunDay as R (runDay)

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

ruleParser :: Parser Rule
ruleParser = do
  n <- decimal
  _ <- string ": "
  let charParser = char '"' *> (anyChar >>= \c -> return $ C c) <* char '"'
  let rParser = do
        rls <- (decimal `sepBy` char ' ') `sepBy` string " | "
        return $ R rls
  r <- charParser <|> rParser
  return (n, r)

inputParser :: Parser Input
inputParser = do
  rls <- ruleParser `sepBy` endOfLine
  _ <- count 2 endOfLine
  received <- takeWhile1 (not . isEndOfLine) `sepBy` endOfLine
  return (Map.fromList rls, received)

data RuleContent = C Char | R [[Int]]
  deriving (Show)

type Rule = (Int, RuleContent)

type Rules = Map.Map Int RuleContent

type Received = [T.Text]

type Input = (Rules, Received)

type OutputA = Int

type OutputB = Int

matchesRule :: Rules -> RuleContent -> T.Text -> Maybe T.Text
matchesRule _ (C c) i = T.stripPrefix (T.singleton c) i
matchesRule r (R rls) i = anyRuleMatches r rls i

anyRuleMatches :: Rules -> [[Int]] -> T.Text -> Maybe T.Text
anyRuleMatches _ [] _ = Nothing
anyRuleMatches rls (x : xs) i = case go x i of
  Just t -> Just t
  Nothing -> anyRuleMatches rls xs i
  where
    go :: [Int] -> T.Text -> Maybe T.Text
    go [] t = Just t
    go (y : ys) t = matchesRule rls (rls Map.! y) t >>= go ys

matchesRules :: T.Text -> Rules -> Bool
matchesRules input rules =
  let initialRule = rules Map.! 0
   in case matchesRule rules initialRule input of
        Just "" -> True
        _ -> False

partA :: Input -> OutputA
partA (rules, received) = length $ filter (`matchesRules` rules) received

partB :: Input -> OutputB
partB (rules, received) =
  let replaced =
        Map.insert 8 (R [[42], [42, 8]]) $
          Map.insert 11 (R [[42, 31], [42, 11, 31]]) rules
   in error "Not implemented yet!"
