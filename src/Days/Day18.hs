module Days.Day18 where

import Control.Applicative
import Data.Attoparsec.Text
import Data.List (foldl')
import qualified Program.RunDay as R (runDay)

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

exprParser :: Parser Expr
exprParser =
  ( ((string "+ " *> valueParser) >>= \v -> return $ Add v)
      <|> ((string "* " *> valueParser) >>= \v -> return $ Mul v)
  )
    <* skipWhile (== ' ')

parenParser :: Parser [Expr]
parenParser = do
  _ <- char '('
  expr <- fullExprParser
  _ <- char ')'
  return expr

valueParser :: Parser Val
valueParser =
  (decimal >>= \n -> return $ V n)
    <|> (parenParser >>= \e -> return $ P e)

initialParser :: Parser Expr
initialParser = valueParser >>= \val -> return $ Add val

fullExprParser :: Parser [Expr]
fullExprParser = do
  i <- initialParser
  _ <- char ' '
  rest <- many' exprParser
  return (i : rest)

inputParser :: Parser Input
inputParser = do
  fullExprParser `sepBy` endOfLine

data Val = V Int | P [Expr]
  deriving (Show)

data Expr = Add Val | Mul Val
  deriving (Show)

evalExpr :: Int -> Expr -> Int
evalExpr n (Mul v) = n * evalVal v
evalExpr n (Add v) = n + evalVal v

evalVal :: Val -> Int
evalVal (V n) = n
evalVal (P e) = eval e

eval :: [Expr] -> Int
eval = foldl' evalExpr 0

evalExprWithAddPrecedence :: ([Int], Int) -> Expr -> ([Int], Int)
evalExprWithAddPrecedence (xs, x) (Mul v) = (x : xs, evalValWithAddPrecedence v)
evalExprWithAddPrecedence (xs, x) (Add v) = (xs, x + evalValWithAddPrecedence v)

evalValWithAddPrecedence :: Val -> Int
evalValWithAddPrecedence (V n) = n
evalValWithAddPrecedence (P e) = evalWithAddPrecedence e

evalWithAddPrecedence :: [Expr] -> Int
evalWithAddPrecedence i =
  let (xs, x) = foldl' evalExprWithAddPrecedence ([], 0) i
   in product (x : xs)

type Equation = [Expr]

type Input = [Equation]

type OutputA = Int

type OutputB = Int

partA :: Input -> OutputA
partA i = sum $ eval <$> i

partB :: Input -> OutputB
partB i = sum $ evalWithAddPrecedence <$> i
