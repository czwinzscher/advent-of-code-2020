module Program.RunDay (runDay) where

import Control.Exception (SomeException, catch, try)
import Control.Monad.Except
import Data.Attoparsec.Text (Parser, parseOnly)
import Data.Text (pack)

runDay ::
  (Show a, Show b, Show i) =>
  Parser i ->
  (i -> a) ->
  (i -> b) ->
  Bool ->
  String ->
  IO ()
runDay inputParser partA partB verbose inputFile = do
  input <- runExceptT $ do
    fileContentsEither <- liftIO $ try $ readFile inputFile
    case fileContentsEither of
      Left (e :: SomeException) ->
        throwError $ "error while reading file " <> inputFile <> ": " <> show e
      Right fileContents -> do
        case parseOnly inputParser (pack fileContents) of
          Left e -> throwError $ "Parser failed to read input. Error " <> e
          Right i -> do
            when verbose $ do
              liftIO $ putStrLn "Parser output:"
              liftIO $ print i

            return i
  case input of
    Left x -> putStrLn x
    Right i -> do
      putStrLn "Part A:"
      catch
        (print $ partA i)
        ( \(m :: SomeException) ->
            putStrLn "Couldn't run Part A!"
              >> when verbose (print m)
        )
      putStrLn "Part B:"
      catch
        (print $ partB i)
        ( \(m :: SomeException) ->
            putStrLn "Couldn't run Part B!"
              >> when verbose (print m)
        )
