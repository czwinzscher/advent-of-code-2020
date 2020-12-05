module Bench.BenchDay where

import Criterion.Main
import Data.Attoparsec.Text
import qualified Data.Text.IO as T

benchDay ::
  String ->
  Parser a ->
  (a -> b) ->
  (a -> c) ->
  IO Benchmark
benchDay name parserFunc partAFunc partBFunc = do
  input <- T.readFile ("input/" <> name <> ".txt")
  case parseOnly parserFunc input of
    Left e -> error e
    Right parsed ->
      return $
        bgroup
          name
          [ bench "Parser" $ whnf (parseOnly parserFunc) input,
            bench "A" $ whnf partAFunc parsed,
            bench "B" $ whnf partBFunc parsed
          ]
