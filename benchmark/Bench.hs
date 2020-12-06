module Main where

import Bench.Day01
import Bench.Day02
import Bench.Day03
import Bench.Day04
import Bench.Day05
import Bench.Day06
import Criterion.Main

main :: IO ()
main = do
  d1 <- benchDay01
  d2 <- benchDay02
  d3 <- benchDay03
  d4 <- benchDay04
  d5 <- benchDay05
  d6 <- benchDay06
  defaultMain [d1, d2, d3, d4, d5, d6]
