module Main where

import Bench.Day01
import Bench.Day02
import Bench.Day03
import Bench.Day04
import Bench.Day05
import Bench.Day06
import Bench.Day07
import Bench.Day08
import Bench.Day09
import Bench.Day10
import Bench.Day12
import Bench.Day13
import Bench.Day14
import Bench.Day15
import Bench.Day16
import Criterion.Main

main :: IO ()
main = do
  d1 <- benchDay01
  d2 <- benchDay02
  d3 <- benchDay03
  d4 <- benchDay04
  d5 <- benchDay05
  d6 <- benchDay06
  d7 <- benchDay07
  d8 <- benchDay08
  d9 <- benchDay09
  d10 <- benchDay10
  d12 <- benchDay12
  d13 <- benchDay13
  d14 <- benchDay14
  d15 <- benchDay15
  d16 <- benchDay16
  defaultMain [d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d12, d13, d14, d15, d16]
