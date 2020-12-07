module Bench.Day07 where

import Bench.BenchDay
import Criterion.Main
import Days.Day07

benchDay07 :: IO Benchmark
benchDay07 = benchDay "Day07" inputParser partA partB
