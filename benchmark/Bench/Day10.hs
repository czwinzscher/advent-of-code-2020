module Bench.Day10 where

import Bench.BenchDay
import Criterion.Main
import Days.Day10

benchDay10 :: IO Benchmark
benchDay10 = benchDay "Day10" inputParser partA partB
