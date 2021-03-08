module Bench.Day18 where

import Bench.BenchDay
import Criterion.Main
import Days.Day18

benchDay18 :: IO Benchmark
benchDay18 = benchDay "Day18" inputParser partA partB
