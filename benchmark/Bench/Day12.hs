module Bench.Day12 where

import Bench.BenchDay
import Criterion.Main
import Days.Day12

benchDay12 :: IO Benchmark
benchDay12 = benchDay "Day12" inputParser partA partB
