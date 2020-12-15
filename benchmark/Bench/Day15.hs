module Bench.Day15 where

import Bench.BenchDay
import Criterion.Main
import Days.Day15

benchDay15 :: IO Benchmark
benchDay15 = benchDay "Day15" inputParser partA partB
