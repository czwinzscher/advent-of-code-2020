module Bench.Day09 where

import Bench.BenchDay
import Criterion.Main
import Days.Day09

benchDay09 :: IO Benchmark
benchDay09 = benchDay "Day09" inputParser partA partB
