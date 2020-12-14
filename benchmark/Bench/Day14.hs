module Bench.Day14 where

import Bench.BenchDay
import Criterion.Main
import Days.Day14

benchDay14 :: IO Benchmark
benchDay14 = benchDay "Day14" inputParser partA partB
