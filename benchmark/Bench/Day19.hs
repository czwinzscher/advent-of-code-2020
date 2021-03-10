module Bench.Day19 where

import Bench.BenchDay
import Criterion.Main
import Days.Day19

benchDay19 :: IO Benchmark
benchDay19 = benchDay "Day19" inputParser partA partB
