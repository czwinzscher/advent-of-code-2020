module Bench.Day17 where

import Bench.BenchDay
import Criterion.Main
import Days.Day17

benchDay17 :: IO Benchmark
benchDay17 = benchDay "Day17" inputParser partA partB
