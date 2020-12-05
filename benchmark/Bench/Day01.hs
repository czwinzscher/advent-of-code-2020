module Bench.Day01 where

import Bench.BenchDay
import Criterion.Main
import Days.Day01

benchDay01 :: IO Benchmark
benchDay01 = benchDay "Day01" inputParser partA partB
