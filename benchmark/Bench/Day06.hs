module Bench.Day06 where

import Bench.BenchDay
import Criterion.Main
import Days.Day06

benchDay06 :: IO Benchmark
benchDay06 = benchDay "Day06" inputParser partA partB
