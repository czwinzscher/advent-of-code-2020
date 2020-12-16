module Bench.Day16 where

import Bench.BenchDay
import Criterion.Main
import Days.Day16

benchDay16 :: IO Benchmark
benchDay16 = benchDay "Day16" inputParser partA partB
