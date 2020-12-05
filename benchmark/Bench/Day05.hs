module Bench.Day05 where

import Bench.BenchDay
import Criterion.Main
import Days.Day05

benchDay05 :: IO Benchmark
benchDay05 = benchDay "Day05" inputParser partA partB
