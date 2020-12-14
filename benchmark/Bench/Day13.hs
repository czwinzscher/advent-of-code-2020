module Bench.Day13 where

import Bench.BenchDay
import Criterion.Main
import Days.Day13

benchDay13 :: IO Benchmark
benchDay13 = benchDay "Day13" inputParser partA partB
