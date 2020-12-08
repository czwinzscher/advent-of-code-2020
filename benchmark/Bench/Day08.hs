module Bench.Day08 where

import Bench.BenchDay
import Criterion.Main
import Days.Day08

benchDay08 :: IO Benchmark
benchDay08 = benchDay "Day08" inputParser partA partB
