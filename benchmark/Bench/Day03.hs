module Bench.Day03 where

import Bench.BenchDay
import Criterion.Main
import Days.Day03

benchDay03 :: IO Benchmark
benchDay03 = benchDay "Day03" inputParser partA partB
