module Bench.Day04 where

import Bench.BenchDay
import Criterion.Main
import Days.Day04

benchDay04 :: IO Benchmark
benchDay04 = benchDay "Day04" inputParser partA partB
