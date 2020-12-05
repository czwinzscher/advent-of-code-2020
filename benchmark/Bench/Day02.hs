module Bench.Day02 where

import Bench.BenchDay
import Criterion.Main
import Days.Day02

benchDay02 :: IO Benchmark
benchDay02 = benchDay "Day02" inputParser partA partB
