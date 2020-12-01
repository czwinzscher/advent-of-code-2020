module Main where

import Bench.Day01
import Criterion.Main

main :: IO ()
main = defaultMain [benchDay01]
