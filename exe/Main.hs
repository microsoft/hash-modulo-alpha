{-# LANGUAGE LambdaCase #-}

import qualified Collision
import qualified Benchmark
import qualified ManualBenchmark
import qualified Hash

import System.Environment (getArgs)

main :: IO ()
main = do
  getArgs >>= \case
    ["manual", "fast"] -> ManualBenchmark.specific_benchmarks Benchmark.fast
    ["manual", "full"] -> ManualBenchmark.specific_benchmarks Benchmark.full
    ["random", "fast"] -> Benchmark.benchmark Benchmark.fast
    ["random", "full"] -> Benchmark.benchmark Benchmark.full
    ["test"] -> Hash.testEverythingInFileStartingWith'prop_'
    ["collisions", "fast"] -> Collision.collisions 1
    ["collisions", "full"] -> Collision.collisions 10
    _ -> putStrLn "Unsupported argument"
