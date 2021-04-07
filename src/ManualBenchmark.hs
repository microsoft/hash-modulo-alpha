{-# LANGUAGE DeriveTraversable #-}

module ManualBenchmark where

import qualified Benchmark
import Data.Function (on)
import Data.Hashable (hash)
import Data.List (groupBy,sortBy)
import Data.Ord (comparing)

import qualified Data.Foldable

data Expressions a = Expressions
  { eMnistCNN :: a
  , eGMM      :: a
  , eBERTs    :: [a]
  }
  deriving (Functor, Foldable, Traversable)

expressions :: Expressions String
expressions = Expressions
  { eMnistCNN = "mnistcnn"
  , eGMM      = "gmm"
  , eBERTs    = map (\i -> "bert/bert" ++ show i) [12 :: Int]
  }

expressionGenerators :: Expressions Benchmark.ExpressionGenerator
expressionGenerators = flip fmap expressions $ \expressionName ->
  Benchmark.ExpressionGenerator
    { Benchmark.bcGenExpr = Benchmark.iteratorPure $ do
        -- Pre-hash the variables, since our benchmark functions
        -- expect Int variables.  This doesn't priviledge any
        -- algorithm over any other.
        let filename = testcase_path expressionName
        expr <- Benchmark.readExpr filename
        pure (fmap hash expr)
    , Benchmark.bcGenName = expressionName
    }

testcase_path :: String -> FilePath
testcase_path = \name -> "./exprs/" ++ name ++ ".expr"

process_stats :: Benchmark.AggregateStatistics -> (Int, Int)
process_stats aggregate_stats =
  let (_, mean, _, _, stddev) = Benchmark.stats aggregate_stats in (round mean, round stddev)

specific_benchmarks :: Benchmark.BenchmarkParams -> IO ()
specific_benchmarks bps = do
      results <- Benchmark.benchmarkResults (Data.Foldable.toList expressionGenerators) bps

      putStrLn ("Outputs are algorithm name: [(size, time in seconds)]")

      let grouped :: [[(String, String, (Int, Double))]]
          grouped = groupBy ((==) `on` fst3) $ sortBy (comparing fst3) $ flip concatMap results $ \(results', genName) ->
            flip concatMap results' $ \((algorithmName, _, _), [results'']) ->
              [(algorithmName, genName, results'')]

      flip mapM_ grouped $ \algorithmName_results' -> do
        let (algorithmName, _, _):_ = algorithmName_results'

        putStrLn ("Algorithm " ++ algorithmName ++ ":")
        flip mapM_ algorithmName_results' $ \(_, genName, results'') -> do
          let (_, minSecs) = results''
              minMicros = minSecs * 1000 * 1000

          putStrLn (genName ++ ": " ++ show (round minMicros :: Int))
        putStrLn ""
  where fst3 (x, _, _) = x
