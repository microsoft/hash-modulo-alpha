{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}

module Benchmark where

import qualified Data.Foldable
import Data.Function (fix)
import Data.Hashable (hash, Hashable)
import Data.List (intercalate)
--import qualified GHC.Stats
import qualified System.Clock as Clock
import Text.Read (readMaybe)
import Text.Printf (printf)
import System.IO.Temp (createTempDirectory, emptyTempFile)
import qualified System.Mem

import Expr (Expr, exprSize)
import Hash (deBruijnHash, structuralHashNested)
import qualified Hash
import qualified AlphaHashOptimizedHash
import AlphaHashEfficientHash (Hash)

data BenchmarkParams = BenchmarkParams
  { runsToMinimizeOver :: Int
  , minimumMeasurableTime_secs :: Double
  , maximumTime_micro :: Double
  , sizeScale :: Double
  }

data ExpressionGenerator = ExpressionGenerator
  { bcGenExpr :: Iterator IO (IO (Expr () Int))
  , bcGenName :: String
  }

data Algorithms a = Algorithms
  { aLocallyNameless        :: a
  --, aAlphaHashFromPaper       :: a
  , aAlphaHashFromPaperFaster :: a
  , aDeBrujinHash           :: a
  , aStructuralHashNested   :: a
  }
  deriving (Functor, Foldable, Traversable)

algorithms_ :: (Hashable a, Ord a)
            => Algorithms (String, Expr h a -> Expr Hash a, String)
algorithms_ = Algorithms
  { aLocallyNameless    = ("LocallyNameless", Hash.locallyNameless, baseline)
  --, aAlphaHashFromPaper   = ("Ours as in paper (will not have this one on final version)", AlphaHashFastOrigHash.alphaHash, paper)
  , aAlphaHashFromPaperFaster = ("Ours", AlphaHashOptimizedHash.alphaHash, good)
  , aDeBrujinHash           = ("DeBruijn*", deBruijnHash, prettyBad)
  , aStructuralHashNested   = ("Structural*", structuralHashNested, veryBad)
  }
  where
      veryBad   = "red"
      prettyBad = "orange"
      good      = "web-green"
      baseline  = "web-blue"
      --paper     = "purple"

fast :: BenchmarkParams
fast = BenchmarkParams
  { runsToMinimizeOver = 3
  , minimumMeasurableTime_secs = 0.01
  , maximumTime_micro = 1000
  , sizeScale = 2
  }

full :: BenchmarkParams
full = BenchmarkParams
  { runsToMinimizeOver = 10
  , minimumMeasurableTime_secs = 0.1
  , maximumTime_micro = 1000 * 1000
  , sizeScale = 1.1
  }

expressionSets :: Double -> [ExpressionGenerator]
expressionSets sizeScale_ =
  [ ExpressionGenerator
    { bcGenExpr =
      fmap (flip Hash.genExprWithVarsUnbalancedSize 10) scaleIterator
    , bcGenName = "unbalanced expressions"
    }
  , ExpressionGenerator
    { bcGenExpr =
        fmap (flip Hash.genExprWithVarsSize 10) scaleIterator
    , bcGenName = "balanced expressions"
    }
  , ExpressionGenerator
    { bcGenExpr = flip fmap (iteratorOfList [1..23 :: Int]) $ \i -> do
       let filename = "./exprs/bert/bert" ++ show i ++ ".expr"
       expr <- readExpr filename
       -- Pre-hash the BERT variables, since our benchmark functions
       -- expect Int variables.  This doesn't priviledge any algorithm
       -- over any other.
       pure (fmap hash expr)
    , bcGenName = "BERT"
    }
  ]
  where -- scaleIterator is a stream of intergers, the next one is the
        -- smallest larger than the previous by a multiple of sizeScale_
        scaleIterator :: Applicative m => Iterator m Int
        scaleIterator =
          fix (\next size -> Iterator $ do
                  let !size' = floor (fromIntegral size * sizeScale_) + 1
                  pure (Just (size, next size'))) 1

-- | This is the entry point to the module.  When run it will
-- benchmark the algorithms on a random set of expressions.  The data
-- from the run will be written out to a directory whose name is
-- displayed at the end of the run.
benchmark :: BenchmarkParams -> IO ()
benchmark bps = do
  benchmarksDir <- createTempDirectory "." "benchmarks"
  results_genNames <- benchmarkResults (expressionSets (sizeScale bps)) bps
  flip mapM_ results_genNames $ \(results', genName) -> do
    datasets <- flip mapM results' $ \((algorithmName, _, algorithmColor), results) -> do
        let textOutput = flip concatMap results $ \(size, time) ->
              show size ++ " " ++  show time ++ "\n"

        filename <- emptyTempFile benchmarksDir (algorithmName ++ ".dat")

        writeFile filename textOutput

        return PlotDataset
          { pdFile  = filename
          , pdTitle = algorithmName
          , pdColor = algorithmColor
          , pdStyle = "7"
          , pdSize  = "0.25"
          }

    makeGnuplot benchmarksDir genName datasets

benchmarkResults :: [ExpressionGenerator]
                 -> BenchmarkParams
                 -> IO [([((String, Expr () Int -> Expr Hash Int, String),
                            [(Int, Double)])],
                          String)]
benchmarkResults expressionSets_ bps = do
  let algorithms = Data.Foldable.toList algorithms_

  flip mapM (enumFrom1 expressionSets_) $ \(i, bc) -> do
    let expressionSet = (show i ++ "/" ++ show (length expressionSets_)
                          ++ " (" ++ bcGenName bc ++ ")")
    results <- benchmarkNoPlot bps expressionSet algorithms bc
    pure (results, bcGenName bc)

benchmarkNoPlot :: BenchmarkParams
                -> String
                -> [(String, Expr () Int -> Expr h a, string)]
                -> ExpressionGenerator
                -> IO [((String, Expr () Int -> Expr h a, string), [(Int, Double)])]
benchmarkNoPlot bps
                expressionSet
                algorithms bc = do
  let allParams = algorithms

  flip mapM (enumFrom1 allParams) $ \(i, algorithm_) -> do
    let (algorithmName, algorithm, _) = algorithm_
    results <- loop (bcGenExpr bc) [] $ \genExpression rest -> do
      -- We force the expression after generating it.  The Expr type
      -- is strict, that is forcing it forces everything it contains,
      -- therefore no time is wasted forcing it in the hashing
      -- algorithm itself.  On the other hand adding this bang pattern
      -- made absolutely no difference to the benchmarks.  Presumably
      -- the expression is generated already in forced state.  But
      -- it's nice to keep this here for clarity.
      putStrLn ("Expression set " ++ expressionSet)
      putStrLn ("Parameter set "
                 ++ show i ++ "/" ++ show (length allParams)
                 ++ " (" ++ algorithmName ++ ")")
      putStrLn ("Generated " ++ show (length rest) ++ " expressions")
      putStrLn ("Generating expression ...")
      !expression <- genExpression
      let !exprSize' = exprSize expression
      putStrLn ("done.  Size was " ++ show exprSize')

      let minimumMeasureableTime_micro = minimumMeasurableTime_secs bps * 1000 * 1000

      (repeats, firstStats) <- benchmarkUntil minimumMeasureableTime_micro
                                              1
                                              (seqHashResult . algorithm)
                                              expression

      r <- benchmarkMore firstStats
                         (runsToMinimizeOver bps - 1)
                         repeats
                         (seqHashResult . algorithm)
                         expression

      let (n, mean_micro, tmin_micro, _, stddev_micro) = stats r
          showFloat = printf "%.0f" :: Double -> String

      putStrLn ("Count: "    ++ show n)
      putStrLn ("Mean: "     ++ showFloat mean_micro ++ "us")
      putStrLn ("Min: "      ++ showFloat tmin_micro ++ "us")
      putStrLn ("Std dev: "  ++ showFloat stddev_micro ++ "us")

      let done = tmin_micro > maximumTime_micro bps
          tmin_secs = tmin_micro / (1000 * 1000)
          rest' = (exprSize', tmin_secs):rest

      pure $ (if done then Right else Left) rest'

    pure (algorithm_, results)

makeGnuplot :: FilePath -> String -> [PlotDataset] -> IO ()
makeGnuplot benchmarksDir xlabel results = do
  gnuplotFilename <- emptyTempFile benchmarksDir "benchmarks.gnuplot"
  gnuplotPdfFilename <- emptyTempFile benchmarksDir "benchmarks-pdf.gnuplot"

  let gnuplotFileContent = gnuplotFile xlabel results
      (outputPdf, gnuplotPdfFileContent) = gnuplotFilePdf benchmarksDir xlabel results

  writeFile gnuplotFilename gnuplotFileContent
  writeFile gnuplotPdfFilename gnuplotPdfFileContent

  putStrLn ("I put stuff in " ++ benchmarksDir ++ ".")
  putStrLn "If you have an X server and you want a live graph view run:"
  putStrLn ("gnuplot --persist " ++ gnuplotFilename)
  putStrLn "If you want to generate a PDF run:"
  putStrLn ("gnuplot " ++ gnuplotPdfFilename)
  putStrLn ("You will find the output PDF in " ++ outputPdf)

type AggregateStatistics = (Int, Double, Double, Double)

stats :: AggregateStatistics -> (Int, Double, Double, Double, Double)
stats (n, tsum, tsquaredsum, tmin) = (n, mean, tmin, variance, stddev)
  where n' = fromIntegral n
        mean     = tsum / n'
        variance = tsquaredsum / n' - mean * mean
        stddev   = sqrt variance

-- This is probably the entry point you want to use to benchmark an
-- algorithm on a list of expressions each read from a FilePath.
--
-- Runs algorithm on expression and produces aggregate timing
-- statistics.
--
-- benchmarkOne will seq the result of `algorithm expression`.  It is
-- the caller's responsibility to ensure that this causes *all*
-- desired work to be performed.  If you're not sure on this point
-- please ask the author.
benchmarkOne :: Int
             -> Integer
             -> (e -> r)
             -> e
             -> IO AggregateStatistics
benchmarkOne = benchmarkMore (0, 0, 0, infinity)
  where infinity = 1e60

benchmarkMore :: AggregateStatistics
              -> Int
              -> Integer
              -> (e -> r)
              -> e
              -> IO AggregateStatistics
benchmarkMore already samplesPerExpression iterationsPerSample algorithm expression =
  times samplesPerExpression already $ \(n, !t, !tsquared, !minSoFar) -> do
        System.Mem.performMajorGC
        start <- Clock.getTime Clock.Monotonic
        times iterationsPerSample () $ \() ->
          evaluate algorithm expression
        stop <- Clock.getTime Clock.Monotonic

        let elapsed_micro = iterationsElapsed_micro / fromIntegral iterationsPerSample
              where iterationsElapsed = Clock.diffTimeSpec stop start
                    iterationsElapsed_nano = Clock.toNanoSecs iterationsElapsed
                    iterationsElapsed_micro = fromIntegral iterationsElapsed_nano / 1e3

        return (n + 1,
                t + elapsed_micro,
                tsquared + elapsed_micro * elapsed_micro,
                min minSoFar elapsed_micro)

benchmarkUntil :: Double
               -> Integer
               -> (e -> r)
               -> e
               -> IO (Integer, AggregateStatistics)
benchmarkUntil minimumMeasurableTime_micro repeats f x = do
  System.Mem.performMajorGC
  --gcStart_nano <- fmap GHC.Stats.gc_elapsed_ns GHC.Stats.getRTSStats
  start <- Clock.getTime Clock.Monotonic
  times repeats () $ \() ->
    evaluate f x
  stop <- Clock.getTime Clock.Monotonic
  --gcStop_nano <- fmap GHC.Stats.gc_elapsed_ns GHC.Stats.getRTSStats

  let iterationsElapsed_micro = fromIntegral iterationsElapsed_nano / 1e3
        where iterationsElapsed = Clock.diffTimeSpec stop start
              iterationsElapsed_nano = Clock.toNanoSecs iterationsElapsed

      elapsed_micro = iterationsElapsed_micro / fromIntegral repeats

{-
      gcElapsed_micro = fromIntegral (gcStop_nano - gcStart_nano) / 1000

      showFloat = printf "%.0f" :: Double -> String

  putStrLn ("Elapsed: " ++ showFloat elapsed_micro ++ "us")
  putStrLn ("GC Elapsed: " ++ showFloat gcElapsed_micro ++ "us")
  putStrLn ("Productivity: "
            ++ showFloat ((1 - (gcElapsed_micro / elapsed_micro)) * 100)
            ++ "%")
-}

  if iterationsElapsed_micro < minimumMeasurableTime_micro
  then benchmarkUntil minimumMeasurableTime_micro (2 * repeats) f x
  else pure (repeats,
             (1, elapsed_micro, elapsed_micro * elapsed_micro, elapsed_micro))

readExpr :: FilePath -> IO (Expr () String)
readExpr = readExprG

readExprG :: Read e => FilePath -> IO e
readExprG filepath = do
  filecontents <- readFile filepath

  case readMaybe filecontents of
    Nothing   -> error ("Couldn't read the expression in " ++ filepath)
    Just expr -> pure expr

gnuplotFilePdf :: String
               -> String
               -> [PlotDataset]
               -> (String, String)
gnuplotFilePdf benchmarksDir xlabel results = (outputPdf, unlines [
    "set terminal pdf font \"Helvetica,13\""
  , "set output \"" ++ outputPdf ++ "\""
  , gnuplotFile xlabel results
  ])
  where outputPdf = benchmarksDir ++ "/benchmark.pdf"

gnuplotFile :: String -> [PlotDataset] -> String
gnuplotFile xlabel results =
  unlines [ "set xlabel \"Number of nodes in expression (" ++ xlabel ++ ")\""
          , "set ylabel \"Time taken to hash all subexpressions (s)"
          , "set format y '%.0se%S'"
          , "set format x '%.0se%S'"
          , "set size 1,1"
          , "set logscale xy 10"
          , "set key right bottom"
          , "set yrange [:1]"
          , "plot " ++ intercalate ", " (fmap plotDataset results)
                    ++ ", "
                    ++ intercalate ", "
                    [ "[x=2.5e3:] x / 100000000 title \"x\" at begin lt rgb \"gray\""
                    , "[x=2.5e3:] x**2 / 100000000 title \"x^2\" at begin lt rgb \"gray\""
                    , "[x=2.5e3:] x * log(x) ** 2 / 100000000 lt rgb \"gray\" title \"x log^2(x)\" at begin" ]
          ]

data PlotDataset = PlotDataset
  { pdFile  :: String
  , pdTitle :: String
  , pdColor :: String
  , pdStyle :: String
  , pdSize  :: String
  }

plotDataset :: PlotDataset -> String
plotDataset pd = intercalate " " [ quote (pdFile pd)
                                 , "title " ++ quote (pdTitle pd)
                                 , "lt rgb " ++ quote (pdColor pd)
                                 , "pt " ++ pdStyle pd
                                 , "ps " ++ pdSize pd ]
  where quote s = "\"" ++ s ++ "\""

-- We apply the argument to the function here.  If we do it at the
-- call site then GHC may float it outside of the timing loop!
-- Therefore it's important that this function not be inlined.
-- It seems it's also important for it to return IO so as not to be
-- floated outside the timing loop.
{-# NOINLINE evaluate #-}
evaluate :: (e -> a) -> e -> IO ()
evaluate a e = let !_ = a e
                     in return ()

seqHashResult :: Expr h a -> ()
seqHashResult = flip seq ()

times :: (Ord a, Num a, Monad m) => a -> s -> (s -> m s) -> m s
times n s f = times_f 0 s
  where times_f m s_ =
          if m >= n
          then return s_
          else do
            s' <- f s_
            times_f (m + 1) s'

enumFrom1 :: [a] -> [(Int, a)]
enumFrom1 = zip [1..]

loop :: Monad m => Iterator m b -> a -> (b -> a -> m (Either a a)) -> m a
loop iterator a f = do
  mNext <- stepIterator iterator
  case mNext of
    Nothing -> pure a
    Just (b, iteratorNext) -> do
      ea <- f b a
      case ea of
        Left aNext -> loop iteratorNext aNext f
        Right aDone -> pure aDone

newtype Iterator m a = Iterator (m (Maybe (a, Iterator m a)))
  deriving Functor

stepIterator :: Iterator m a -> m (Maybe (a, (Iterator m a)))
stepIterator (Iterator a) = a

iteratorOfList :: Applicative m => [a] -> Iterator m a
iteratorOfList = \case
  []   -> Iterator (pure Nothing)
  x:xs -> Iterator (pure (Just (x, iteratorOfList xs)))

iteratorPure :: Applicative m => a -> Iterator m a
iteratorPure a = Iterator (pure (Just (a, Iterator (pure Nothing))))
