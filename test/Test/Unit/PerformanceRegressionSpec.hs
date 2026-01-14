{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.PerformanceRegressionSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.Maybe (isJust, catMaybes)
import SourceLocation (SourcePos(..), SourceSpan(..))
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

-- Mock data types for performance regression testing
data PerformanceMetric = PerformanceMetric
  { metricName :: String
  , metricValue :: Double
  , metricUnit :: String
  } deriving (Show, Eq)

data PerformanceTest = PerformanceTest
  { testName :: String
  , testInputSize :: Int
  , testMetrics :: [PerformanceMetric]
  } deriving (Show, Eq)

data PerformanceBaseline = PerformanceBaseline
  { baselineName :: String
  , baselineTests :: [PerformanceTest]
  } deriving (Show, Eq)

data PerformanceRegression = PerformanceRegression
  { regressionTestName :: String
  , regressionMetricName :: String
  , regressionOldValue :: Double
  , regressionNewValue :: Double
  , regressionThreshold :: Double
  } deriving (Show, Eq)

data PerformanceReport = PerformanceReport
  { reportBaselines :: [PerformanceBaseline]
  , reportRegressions :: [PerformanceRegression]
  , reportImprovements :: [PerformanceRegression]
  } deriving (Show, Eq)

-- Mock performance functions
measureLexingPerformance :: String -> IO PerformanceMetric
measureLexingPerformance content = do
  start <- getCPUTime
  let _ = length content  -- Mock work
  end <- getCPUTime
  let diff = fromIntegral (end - start) / (10^12)
  return $ PerformanceMetric "LexingTime" diff "seconds"

measureParsingPerformance :: Int -> IO PerformanceMetric
measureParsingPerformance tokenCount = do
  start <- getCPUTime
  let _ = sum [1..tokenCount]  -- Mock work
  end <- getCPUTime
  let diff = fromIntegral (end - start) / (10^12)
  return $ PerformanceMetric "ParsingTime" diff "seconds"

measureTypeCheckingPerformance :: Int -> IO PerformanceMetric
measureTypeCheckingPerformance nodeCount = do
  start <- getCPUTime
  let _ = product [1..min nodeCount 100]  -- Mock work with cap to avoid overflow
  end <- getCPUTime
  let diff = fromIntegral (end - start) / (10^12)
  return $ PerformanceMetric "TypeCheckingTime" diff "seconds"

measureCodeGenerationPerformance :: Int -> IO PerformanceMetric
measureCodeGenerationPerformance instructionCount = do
  start <- getCPUTime
  let _ = sum [1..instructionCount]  -- Mock work
  end <- getCPUTime
  let diff = fromIntegral (end - start) / (10^12)
  return $ PerformanceMetric "CodeGenerationTime" diff "seconds"

measureMemoryUsage :: Int -> IO PerformanceMetric
measureMemoryUsage size = do
  let _ = replicate size 'a'  -- Mock memory allocation
  return $ PerformanceMetric "MemoryUsage" (fromIntegral size) "bytes"

runPerformanceTest :: String -> Int -> IO PerformanceTest
runPerformanceTest testName inputSize = do
  case testName of
    "Lexing" -> do
      content <- return $ replicate inputSize 'a'
      metric <- measureLexingPerformance content
      return $ PerformanceTest testName inputSize [metric]
    "Parsing" -> do
      metric <- measureParsingPerformance inputSize
      return $ PerformanceTest testName inputSize [metric]
    "TypeChecking" -> do
      metric <- measureTypeCheckingPerformance inputSize
      return $ PerformanceTest testName inputSize [metric]
    "CodeGeneration" -> do
      metric <- measureCodeGenerationPerformance inputSize
      return $ PerformanceTest testName inputSize [metric]
    "Memory" -> do
      metric <- measureMemoryUsage inputSize
      return $ PerformanceTest testName inputSize [metric]
    _ -> return $ PerformanceTest testName inputSize []

comparePerformanceMetrics :: PerformanceMetric -> PerformanceMetric -> Double -> Maybe PerformanceRegression
comparePerformanceMetrics oldMetric newMetric threshold = 
  let oldValue = metricValue oldMetric
      newValue = metricValue newMetric
      regressionPercent = (newValue - oldValue) / oldValue * 100
  in if regressionPercent > threshold
     then Just $ PerformanceRegression "" (metricName oldMetric) oldValue newValue threshold
     else Nothing

detectRegressions :: PerformanceBaseline -> PerformanceBaseline -> Double -> [PerformanceRegression]
detectRegressions oldBaseline newBaseline threshold = 
  let oldTests = baselineTests oldBaseline
      newTests = baselineTests newBaseline
      pairs = zip oldTests newTests
      regressions = concatMap (\(oldTest, newTest) -> 
        let oldMetrics = testMetrics oldTest
            newMetrics = testMetrics newTest
            metricPairs = zip oldMetrics newMetrics
        in catMaybes $ map (\(oldMetric, newMetric) -> 
          comparePerformanceMetrics oldMetric newMetric threshold) metricPairs
        ) pairs
    in regressions

createPerformanceReport :: [PerformanceBaseline] -> [PerformanceBaseline] -> Double -> PerformanceReport
createPerformanceReport oldBaselines newBaselines threshold = 
  let baselinePairs = zip oldBaselines newBaselines
      allRegressions = concatMap (\(old, new) -> detectRegressions old new threshold) baselinePairs
      improvements = filter (\r -> regressionNewValue r < regressionOldValue r) allRegressions
      actualRegressions = filter (\r -> regressionNewValue r > regressionOldValue r) allRegressions
  in PerformanceReport oldBaselines actualRegressions improvements

tests :: TestTree
tests = testGroup "Performance Regression Tests"
  [ testGroup "Performance metrics"
    [ testCase "creates performance metrics correctly" $ do
        let metric = PerformanceMetric "TestMetric" 1.5 "seconds"
        metricName metric @?= "TestMetric"
        metricValue metric @?= 1.5
        metricUnit metric @?= "seconds"
      
    , testCase "compares performance metrics correctly" $ do
        let metric1 = PerformanceMetric "TestMetric" 1.5 "seconds"
            metric2 = PerformanceMetric "TestMetric" 1.5 "seconds"
            metric3 = PerformanceMetric "TestMetric" 2.0 "seconds"
        metric1 @?= metric2
        assertBool "metric1 should not be metric3" (metric1 /= metric3)
    ]

  , testGroup "Performance tests"
    [ testCase "creates performance tests correctly" $
        let metric = PerformanceMetric "TestMetric" 1.5 "seconds"
            test = PerformanceTest "Test" 100 [metric]
        in do
          testName test @?= "Test"
          testInputSize test @?= 100
          testMetrics test @?= [metric]
      
    , testCase "handles performance tests with multiple metrics" $ do
        let metric1 = PerformanceMetric "Time" 1.5 "seconds"
            metric2 = PerformanceMetric "Memory" 1024 "bytes"
            test = PerformanceTest "Test" 100 [metric1, metric2]
        length (testMetrics test) @?= 2
    ]

  , testGroup "Performance baselines"
    [ testCase "creates performance baselines correctly" $ do
        let metric = PerformanceMetric "TestMetric" 1.5 "seconds"
            test = PerformanceTest "Test" 100 [metric]
            baseline = PerformanceBaseline "Baseline" [test]
        baselineName baseline @?= "Baseline"
        baselineTests baseline @?= [test]
      
    , testCase "handles baselines with multiple tests" $ do
        let metric1 = PerformanceMetric "Time1" 1.5 "seconds"
            metric2 = PerformanceMetric "Time2" 2.0 "seconds"
            test1 = PerformanceTest "Test1" 100 [metric1]
            test2 = PerformanceTest "Test2" 200 [metric2]
            baseline = PerformanceBaseline "Baseline" [test1, test2]
        length (baselineTests baseline) @?= 2
    ]

  , testGroup "Performance regressions"
    [ testCase "creates performance regressions correctly" $ do
        let regression = PerformanceRegression "Test" "Time" 1.0 1.5 10.0
        regressionTestName regression @?= "Test"
        regressionMetricName regression @?= "Time"
        regressionOldValue regression @?= 1.0
        regressionNewValue regression @?= 1.5
        regressionThreshold regression @?= 10.0
    ]

  , testGroup "Performance measurement"
    [ testCase "measures lexing performance" $ do
        content <- return $ replicate 100 'a'
        metric <- measureLexingPerformance content
        metricName metric @?= "LexingTime"
        metricUnit metric @?= "seconds"
        assertBool "metricValue should be > 0" (metricValue metric > 0)
      
    , testCase "measures parsing performance" $ do
        metric <- measureParsingPerformance 100
        metricName metric @?= "ParsingTime"
        metricUnit metric @?= "seconds"
        assertBool "metricValue should be > 0" (metricValue metric > 0)
      
    , testCase "measures type checking performance" $ do
        metric <- measureTypeCheckingPerformance 100
        metricName metric @?= "TypeCheckingTime"
        metricUnit metric @?= "seconds"
        assertBool "metricValue should be > 0" (metricValue metric > 0)
      
    , testCase "measures code generation performance" $ do
        metric <- measureCodeGenerationPerformance 100
        metricName metric @?= "CodeGenerationTime"
        metricUnit metric @?= "seconds"
        assertBool "metricValue should be > 0" (metricValue metric > 0)
      
    , testCase "measures memory usage" $ do
        metric <- measureMemoryUsage 1024
        metricName metric @?= "MemoryUsage"
        metricUnit metric @?= "bytes"
        metricValue metric @?= 1024.0
    ]

  , testGroup "Performance test execution"
    [ testCase "runs lexing performance test" $ do
        test <- runPerformanceTest "Lexing" 100
        testName test @?= "Lexing"
        testInputSize test @?= 100
        length (testMetrics test) @?= 1
      
    , testCase "runs parsing performance test" $ do
        test <- runPerformanceTest "Parsing" 100
        testName test @?= "Parsing"
        testInputSize test @?= 100
        length (testMetrics test) @?= 1
      
    , testCase "runs type checking performance test" $ do
        test <- runPerformanceTest "TypeChecking" 100
        testName test @?= "TypeChecking"
        testInputSize test @?= 100
        length (testMetrics test) @?= 1
      
    , testCase "runs code generation performance test" $ do
        test <- runPerformanceTest "CodeGeneration" 100
        testName test @?= "CodeGeneration"
        testInputSize test @?= 100
        length (testMetrics test) @?= 1
      
    , testCase "runs memory usage test" $ do
        test <- runPerformanceTest "Memory" 1024
        testName test @?= "Memory"
        testInputSize test @?= 1024
        length (testMetrics test) @?= 1
      
    , testCase "handles unknown test types" $ do
        test <- runPerformanceTest "Unknown" 100
        testName test @?= "Unknown"
        testInputSize test @?= 100
        testMetrics test @?= []
    ]

  , testGroup "Performance comparison"
    [ testCase "detects performance regressions" $ do
        let oldMetric = PerformanceMetric "Time" 1.0 "seconds"
            newMetric = PerformanceMetric "Time" 1.5 "seconds"
            regression = comparePerformanceMetrics oldMetric newMetric 10.0
        assertBool "regression should be Just" (isJust regression)
        let Just r = regression
        regressionOldValue r @?= 1.0
        regressionNewValue r @?= 1.5
      
    , testCase "does not detect regressions within threshold" $ do
        let oldMetric = PerformanceMetric "Time" 1.0 "seconds"
            newMetric = PerformanceMetric "Time" 1.05 "seconds"
            regression = comparePerformanceMetrics oldMetric newMetric 10.0
        regression @?= Nothing
      
    , testCase "detects performance improvements" $ do
        let oldMetric = PerformanceMetric "Time" 1.5 "seconds"
            newMetric = PerformanceMetric "Time" 1.0 "seconds"
            regression = comparePerformanceMetrics oldMetric newMetric 10.0
        assertBool "regression should be Just" (isJust regression)
        let Just r = regression
        regressionOldValue r @?= 1.5
        regressionNewValue r @?= 1.0
    ]

  , testGroup "Regression detection"
    [ testCase "detects regressions between baselines" $ do
        let oldMetric = PerformanceMetric "Time" 1.0 "seconds"
            newMetric = PerformanceMetric "Time" 1.5 "seconds"
            oldTest = PerformanceTest "Test" 100 [oldMetric]
            newTest = PerformanceTest "Test" 100 [newMetric]
            oldBaseline = PerformanceBaseline "Old" [oldTest]
            newBaseline = PerformanceBaseline "New" [newTest]
            regressions = detectRegressions oldBaseline newBaseline 10.0
        length regressions @?= 1
      
    , testCase "handles multiple regressions" $ do
        let oldMetric1 = PerformanceMetric "Time1" 1.0 "seconds"
            oldMetric2 = PerformanceMetric "Time2" 2.0 "seconds"
            newMetric1 = PerformanceMetric "Time1" 1.5 "seconds"
            newMetric2 = PerformanceMetric "Time2" 2.5 "seconds"
            oldTest = PerformanceTest "Test" 100 [oldMetric1, oldMetric2]
            newTest = PerformanceTest "Test" 100 [newMetric1, newMetric2]
            oldBaseline = PerformanceBaseline "Old" [oldTest]
            newBaseline = PerformanceBaseline "New" [newTest]
            regressions = detectRegressions oldBaseline newBaseline 10.0
        length regressions @?= 2
    ]

  , testGroup "Performance reports"
    [ testCase "creates performance reports correctly" $ do
        let oldMetric = PerformanceMetric "Time" 1.0 "seconds"
            newMetric = PerformanceMetric "Time" 1.5 "seconds"
            oldTest = PerformanceTest "Test" 100 [oldMetric]
            newTest = PerformanceTest "Test" 100 [newMetric]
            oldBaseline = PerformanceBaseline "Old" [oldTest]
            newBaseline = PerformanceBaseline "New" [newTest]
            report = createPerformanceReport [oldBaseline] [newBaseline] 10.0
        length (reportBaselines report) @?= 1
        length (reportRegressions report) @?= 1
        length (reportImprovements report) @?= 0
      
    , testCase "separates regressions from improvements" $ do
        let oldMetric1 = PerformanceMetric "Time1" 1.0 "seconds"
            oldMetric2 = PerformanceMetric "Time2" 2.0 "seconds"
            newMetric1 = PerformanceMetric "Time1" 1.5 "seconds"  -- Regression
            newMetric2 = PerformanceMetric "Time2" 1.5 "seconds"  -- Improvement
            oldTest = PerformanceTest "Test" 100 [oldMetric1, oldMetric2]
            newTest = PerformanceTest "Test" 100 [newMetric1, newMetric2]
            oldBaseline = PerformanceBaseline "Old" [oldTest]
            newBaseline = PerformanceBaseline "New" [newTest]
            report = createPerformanceReport [oldBaseline] [newBaseline] 10.0
        length (reportRegressions report) @?= 1
        length (reportImprovements report) @?= 1
    ]

  , testGroup "QuickCheck properties"
    [ testProperty "performance metrics are positive" $
        \name value unit ->
          let metric = PerformanceMetric name value unit
          in metricValue metric >= 0
        
    , testProperty "regression detection is consistent" $
        \oldValue newValue threshold ->
          let oldMetric = PerformanceMetric "Test" oldValue "seconds"
              newMetric = PerformanceMetric "Test" newValue "seconds"
              regression1 = comparePerformanceMetrics oldMetric newMetric threshold
              regression2 = comparePerformanceMetrics oldMetric newMetric threshold
          in regression1 == regression2
        
    , testProperty "regression threshold affects detection" $
        \oldValue newValue ->
          let oldMetric = PerformanceMetric "Test" oldValue "seconds"
              newMetric = PerformanceMetric "Test" newValue "seconds"
              lowThreshold = 1.0
              highThreshold = 100.0
              regressionLow = comparePerformanceMetrics oldMetric newMetric lowThreshold
              regressionHigh = comparePerformanceMetrics oldMetric newMetric highThreshold
          in if isJust regressionLow
             then isJust regressionHigh
             else True  -- High threshold might not detect what low threshold detects
    ]

  , testGroup "Edge cases"
    [ testCase "handles zero input size" $ do
        test <- runPerformanceTest "Lexing" 0
        testName test @?= "Lexing"
        testInputSize test @?= 0
      
    , testCase "handles very large input size" $ do
        test <- runPerformanceTest "Memory" 1000000
        testName test @?= "Memory"
        testInputSize test @?= 1000000
      
    , testCase "handles identical performance values" $ do
        let oldMetric = PerformanceMetric "Time" 1.0 "seconds"
            newMetric = PerformanceMetric "Time" 1.0 "seconds"
            regression = comparePerformanceMetrics oldMetric newMetric 10.0
        regression @?= Nothing
      
    , testCase "handles zero threshold" $ do
        let oldMetric = PerformanceMetric "Time" 1.0 "seconds"
            newMetric = PerformanceMetric "Time" 1.1 "seconds"
            regression = comparePerformanceMetrics oldMetric newMetric 0.0
        assertBool "regression should be Just" (isJust regression)
      
    , testCase "handles empty baselines" $ do
        let oldBaseline = PerformanceBaseline "Old" []
            newBaseline = PerformanceBaseline "New" []
            report = createPerformanceReport [oldBaseline] [newBaseline] 10.0
        length (reportRegressions report) @?= 0
        length (reportImprovements report) @?= 0
      
    , testCase "handles mismatched test counts" $ do
        let oldMetric = PerformanceMetric "Time" 1.0 "seconds"
            newMetric = PerformanceMetric "Time" 1.5 "seconds"
            oldTest = PerformanceTest "Test" 100 [oldMetric]
            newTest = PerformanceTest "Test" 100 [newMetric]
            oldBaseline = PerformanceBaseline "Old" [oldTest]
            newBaseline = PerformanceBaseline "New" [oldTest, newTest]  -- Extra test
            report = createPerformanceReport [oldBaseline] [newBaseline] 10.0
        length (reportRegressions report) @?= 1  -- Only compares matching tests
    ]
  ]