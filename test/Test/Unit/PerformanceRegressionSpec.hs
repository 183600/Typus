{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.PerformanceRegressionSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.List (sort, nub, intersect, union, (\\))
import Data.Maybe (isJust, isNothing, fromMaybe, catMaybes)
import qualified Data.Set as Set
import qualified Data.Map as Map
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

spec :: Spec
spec = describe "Performance Regression Tests" $ do

  describe "Performance metrics" $ do
    it "creates performance metrics correctly" $ do
      let metric = PerformanceMetric "TestMetric" 1.5 "seconds"
      metricName metric `shouldBe` "TestMetric"
      metricValue metric `shouldBe` 1.5
      metricUnit metric `shouldBe` "seconds"
      
    it "compares performance metrics correctly" $ do
      let metric1 = PerformanceMetric "TestMetric" 1.5 "seconds"
          metric2 = PerformanceMetric "TestMetric" 1.5 "seconds"
          metric3 = PerformanceMetric "TestMetric" 2.0 "seconds"
      metric1 `shouldBe` metric2
      metric1 `shouldNotBe` metric3

  describe "Performance tests" $ do
    it "creates performance tests correctly" $ do
      let metric = PerformanceMetric "TestMetric" 1.5 "seconds"
          test = PerformanceTest "Test" 100 [metric]
      testName test `shouldBe` "Test"
      testInputSize test `shouldBe` 100
      testMetrics test `shouldBe` [metric]
      
    it "handles performance tests with multiple metrics" $ do
      let metric1 = PerformanceMetric "Time" 1.5 "seconds"
          metric2 = PerformanceMetric "Memory" 1024 "bytes"
          test = PerformanceTest "Test" 100 [metric1, metric2]
      length (testMetrics test) `shouldBe` 2

  describe "Performance baselines" $ do
    it "creates performance baselines correctly" $ do
      let metric = PerformanceMetric "TestMetric" 1.5 "seconds"
          test = PerformanceTest "Test" 100 [metric]
          baseline = PerformanceBaseline "Baseline" [test]
      baselineName baseline `shouldBe` "Baseline"
      baselineTests baseline `shouldBe` [test]
      
    it "handles baselines with multiple tests" $ do
      let metric1 = PerformanceMetric "Time1" 1.5 "seconds"
          metric2 = PerformanceMetric "Time2" 2.0 "seconds"
          test1 = PerformanceTest "Test1" 100 [metric1]
          test2 = PerformanceTest "Test2" 200 [metric2]
          baseline = PerformanceBaseline "Baseline" [test1, test2]
      length (baselineTests baseline) `shouldBe` 2

  describe "Performance regressions" $ do
    it "creates performance regressions correctly" $ do
      let regression = PerformanceRegression "Test" "Time" 1.0 1.5 10.0
      regressionTestName regression `shouldBe` "Test"
      regressionMetricName regression `shouldBe` "Time"
      regressionOldValue regression `shouldBe` 1.0
      regressionNewValue regression `shouldBe` 1.5
      regressionThreshold regression `shouldBe` 10.0

  describe "Performance measurement" $ do
    it "measures lexing performance" $ do
      content <- return $ replicate 100 'a'
      metric <- measureLexingPerformance content
      metricName metric `shouldBe` "LexingTime"
      metricUnit metric `shouldBe` "seconds"
      metricValue metric `shouldSatisfy` (> 0)
      
    it "measures parsing performance" $ do
      metric <- measureParsingPerformance 100
      metricName metric `shouldBe` "ParsingTime"
      metricUnit metric `shouldBe` "seconds"
      metricValue metric `shouldSatisfy` (> 0)
      
    it "measures type checking performance" $ do
      metric <- measureTypeCheckingPerformance 100
      metricName metric `shouldBe` "TypeCheckingTime"
      metricUnit metric `shouldBe` "seconds"
      metricValue metric `shouldSatisfy` (> 0)
      
    it "measures code generation performance" $ do
      metric <- measureCodeGenerationPerformance 100
      metricName metric `shouldBe` "CodeGenerationTime"
      metricUnit metric `shouldBe` "seconds"
      metricValue metric `shouldSatisfy` (> 0)
      
    it "measures memory usage" $ do
      metric <- measureMemoryUsage 1024
      metricName metric `shouldBe` "MemoryUsage"
      metricUnit metric `shouldBe` "bytes"
      metricValue metric `shouldBe` 1024.0

  describe "Performance test execution" $ do
    it "runs lexing performance test" $ do
      test <- runPerformanceTest "Lexing" 100
      testName test `shouldBe` "Lexing"
      testInputSize test `shouldBe` 100
      length (testMetrics test) `shouldBe` 1
      
    it "runs parsing performance test" $ do
      test <- runPerformanceTest "Parsing" 100
      testName test `shouldBe` "Parsing"
      testInputSize test `shouldBe` 100
      length (testMetrics test) `shouldBe` 1
      
    it "runs type checking performance test" $ do
      test <- runPerformanceTest "TypeChecking" 100
      testName test `shouldBe` "TypeChecking"
      testInputSize test `shouldBe` 100
      length (testMetrics test) `shouldBe` 1
      
    it "runs code generation performance test" $ do
      test <- runPerformanceTest "CodeGeneration" 100
      testName test `shouldBe` "CodeGeneration"
      testInputSize test `shouldBe` 100
      length (testMetrics test) `shouldBe` 1
      
    it "runs memory usage test" $ do
      test <- runPerformanceTest "Memory" 1024
      testName test `shouldBe` "Memory"
      testInputSize test `shouldBe` 1024
      length (testMetrics test) `shouldBe` 1
      
    it "handles unknown test types" $ do
      test <- runPerformanceTest "Unknown" 100
      testName test `shouldBe` "Unknown"
      testInputSize test `shouldBe` 100
      testMetrics test `shouldBe` []

  describe "Performance comparison" $ do
    it "detects performance regressions" $ do
      let oldMetric = PerformanceMetric "Time" 1.0 "seconds"
          newMetric = PerformanceMetric "Time" 1.5 "seconds"
          regression = comparePerformanceMetrics oldMetric newMetric 10.0
      isJust regression `shouldBe` True
      let Just r = regression
      regressionOldValue r `shouldBe` 1.0
      regressionNewValue r `shouldBe` 1.5
      
    it "does not detect regressions within threshold" $ do
      let oldMetric = PerformanceMetric "Time" 1.0 "seconds"
          newMetric = PerformanceMetric "Time" 1.05 "seconds"
          regression = comparePerformanceMetrics oldMetric newMetric 10.0
      regression `shouldBe` Nothing
      
    it "detects performance improvements" $ do
      let oldMetric = PerformanceMetric "Time" 1.5 "seconds"
          newMetric = PerformanceMetric "Time" 1.0 "seconds"
          regression = comparePerformanceMetrics oldMetric newMetric 10.0
      isJust regression `shouldBe` True
      let Just r = regression
      regressionOldValue r `shouldBe` 1.5
      regressionNewValue r `shouldBe` 1.0

  describe "Regression detection" $ do
    it "detects regressions between baselines" $ do
      let oldMetric = PerformanceMetric "Time" 1.0 "seconds"
          newMetric = PerformanceMetric "Time" 1.5 "seconds"
          oldTest = PerformanceTest "Test" 100 [oldMetric]
          newTest = PerformanceTest "Test" 100 [newMetric]
          oldBaseline = PerformanceBaseline "Old" [oldTest]
          newBaseline = PerformanceBaseline "New" [newTest]
          regressions = detectRegressions oldBaseline newBaseline 10.0
      length regressions `shouldBe` 1
      
    it "handles multiple regressions" $ do
      let oldMetric1 = PerformanceMetric "Time1" 1.0 "seconds"
          oldMetric2 = PerformanceMetric "Time2" 2.0 "seconds"
          newMetric1 = PerformanceMetric "Time1" 1.5 "seconds"
          newMetric2 = PerformanceMetric "Time2" 2.5 "seconds"
          oldTest = PerformanceTest "Test" 100 [oldMetric1, oldMetric2]
          newTest = PerformanceTest "Test" 100 [newMetric1, newMetric2]
          oldBaseline = PerformanceBaseline "Old" [oldTest]
          newBaseline = PerformanceBaseline "New" [newTest]
          regressions = detectRegressions oldBaseline newBaseline 10.0
      length regressions `shouldBe` 2

  describe "Performance reports" $ do
    it "creates performance reports correctly" $ do
      let oldMetric = PerformanceMetric "Time" 1.0 "seconds"
          newMetric = PerformanceMetric "Time" 1.5 "seconds"
          oldTest = PerformanceTest "Test" 100 [oldMetric]
          newTest = PerformanceTest "Test" 100 [newMetric]
          oldBaseline = PerformanceBaseline "Old" [oldTest]
          newBaseline = PerformanceBaseline "New" [newTest]
          report = createPerformanceReport [oldBaseline] [newBaseline] 10.0
      length (reportBaselines report) `shouldBe` 1
      length (reportRegressions report) `shouldBe` 1
      length (reportImprovements report) `shouldBe` 0
      
    it "separates regressions from improvements" $ do
      let oldMetric1 = PerformanceMetric "Time1" 1.0 "seconds"
          oldMetric2 = PerformanceMetric "Time2" 2.0 "seconds"
          newMetric1 = PerformanceMetric "Time1" 1.5 "seconds"  -- Regression
          newMetric2 = PerformanceMetric "Time2" 1.5 "seconds"  -- Improvement
          oldTest = PerformanceTest "Test" 100 [oldMetric1, oldMetric2]
          newTest = PerformanceTest "Test" 100 [newMetric1, newMetric2]
          oldBaseline = PerformanceBaseline "Old" [oldTest]
          newBaseline = PerformanceBaseline "New" [newTest]
          report = createPerformanceReport [oldBaseline] [newBaseline] 10.0
      length (reportRegressions report) `shouldBe` 1
      length (reportImprovements report) `shouldBe` 1

  describe "QuickCheck properties" $ do
    it "performance metrics are positive" $ property $
      \name value unit ->
        let metric = PerformanceMetric name value unit
        in metricValue metric `shouldSatisfy` (>= 0)
        
    it "regression detection is consistent" $ property $
      \oldValue newValue threshold ->
        let oldMetric = PerformanceMetric "Test" oldValue "seconds"
            newMetric = PerformanceMetric "Test" newValue "seconds"
            regression1 = comparePerformanceMetrics oldMetric newMetric threshold
            regression2 = comparePerformanceMetrics oldMetric newMetric threshold
        in regression1 `shouldBe` regression2
        
    it "regression threshold affects detection" $ property $
      \oldValue newValue ->
        let oldMetric = PerformanceMetric "Test" oldValue "seconds"
            newMetric = PerformanceMetric "Test" newValue "seconds"
            lowThreshold = 1.0
            highThreshold = 100.0
            regressionLow = comparePerformanceMetrics oldMetric newMetric lowThreshold
            regressionHigh = comparePerformanceMetrics oldMetric newMetric highThreshold
        in if isJust regressionLow
           then isJust regressionHigh `shouldBe` True
           else True  -- High threshold might not detect what low threshold detects

  describe "Edge cases" $ do
    it "handles zero input size" $ do
      test <- runPerformanceTest "Lexing" 0
      testName test `shouldBe` "Lexing"
      testInputSize test `shouldBe` 0
      
    it "handles very large input size" $ do
      test <- runPerformanceTest "Memory" 1000000
      testName test `shouldBe` "Memory"
      testInputSize test `shouldBe` 1000000
      
    it "handles identical performance values" $ do
      let oldMetric = PerformanceMetric "Time" 1.0 "seconds"
          newMetric = PerformanceMetric "Time" 1.0 "seconds"
          regression = comparePerformanceMetrics oldMetric newMetric 10.0
      regression `shouldBe` Nothing
      
    it "handles zero threshold" $ do
      let oldMetric = PerformanceMetric "Time" 1.0 "seconds"
          newMetric = PerformanceMetric "Time" 1.1 "seconds"
          regression = comparePerformanceMetrics oldMetric newMetric 0.0
      isJust regression `shouldBe` True
      
    it "handles empty baselines" $ do
      let oldBaseline = PerformanceBaseline "Old" []
          newBaseline = PerformanceBaseline "New" []
          report = createPerformanceReport [oldBaseline] [newBaseline] 10.0
      length (reportRegressions report) `shouldBe` 0
      length (reportImprovements report) `shouldBe` 0
      
    it "handles mismatched test counts" $ do
      let oldMetric = PerformanceMetric "Time" 1.0 "seconds"
          newMetric = PerformanceMetric "Time" 1.5 "seconds"
          oldTest = PerformanceTest "Test" 100 [oldMetric]
          newTest = PerformanceTest "Test" 100 [newMetric]
          oldBaseline = PerformanceBaseline "Old" [oldTest]
          newBaseline = PerformanceBaseline "New" [oldTest, newTest]  -- Extra test
          report = createPerformanceReport [oldBaseline] [newBaseline] 10.0
      length (reportRegressions report) `shouldBe` 1  -- Only compares matching tests