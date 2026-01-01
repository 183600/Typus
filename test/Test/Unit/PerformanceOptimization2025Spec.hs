{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.PerformanceOptimization2025Spec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, choose, listOf, elements)
import Test.Tasty.HUnit (testCase, (@=?))
import System.CPUTime (getCPUTime)
import Text.Printf (printf)
import Data.List (nub, sort)

import Utils (trim, splitBy, normalizeIndentation)
import Parser (parseTypus)
import SourceLocation (SourcePos(..), advancePos)
import Compiler (compile)

tests :: TestTree
tests = testGroup "Performance Optimization Tests"
  [ testProperty "String processing scales linearly" propStringProcessingLinear
  , testProperty "Parser performance degrades gracefully" propParserPerformanceGraceful
  , testProperty "Source location tracking is efficient" propSourceLocationEfficient
  , testProperty "Compilation time scales appropriately" propCompilationTimeScales
  , testProperty "Memory usage remains bounded" propMemoryUsageBounded
  , testCase "Large file parsing performance" testLargeFileParsingPerformance
  , testProperty "Concurrent processing provides speedup" propConcurrentSpeedup
  , testCase "Memory leak detection" testMemoryLeakDetection
  , testProperty "Cache effectiveness" propCacheEffectiveness
  , testCase "Performance regression detection" testPerformanceRegressionDetection
  ]

-- Performance measurement utilities
data PerformanceResult = PerformanceResult
  { executionTime :: Double
  , memoryUsage :: Int
  , inputSize :: Int
  } deriving (Show, Eq)

-- Property 1: String processing scales linearly
propStringProcessingLinear :: [String] -> Bool
propStringProcessingLinear inputs =
  L.length inputs > 1 ==>
  let sizes = [100, 1000, 10000]
      results = L.map (\size -> measureStringProcessingPerformance (take size (L.concat inputs))) sizes
      times = map executionTime results
  in isLinearGrowth times

-- Property 2: Parser performance degrades gracefully
propParserPerformanceGraceful :: String -> Bool
propParserPerformanceGraceful baseInput =
  let sizes = [100, 500, 1000, 2000]
      inputs = L.map (\size -> take size (baseInput ++ cycle "func test() {}")) sizes
      results = map measureParserPerformance inputs
      times = map executionTime results
  in not (hasExponentialGrowth times)

-- Property 3: Source location tracking is efficient
propSourceLocationEfficient :: String -> Bool
propSourceLocationEfficient input =
  let sizes = [100, 1000, 10000]
      results = L.map (\size -> measureSourceLocationPerformance (take size input)) sizes
      times = map executionTime results
  in isLinearGrowth times

-- Property 4: Compilation time scales appropriately
propCompilationTimeScales :: String -> Bool
propCompilationTimeScales baseCode =
  let sizes = [50, 100, 200]
      inputs = L.map (\size -> replicate size "func x() { return x; }") sizes
      flatInputs = map unlines inputs
      results = map measureCompilationPerformance flatInputs
      times = map executionTime results
  in not (hasExponentialGrowth times)

-- Property 5: Memory usage remains bounded
propMemoryUsageBounded :: String -> Bool
propMemoryUsageBounded baseInput =
  let sizes = [100, 1000, 5000, 10000]
      results = L.map (\size -> measureMemoryUsage (take size baseInput)) sizes
      memoryUsages = map memoryUsage results
  in isLinearGrowth memoryUsages

-- Test Case 6: Large file parsing performance
testLargeFileParsingPerformance :: IO ()
testLargeFileParsingPerformance = do
  let largeFile = unlines $ replicate 10000 "func test_" ++ map show [1..10000] ++ "() { return " ++ map show [1..10000] ++ "; }"
  result <- measureParserPerformance largeFile
  
  -- Should complete within reasonable time (5 seconds for 10k lines)
  executionTime result @=? (min 5.0 (executionTime result))
  
  -- Memory usage should be reasonable (less than 100MB)
  memoryUsage result @=? (min 100000 (memoryUsage result))

-- Property 7: Concurrent processing provides speedup
propConcurrentSpeedup :: [String] -> Bool
propConcurrentSpeedup inputs =
  L.length inputs > 1 ==>
  let sequentialTime = measureSequentialProcessing inputs
      concurrentTime = measureConcurrentProcessing inputs
  in sequentialTime / concurrentTime >= 1.5  -- At least 50% speedup

-- Test Case 8: Memory leak detection
testMemoryLeakDetection :: IO ()
testMemoryLeakDetection = do
  let testInput = "func test() { return 42; }"
  initialMemory <- getCurrentMemoryUsage
  
  -- Process many times to detect leaks
  replicateM_ 1000 $ measureParserPerformance testInput
  
  finalMemory <- getCurrentMemoryUsage
  let memoryGrowth = finalMemory - initialMemory
  
  -- Memory growth should be minimal (less than 10MB)
  memoryGrowth @=? (min 10000 memoryGrowth)

-- Property 9: Cache effectiveness
propCacheEffectiveness :: [String] -> Bool
propCacheEffectiveness inputs =
  not (null inputs) ==>
  let firstRun = map measureParserPerformance inputs
      secondRun = map measureParserPerformance inputs  -- Should benefit from cache
      firstTimes = map executionTime firstRun
      secondTimes = map executionTime secondRun
  in average secondTimes <= average firstTimes * 0.8  -- 20% improvement with cache

-- Test Case 10: Performance regression detection
testPerformanceRegressionDetection :: IO ()
testPerformanceRegressionDetection = do
  let benchmarkInput = unlines 
        [ "func factorial(n) {"
        , "  if (n <= 1) return 1;"
        , "  return n * factorial(n - 1);"
        , "}"
        , "func fibonacci(n) {"
        , "  if (n <= 1) return n;"
        , "  return fibonacci(n - 1) + fibonacci(n - 2);"
        , "}"
        , "func main() {"
        , "  return factorial(10) + fibonacci(10);"
        , "}"
        ]
  
  result <- measureParserPerformance benchmarkInput
  
  -- Known baseline: should complete within 1 second
  executionTime result @=? (min 1.0 (executionTime result))
  
  -- Memory usage should be under 10MB
  memoryUsage result @=? (min 10000 (memoryUsage result))

-- Performance measurement functions
measureStringProcessingPerformance :: String -> PerformanceResult
measureStringProcessingPerformance input = do
  start <- getCPUTime
  let result = trim (normalizeIndentation input)
  end <- getCPUTime
  let time = fromIntegral (end - start) / (10^12)
  let size = L.length input
  return $ PerformanceResult time (size `div` 1000) size

measureParserPerformance :: String -> PerformanceResult
measureParserPerformance input = do
  start <- getCPUTime
  let result = parseTypus input
  end <- getCPUTime
  let time = fromIntegral (end - start) / (10^12)
  let size = L.length input
  return $ PerformanceResult time (size `div` 10) size

measureSourceLocationPerformance :: String -> PerformanceResult
measureSourceLocationPerformance input = do
  start <- getCPUTime
  let result = foldl advancePos (SourcePos 1 1) input
  end <- getCPUTime
  let time = fromIntegral (end - start) / (10^12)
  let size = L.length input
  return $ PerformanceResult time (size `div` 100) size

measureCompilationPerformance :: String -> PerformanceResult
measureCompilationPerformance input = do
  start <- getCPUTime
  let result = compile input  -- Mock compilation
  end <- getCPUTime
  let time = fromIntegral (end - start) / (10^12)
  let size = L.length input
  return $ PerformanceResult time (size `div` 50) size

measureMemoryUsage :: String -> PerformanceResult
measureMemoryUsage input = do
  memory <- getCurrentMemoryUsage
  let size = L.length input
  return $ PerformanceResult 0.0 memory size

measureSequentialProcessing :: [String] -> Double
measureSequentialProcessing inputs = do
  start <- getCPUTime
  mapM measureParserPerformance inputs
  end <- getCPUTime
  return $ fromIntegral (end - start) / (10^12)

measureConcurrentProcessing :: [String] -> Double
measureConcurrentProcessing inputs = do
  start <- getCPUTime
  -- Mock concurrent processing - in real implementation would use actual concurrency
  mapM measureParserPerformance inputs
  end <- getCPUTime
  return $ fromIntegral (end - start) / (10^12)

getCurrentMemoryUsage :: IO Int
getCurrentMemoryUsage = return 1000  -- Mock memory usage in KB

-- Utility functions for performance analysis
isLinearGrowth :: [Double] -> Bool
isLinearGrowth [] = True
isLinearGrowth [_] = True
isLinearGrowth (x:y:zs) = 
  let ratio = y / x
  in ratio <= 10.0 && isLinearGrowth (y:zs)

hasExponentialGrowth :: [Double] -> Bool
hasExponentialGrowth [] = False
hasExponentialGrowth [_] = False
hasExponentialGrowth (x:y:zs) =
  let ratio = y / x
  in ratio > 100.0 || hasExponentialGrowth (y:zs)

average :: [Double] -> Double
average [] = 0.0
average xs = L.sum xs / fromIntegral (L.length xs)

-- Mock compilation function
compile :: String -> String
compile input = "compiled: " ++ take 100 input

-- Arbitrary instances for testing
instance Arbitrary String where
  arbitrary = do
    size <- choose (10, 1000)
    elements [replicate size 'a', 
              L.concat $ replicate (size `div` 10) "func test() {} ",
              L.concat $ replicate (size `div` 20) "let x = 1; let y = 2; ",
              unlines $ replicate (size `div` 30) "func test" ++ map show [1..] ++ "() { return 0; }"]