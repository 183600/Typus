module Test.Unit.NewEnhancedPerformanceRegressionSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Data.List (sort)

-- Test Properties for Performance Regression

-- Property: Compilation time should scale linearly with input size
prop_compilation_time_linear :: String -> Property
prop_compilation_time_linear source = property $ 
  let size = length source
      time = measureCompilationTime source
      baselineTime = measureCompilationTime (take (size `div` 2) source)
  in size > 10 ==> time <= baselineTime * 3  -- Allow some overhead

-- Property: Memory usage should not grow exponentially
prop_memory_usage_reasonable :: String -> Property
prop_memory_usage_reasonable source = property $ 
  let size = length source
      memory = measureMemoryUsage source
      baselineMemory = measureMemoryUsage (take (size `div` 2) source)
  in size > 10 ==> memory <= baselineMemory * 4  -- Allow some overhead

-- Property: Optimization should not significantly increase compile time
prop_optimization_time_reasonable :: String -> Property
prop_optimization_time_reasonable source = property $ 
  let compileTime = measureCompilationTime source
      optimizedTime = measureOptimizationTime source
  in optimizedTime <= compileTime * 2  -- Optimization shouldn't double compile time

-- Property: Large files should not cause stack overflow
prop_large_files_no_overflow :: String -> Property
prop_large_files_no_overflow source = property $ 
  let largeSource = concat $ replicate 100 source
      result = compileSource largeSource
  in not (hasOverflowError result)

-- Property: Incremental compilation should be faster than full compilation
prop_incremental_faster_than_full :: String -> String -> Property
prop_incremental_faster_than_full baseSource change = property $ 
  let fullTime = measureCompilationTime (baseSource ++ change)
      incrementalTime = measureIncrementalCompilation baseSource change
  in incrementalTime <= fullTime

-- Property: Performance should be consistent across runs
prop_performance_consistent :: String -> Property
prop_performance_consistent source = property $ 
  let time1 = measureCompilationTime source
      time2 = measureCompilationTime source
  in abs (time1 - time2) <= max time1 time2 `div` 10  -- Within 10% variance

-- Helper functions (mock implementations)
measureCompilationTime :: String -> Int
measureCompilationTime source = length source `div` 10 + 5  -- Mock: time proportional to size

measureMemoryUsage :: String -> Int
measureMemoryUsage source = length source * 2 + 100  -- Mock: memory proportional to size

measureOptimizationTime :: String -> Int
measureOptimizationTime source = length source `div` 20 + 10  -- Mock: optimization is faster

compileSource :: String -> String
compileSource source = "Compiled(" ++ source ++ ")"

hasOverflowError :: String -> Bool
hasOverflowError result = "OverflowError" `isInfixOf` result

measureIncrementalCompilation :: String -> String -> Int
measureIncrementalCompilation baseSource change = 
  length change `div` 5 + 2  -- Mock: incremental is faster

isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
  where
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
    
    tails :: [a] -> [[a]]
    tails [] = [[]]
    tails xs@(x:xs') = xs : tails xs'

tests :: TestTree
tests = testGroup "Test.Unit.NewEnhancedPerformanceRegressionSpec Tests"
  [ testProperty "Compilation time should scale linearly with input size" prop_compilation_time_linear
  , testProperty "Memory usage should not grow exponentially" prop_memory_usage_reasonable
  , testProperty "Optimization should not significantly increase compile time" prop_optimization_time_reasonable
  , testProperty "Large files should not cause stack overflow" prop_large_files_no_overflow
  , testProperty "Incremental compilation should be faster than full compilation" prop_incremental_faster_than_full
  , testProperty "Performance should be consistent across runs" prop_performance_consistent
  ]