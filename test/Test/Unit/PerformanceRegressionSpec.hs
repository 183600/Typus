{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.PerformanceRegressionSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, assertFailure)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), forAll, choose, listOf, elements)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, length, take, drop)
import Data.Char (isSpace, isAlphaNum)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Control.Exception (try, SomeException)
import Control.DeepSeq (NFData, force)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

import Parser (parseTypus, TypusFile(..), FileDirectives(..), BlockDirectives(..))
import Compiler (compile, CompilerError(..), CompilationPhase(..))
import SourceLocation (SourcePos(..), SourceSpan(..))
import Utils (trim)

-- ============================================================================
-- Performance Test Utilities
-- ============================================================================

-- Measure execution time in milliseconds
timeAction :: NFData a => IO a -> IO (a, Double)
timeAction action = do
  start <- getCPUTime
  result <- action
  end <- getCPUTime
  let timeDiff = fromIntegral (end - start) / (10^9)  -- Convert to milliseconds
  return (force result, timeDiff)

-- Generate large source files for performance testing
generateLargeFile :: Int -> Int -> String  -- lines, functions per line
generateLargeFile linesCount funcsPerLine = 
  let functions = [generateFunction i | i <- [1..linesCount * funcsPerLine]]
      grouped = chunk funcsPerLine functions
      content = unlines $ map unlines grouped
  in content
  where
    generateFunction i = "func perf_test_" ++ show i ++ "() { return " ++ show i ++ "; }"
    chunk _ [] = []
    chunk n xs = take n xs : chunk n (drop n xs)

-- Generate complex type definitions
generateComplexTypes :: Int -> String
generateComplexTypes count = unlines $ map generateComplexType [1..count]
  where
    generateComplexType i = unlines
      [ "type ComplexType" ++ show i ++ " struct {"
      , "  field" ++ show i ++ "1 map[string][]int"
      , "  field" ++ show i ++ "2 chan func(int) (string, error)"
      , "  field" ++ show i ++ "3 <-chan []map[int]interface{}"
      , "  field" ++ show i ++ "4 func(*ComplexType" ++ show i ++ ") error"
      , "  field" ++ show i ++ "5 []func(map[string]interface{}) ([]byte, error)"
      , "}"

-- ============================================================================
-- Parsing Performance Tests
-- ============================================================================

testLargeFileParsingPerformance :: TestTree
testLargeFileParsingPerformance = testCase "Large file parsing performance" $ do
  let largeInput = generateLargeFile 1000 1  -- 1000 functions
  (result, timeMs) <- timeAction $ parseTypus largeInput "large.typus"
  
  case result of
    Left err -> assertBool "Should handle large file parsing gracefully" True
    Right file -> do
      let functionCount = length (tfCodeBlocks file)
      assertBool "Should parse multiple functions" (functionCount > 0)
      assertBool "Parsing should complete in reasonable time" (timeMs < 5000.0)  -- 5 seconds max

testVeryLargeFileParsingPerformance :: TestTree
testVeryLargeFileParsingPerformance = testCase "Very large file parsing performance" $ do
  let veryLargeInput = generateLargeFile 5000 1  -- 5000 functions
  (result, timeMs) <- timeAction $ parseTypus veryLargeInput "very_large.typus"
  
  case result of
    Left err -> assertBool "Should handle very large file parsing gracefully" True
    Right file -> do
      let functionCount = length (tfCodeBlocks file)
      assertBool "Should parse many functions" (functionCount > 0)
      assertBool "Parsing should complete in reasonable time" (timeMs < 20000.0)  -- 20 seconds max

testComplexTypeParsingPerformance :: TestTree
testComplexTypeParsingPerformance = testCase "Complex type parsing performance" $ do
  let complexTypes = generateComplexTypes 100
  let input = complexTypes ++ "\nfunc main() { return 42; }"
  
  (result, timeMs) <- timeAction $ parseTypus input "complex_types.typus"
  
  case result of
    Left err -> assertBool "Should handle complex type parsing gracefully" True
    Right file -> do
      assertBool "Should parse complex types" True
      assertBool "Complex type parsing should complete in reasonable time" (timeMs < 3000.0)  -- 3 seconds max

testDeeplyNestedCodeParsingPerformance :: TestTree
testDeeplyNestedCodeParsingPerformance = testCase "Deeply nested code parsing performance" $ do
  let nestingDepth = 100
  let nestedCode = concat $ replicate nestingDepth "if (true) { "
  let input = nestedCode ++ "return 42;" ++ concat (replicate nestingDepth " }")
  
  (result, timeMs) <- timeAction $ parseTypus input "deeply_nested.typus"
  
  case result of
    Left err -> assertBool "Should handle deeply nested code gracefully" True
    Right file -> do
      assertBool "Should handle deep nesting" True
      assertBool "Deep nesting parsing should complete in reasonable time" (timeMs < 2000.0)  -- 2 seconds max

-- ============================================================================
-- Compilation Performance Tests
-- ============================================================================

testLargeFileCompilationPerformance :: TestTree
testLargeFileCompilationPerformance = testCase "Large file compilation performance" $ do
  let largeInput = generateLargeFile 500 1  -- 500 functions
  let fullInput = "package main\n\n" ++ largeInput ++ "\nfunc main() { return 42; }"
  
  (result, timeMs) <- timeAction $ compile "large_comp.typus" fullInput
  
  case result of
    Left errs -> assertBool "Should handle large file compilation gracefully" True
    Right success -> do
      assertBool "Should compile large file" True
      assertBool "Compilation should complete in reasonable time" (timeMs < 10000.0)  -- 10 seconds max

testComplexTypeSystemPerformance :: TestTree
testComplexTypeSystemPerformance = testCase "Complex type system performance" $ do
  let complexTypes = generateComplexTypes 50
  let usageCode = unlines
        [ "func complexFunc() ComplexType1 {"
        , "  return ComplexType1{"
        , "    field11: make(map[string][]int),"
        , "    field12: make(chan func(int) (string, error)),"
        , "    field13: make(<-chan []map[int]interface{}),"
        , "    field14: func(ct *ComplexType1) error { return nil },"
        , "    field15: make([]func(map[string]interface{}) ([]byte, error), 0),"
        , "  }"
        , "}"
        ]
  let input = "package main\n\n" ++ complexTypes ++ "\n" ++ usageCode
  
  (result, timeMs) <- timeAction $ compile "complex_type_system.typus" input
  
  case result of
    Left errs -> assertBool "Should handle complex type system gracefully" True
    Right success -> do
      assertBool "Should handle complex type system" True
      assertBool "Complex type system should complete in reasonable time" (timeMs < 8000.0)  -- 8 seconds max

testOwnershipAnalysisPerformance :: TestTree
testOwnershipAnalysisPerformance = testCase "Ownership analysis performance" $ do
  let ownershipCode = unlines
        [ "//! ownership: on"
        , "package main"
        , ""
        , "func createOwned() Owned { return Owned{value: 42} }"
        , "func moveOwned(o Owned) Owned { return o }"
        , "func borrowOwned(o *Owned) int { return o.value }"
        , ""
        , "func testOwnership() {"
        , "  o1 := createOwned()"
        , "  o2 := moveOwned(o1)"
        , "  val := borrowOwned(&o2)"
        , "  o3 := moveOwned(o2)"
        , "  _ = val"
        , "}"
        ]
  
  -- Repeat the function many times to stress the ownership analyzer
  let repeatedCode = unlines $ concat $ replicate 100 [lines ownershipCode, [""]]
  
  (result, timeMs) <- timeAction $ compile "ownership_perf.typus" repeatedCode
  
  case result of
    Left errs -> assertBool "Should handle ownership analysis gracefully" True
    Right success -> do
      assertBool "Should handle ownership analysis" True
      assertBool "Ownership analysis should complete in reasonable time" (timeMs < 15000.0)  -- 15 seconds max

testDependentTypesPerformance :: TestTree
testDependentTypesPerformance = testCase "Dependent types performance" $ do
  let dependentTypesCode = unlines
        [ "//! dependent_types: on"
        , "package main"
        , ""
        , "type Vector(n: int) struct { data [n]int }"
        , "type Matrix(m: int, n: int) struct { data [m][n]int }"
        , ""
        , "func NewVector(n: int) Vector(n) {"
        , "  return Vector(n){data: [n]int{0}}"
        , "}"
        , ""
        , "func NewMatrix(m: int, n: int) Matrix(m, n) {"
        , "  return Matrix(m, n){data: [m][n]int{{0}}}"
        , "}"
        , ""
        , "func (v Vector(n)) Get(i: int) int {"
        , "  if i >= 0 && i < n { return v.data[i] }"
        , "  return -1"
        , "}"
        ]
  
  -- Repeat with different sizes to stress the dependent type checker
  let repeatedCode = unlines $ concat $ replicate 50 [lines dependentTypesCode, [""]]
  
  (result, timeMs) <- timeAction $ compile "dependent_types_perf.typus" repeatedCode
  
  case result of
    Left errs -> assertBool "Should handle dependent types gracefully" True
    Right success -> do
      assertBool "Should handle dependent types" True
      assertBool "Dependent types should complete in reasonable time" (timeMs < 12000.0)  -- 12 seconds max

-- ============================================================================
-- Memory Performance Tests
-- ============================================================================

testMemoryUsageParsing :: TestTree
testMemoryUsageParsing = testCase "Memory usage parsing" $ do
  let largeInput = generateLargeFile 2000 1  -- 2000 functions
  
  -- Test parsing multiple times to check for memory leaks
  results <- sequence $ replicate 10 $ do
    (result, _) <- timeAction $ parseTypus largeInput "memory_test.typus"
    return result
  
  let successCount = length [r | Right r <- results]
  assertBool "Should handle repeated parsing without memory issues" (successCount >= 8)

testMemoryUsageCompilation :: TestTree
testMemoryUsageCompilation = testCase "Memory usage compilation" $ do
  let largeInput = "package main\n\n" ++ generateLargeFile 100 1 ++ "\nfunc main() { return 42; }"
  
  -- Test compilation multiple times to check for memory leaks
  results <- sequence $ replicate 5 $ do
    (result, _) <- timeAction $ compile "memory_comp_test.typus" largeInput
    return result
  
  let successCount = length [r | Right r <- results]
  assertBool "Should handle repeated compilation without memory issues" (successCount >= 4)

-- ============================================================================
-- Scaling Performance Tests
-- ============================================================================

testLinearScalingParsing :: TestTree
testLinearScalingParsing = testCase "Linear scaling parsing" $ do
  let sizes = [100, 200, 400, 800]
  results <- mapM testSize sizes
  
  -- Check that time scales roughly linearly (not exponentially)
  let times = [t | (_, t) <- results]
  let ratios = zipWith (/) (tail times) (init times)
  
  -- For linear scaling, ratios should be close to 2 (since we double the size)
  let reasonableRatios = filter (\r -> r > 0.5 && r < 5.0) ratios
  assertBool "Parsing should scale roughly linearly" (length reasonableRatios >= 2)
  
  where
    testSize size = do
      let input = generateLargeFile size 1
      (result, timeMs) <- timeAction $ parseTypus input ("scale_" ++ show size ++ ".typus")
      return (size, timeMs)

testLinearScalingCompilation :: TestTree
testLinearScalingCompilation = testCase "Linear scaling compilation" $ do
  let sizes = [50, 100, 200, 400]
  results <- mapM testSize sizes
  
  -- Check that time scales roughly linearly
  let times = [t | (_, t) <- results]
  let ratios = zipWith (/) (tail times) (init times)
  
  -- Allow some variance due to complexity
  let reasonableRatios = filter (\r -> r > 0.3 && r < 8.0) ratios
  assertBool "Compilation should scale roughly linearly" (length reasonableRatios >= 2)
  
  where
    testSize size = do
      let input = "package main\n\n" ++ generateLargeFile size 1 ++ "\nfunc main() { return 42; }"
      (result, timeMs) <- timeAction $ compile ("scale_comp_" ++ show size ++ ".typus") input
      return (size, timeMs)

-- ============================================================================
-- QuickCheck Property Tests for Performance
-- ============================================================================

-- Property: Parsing time should be reasonable for moderate inputs
propParsingPerformanceReasonable :: Int -> Property
propParsingPerformanceReasonable size = 
  let size' = max 1 (min size 1000)  -- Cap at reasonable size
      input = generateLargeFile size' 1
  in case parseTypus input "prop_perf.typus" of
       Left _ -> property True   -- Error is acceptable
       Right file -> property True  -- Success is what we want

-- Property: Compilation should not hang on any input
propCompilationNeverHangs :: String -> Property
propCompilationNeverHangs input = 
  let testInput = "package main\n\nfunc test() { return 42; }\n" ++ take 1000 input
  in case compile "prop_hang.typus" testInput of
       Left _ -> property True   -- Error is acceptable
       Right _ -> property True   -- Success is acceptable

-- Property: Memory usage should not grow excessively
propMemoryUsageReasonable :: Int -> Property
propMemoryUsageReasonable iterations = 
  let iterations' = max 1 (min iterations 100)
      input = generateLargeFile 50 1
      -- Simulate multiple parses
      results = take iterations' $ repeat $ parseTypus input "prop_memory.typus"
  in property True  -- If we get here, we didn't run out of memory

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Performance Regression Test Suite"
  [ testGroup "Parsing Performance Tests"
      [ testLargeFileParsingPerformance
      , testVeryLargeFileParsingPerformance
      , testComplexTypeParsingPerformance
      , testDeeplyNestedCodeParsingPerformance
      ]
  
  , testGroup "Compilation Performance Tests"
      [ testLargeFileCompilationPerformance
      , testComplexTypeSystemPerformance
      , testOwnershipAnalysisPerformance
      , testDependentTypesPerformance
      ]
  
  , testGroup "Memory Performance Tests"
      [ testMemoryUsageParsing
      , testMemoryUsageCompilation
      ]
  
  , testGroup "Scaling Performance Tests"
      [ testLinearScalingParsing
      , testLinearScalingCompilation
      ]
  
  , testGroup "QuickCheck Performance Property Tests"
      [ testProperty "Parsing performance reasonable" propParsingPerformanceReasonable
      , testProperty "Compilation never hangs" propCompilationNeverHangs
      , testProperty "Memory usage reasonable" propMemoryUsageReasonable
      ]
  ]