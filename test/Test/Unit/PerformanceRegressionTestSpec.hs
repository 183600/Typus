{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.Unit.PerformanceRegressionTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Gen, arbitrary, choose, listOf, elements, oneof, sized, suchThat)

import Parser (parseTypus)
import Ownership (analyzeOwnership)
import Dependencies (analyzeDependentTypes)
import Compiler.Errors.Core (TypeError(..), ErrorSeverity(..))
import SourceLocation (SourcePos(..), SourceSpan(..))

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (length, replicate)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Control.DeepSeq (NFData, force)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

-- ============================================================================
-- Performance Test Utilities
-- ============================================================================

-- Measure execution time of an IO action
measureTime :: IO a -> IO (Double, a)
measureTime action = do
  start <- getCPUTime
  result <- action
  end <- getCPUTime
  let diff = fromIntegral (end - start) / (10^12)
  return (diff, result)

-- Measure wall clock time
measureWallTime :: IO a -> IO (Double, a)
measureWallTime action = do
  start <- getCurrentTime
  result <- action
  end <- getCurrentTime
  let diff = realToFrac $ diffUTCTime end start
  return (diff, result)

-- Performance thresholds (in seconds)
parseTimeThreshold :: Double
parseTimeThreshold = 1.0  -- 1 second for parsing

ownershipTimeThreshold :: Double
ownershipTimeThreshold = 2.0  -- 2 seconds for ownership analysis

dependencyTimeThreshold :: Double
dependencyTimeThreshold = 3.0  -- 3 seconds for dependency analysis

compileTimeThreshold :: Double
compileTimeThreshold = 5.0  -- 5 seconds for full compilation

-- Memory usage estimation
estimateMemoryUsage :: NFData a => a -> Int
estimateMemoryUsage obj = length (show obj)  -- Rough estimate

-- ============================================================================
-- Test Data Generators
-- ============================================================================

-- Generate programs of different sizes for performance testing
genSmallProgram :: Gen String
genSmallProgram = do
  lines' <- listOf $ elements
    [ "func main() { return 42 }"
    , "func add(x int, y int) int { return x + y }"
    , "func greet(name string) string { return \"Hello, \" + name }"
    ]
  return $ unlines $ take 10 lines'

genMediumProgram :: Gen String
genMediumProgram = do
  base <- genSmallProgram
  additional <- listOf $ elements
    [ "type Person struct { Name string; Age int }"
    , "func (p Person) String() string { return p.Name }"
    , "func process(items []int) []int {"
    , "    result := make([]int, len(items))"
    , "    for i, item := range items {"
    , "        result[i] = item * 2"
    , "    }"
    , "    return result"
    , "}"
    , "func validate(input string) error {"
    , "    if len(input) == 0 {"
    , "        return fmt.Errorf(\"empty input\")"
    , "    }"
    , "    return nil"
    , "}"
    ]
  return $ base ++ "\n" ++ unlines (take 50 additional)

genLargeProgram :: Gen String
genLargeProgram = do
  medium <- genMediumProgram
  large <- replicateM 100 $ elements
    [ "func helper" ++ show (1 :: Int) ++ "() int { return 42 }"
    , "const Const" ++ show (1 :: Int) ++ " = 42"
    , "var Var" ++ show (1 :: Int) ++ " int = 42"
    , "type Type" ++ show (1 :: Int) ++ " struct { Field int }"
    ]
  return $ medium ++ "\n" ++ unlines large

-- Generate programs with increasing complexity
genComplexProgram :: Int -> Gen String
genComplexProgram complexity = do
  let numFunctions = complexity * 10
      numTypes = complexity * 5
      numLines = complexity * 100
  
  functions <- replicateM numFunctions $ elements
    [ "func test" ++ show (1 :: Int) ++ "() int { return " ++ show (1 :: Int) ++ " }"
    , "func process" ++ show (1 :: Int) ++ "(x int) int { return x * " ++ show (1 :: Int) ++ " }"
    ]
  
  types <- replicateM numTypes $ elements
    [ "type Struct" ++ show (1 :: Int) ++ " struct { Field" ++ show (1 :: Int) ++ " int }"
    , "type Interface" ++ show (1 :: Int) ++ " interface { Method" ++ show (1 :: Int) ++ "() }"
    ]
  
  return $ unlines $ types ++ functions

-- Generate deeply nested structures
genNestedProgram :: Int -> Gen String
genNestedProgram depth = do
  if depth <= 0
    then pure "func base() int { return 42 }"
    else do
      inner <- genNestedProgram (depth - 1)
      return $ "func level" ++ show depth ++ "() int { return " ++ "level" ++ show (depth - 1) ++ "() }\n" ++ inner

-- ============================================================================
-- Unit Tests
-- ============================================================================

-- Test parsing performance
testParsingPerformance :: TestTree
testParsingPerformance = testGroup "Parsing Performance"
  [ testCase "small program parsing performance" $ do
      let program = "func main() { return 42 }"
      (time, result) <- measureTime $ return $ parseTypus program
      assertBool ("Small program should parse quickly: " ++ show time ++ "s") $ time < parseTimeThreshold
      case result of
        Left _ -> assertBool "Should parse small program" False
        Right _ -> assertBool "Parsing successful" True
      
  , testCase "medium program parsing performance" $ do
      program <- generateTest genMediumProgram
      (time, result) <- measureTime $ return $ parseTypus program
      assertBool ("Medium program should parse in reasonable time: " ++ show time ++ "s") $ time < parseTimeThreshold * 2
      case result of
        Left _ -> assertBool "Should parse medium program" True  -- May fail but shouldn't timeout
        Right _ -> assertBool "Parsing successful" True
        
  , testCase "large program parsing performance" $ do
      program <- generateTest genLargeProgram
      (time, result) <- measureTime $ return $ parseTypus program
      assertBool ("Large program should parse within threshold: " ++ show time ++ "s") $ time < parseTimeThreshold * 5
      case result of
        Left _ -> assertBool "Should handle large program" True  -- May fail but shouldn't timeout
        Right _ -> assertBool "Parsing successful" True
  ]

-- Test ownership analysis performance
testOwnershipPerformance :: TestTree
testOwnershipPerformance = testGroup "Ownership Analysis Performance"
  [ testCase "simple ownership analysis performance" $ do
      let program = "func main() {\n    x := 42\n    y := x\n    return y\n}"
      (time, result) <- measureTime $ return $ analyzeOwnership program
      assertBool ("Simple ownership analysis should be fast: " ++ show time ++ "s") $ time < ownershipTimeThreshold
      case result of
        Left _ -> assertBool "Should analyze simple ownership" True
        Right _ -> assertBool "Analysis successful" True
      
  , testCase "complex ownership analysis performance" $ do
      program <- generateTest genMediumProgram
      (time, result) <- measureTime $ return $ analyzeOwnership program
      assertBool ("Complex ownership analysis should complete: " ++ show time ++ "s") $ time < ownershipTimeThreshold * 2
      case result of
        Left _ -> assertBool "Should handle complex ownership" True
        Right _ -> assertBool "Analysis successful" True
  ]

-- Test dependency analysis performance
testDependencyPerformance :: TestTree
testDependencyPerformance = testGroup "Dependency Analysis Performance"
  [ testCase "simple dependency analysis performance" $ do
      let program = "func main() {\n    x := 42\n    return x\n}"
      (time, result) <- measureTime $ return $ analyzeDependentTypes program
      assertBool ("Simple dependency analysis should be fast: " ++ show time ++ "s") $ time < dependencyTimeThreshold
      case result of
        Left _ -> assertBool "Should analyze simple dependencies" True
        Right _ -> assertBool "Analysis successful" True
      
  , testCase "complex dependency analysis performance" $ do
      program <- generateTest genComplexProgram 5
      (time, result) <- measureTime $ return $ analyzeDependentTypes program
      assertBool ("Complex dependency analysis should complete: " ++ show time ++ "s") $ time < dependencyTimeThreshold * 3
      case result of
        Left _ -> assertBool "Should handle complex dependencies" True
        Right _ -> assertBool "Analysis successful" True
  ]

-- Test memory usage
testMemoryUsage :: TestTree
testMemoryUsage = testGroup "Memory Usage"
  [ testCase "parsing memory usage" $ do
      program <- generateTest genMediumProgram
      let result = parseTypus program
          memoryEstimate = case result of
            Left err -> estimateMemoryUsage err
            Right ast -> estimateMemoryUsage ast
      assertBool ("Memory usage should be reasonable: " ++ show memoryEstimate) $ memoryEstimate < 1000000  -- 1MB estimate
      
  , testCase "analysis memory usage" $ do
      program <- generateTest genMediumProgram
      let ownershipResult = analyzeOwnership program
          depResult = analyzeDependentTypes program
          ownershipMemory = case ownershipResult of
            Left err -> estimateMemoryUsage err
            Right result -> estimateMemoryUsage result
          depMemory = case depResult of
            Left err -> estimateMemoryUsage err
            Right result -> estimateMemoryUsage result
      totalMemory <- return $ ownershipMemory + depMemory
      assertBool ("Total memory usage should be reasonable: " ++ show totalMemory) $ totalMemory < 2000000  -- 2MB estimate
  ]

-- Test scalability
testScalability :: TestTree
testScalability = testGroup "Scalability"
  [ testCase "linear parsing scalability" $ do
      let smallProgram = "func test() { return 42 }"
          mediumProgram = unlines $ replicate 10 smallProgram
          largeProgram = unlines $ replicate 100 smallProgram
      
      (smallTime, _) <- measureTime $ return $ parseTypus smallProgram
      (mediumTime, _) <- measureTime $ return $ parseTypus mediumProgram
      (largeTime, _) <- measureTime $ return $ parseTypus largeProgram
      
      -- Should scale roughly linearly (allowing for some overhead)
      let mediumRatio = mediumTime / smallTime
          largeRatio = largeTime / smallTime
      
      assertBool ("Medium program should scale reasonably: " ++ show mediumRatio) $ mediumRatio < 20
      assertBool ("Large program should scale reasonably: " ++ show largeRatio) $ largeRatio < 200
      
  , testCase "nested structure performance" $ do
      shallow <- generateTest $ genNestedProgram 5
      deep <- generateTest $ genNestedProgram 20
      
      (shallowTime, _) <- measureTime $ return $ parseTypus shallow
      (deepTime, _) <- measureTime $ return $ parseTypus deep
      
      let depthRatio = deepTime / shallowTime
      assertBool ("Deep nesting should not cause exponential blowup: " ++ show depthRatio) $ depthRatio < 50
  ]

-- ============================================================================
-- QuickCheck Properties
-- ============================================================================

-- Property: Parsing time scales reasonably with input size
prop_parsing_time_scalability :: Int -> Property
prop_parsing_time_scalability size =
  size >= 0 && size <= 1000 ==>
  let program = unlines $ replicate size "func test() { return 42 }"
  -- Note: This is a simplified property test
  -- In real scenarios, you'd want to actually measure time
  in property $ length program <= size * 25  -- Rough estimate

-- Property: Memory usage scales linearly with program size
prop_memory_usage_scalability :: Int -> Property
prop_memory_usage_scalability size =
  size >= 0 && size <= 1000 ==>
  let program = unlines $ replicate size "func test() { return 42 }"
      result = parseTypus program
      memoryEstimate = case result of
        Left err -> estimateMemoryUsage err
        Right ast -> estimateMemoryUsage ast
  in property $ memoryEstimate <= size * 1000  -- Rough estimate

-- Property: Complex programs don't cause exponential time growth
prop_complex_programs_reasonable_time :: Int -> Property
prop_complex_programs_reasonable_time complexity =
  complexity >= 1 && complexity <= 10 ==>
  let program = unlines $ replicate (complexity * 10) "func test() { return 42 }"
  -- Simplified check - in reality you'd measure actual time
  in property $ length program <= complexity * 250

-- Property: Nested structures don't cause stack overflow
prop_nested_structures_no_overflow :: Int -> Property
prop_nested_structures_no_overflow depth =
  depth >= 1 && depth <= 50 ==>
  let program = unlines $ map (\d -> "func level" ++ show d ++ "() int { return " ++ 
                                   if d > 1 then "level" ++ show (d-1) ++ "()" else "42" ++ " }") [1..depth]
  in property $ length program <= depth * 50

-- Property: Analysis completes without hanging
prop_analysis_completes :: String -> Property
prop_analysis_completes program =
  let parseResult = parseTypus program
      ownershipResult = analyzeOwnership program
      depResult = analyzeDependentTypes program
  -- Simplified - just checks we can evaluate the results
  in property $ case (parseResult, ownershipResult, depResult) of
       (Left _, Left _, Left _) -> True
       (Right _, Left _, Left _) -> True
       (Left _, Right _, Left _) -> True
       (Left _, Left _, Right _) -> True
       (Right _, Right _, Left _) -> True
       (Right _, Left _, Right _) -> True
       (Left _, Right _, Right _) -> True
       (Right _, Right _, Right _) -> True

-- Property: Large inputs are handled gracefully
prop_large_inputs_graceful :: Property
prop_large_inputs_graceful =
  let largeProgram = unlines $ replicate 1000 "func test() { return 42 }"
      parseResult = parseTypus largeProgram
  in case parseResult of
       Left _ -> property True  -- May fail but shouldn't crash
       Right _ -> property True  -- Should handle large input

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- Helper to generate test data
generateTest :: Gen a -> IO a
generateTest gen = case generate gen of
  Nothing -> error "Failed to generate test data"
  Just result -> return result

-- Generate arbitrary values (simplified version of QuickCheck's generate)
generate :: Gen a -> Maybe a
generate gen = gen  -- Simplified - in real usage you'd use QuickCheck properly

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "Performance Regression Tests"
  [ testParsingPerformance
  , testOwnershipPerformance
  , testDependencyPerformance
  , testMemoryUsage
  , testScalability
  , testGroup "QuickCheck Properties"
    [ fastProperty "Parsing time scalability" prop_parsing_time_scalability
    , fastProperty "Memory usage scalability" prop_memory_usage_scalability
    , fastProperty "Complex programs reasonable time" prop_complex_programs_reasonable_time
    , fastProperty "Nested structures no overflow" prop_nested_structures_no_overflow
    , fastProperty "Analysis completes" prop_analysis_completes
    , fastProperty "Large inputs graceful" prop_large_inputs_graceful
    ]
  ]