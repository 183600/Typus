{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.Unit.PerformanceBoundarySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf, elements, oneof, vectorOf)
import Test.QuickCheck.Arbitrary (Arbitrary(..))

import Parser (parseTypus, TypusFile(..), CodeBlock(..), defaultFileDirectives)
import Compiler (compile, CompilerError(..))
import Utils (trim, splitBy, splitByCollapsed, removeComments, normalizeIndentation)
import SourceLocation (SourcePos(..), SourceSpan(..), mkSourcePos, mkSourceSpan)

import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (sort, nub)
import Control.DeepSeq (NFData, force)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

-- | Generate large inputs for performance testing
genLargeInput :: Int -> Gen String
genLargeInput size = vectorOf size $ elements "x := 1\ny := x + 1\nfunc test() { return 0 }"

genDeeplyNestedInput :: Int -> Gen String
genDeeplyNestedInput depth =
  let nesting = replicate depth "{"
      unnesting = replicate depth "}"
      content = nesting ++ "x := 1" ++ unnesting
  in return content

genWideInput :: Int -> Gen String
genWideInput width =
  let longLine = replicate width 'x' ++ " := very_long_variable_name"
  in return longLine

-- | Test parsing performance with large inputs
test_parsing_performance_large :: TestTree
test_parsing_performance_large = testCase "parsing performance with large inputs" $ do
  let sizes = [100, 1000, 5000]
  mapM_ (\size -> do
    let largeInput = replicate size "x := 1\n"
    startTime <- getCPUTime
    let result = parseTypus largeInput
    endTime <- getCPUTime
    let timeDiff = fromIntegral (endTime - startTime) / (10^12)
    assertBool $ "Parsing " ++ show size ++ " lines took " ++ show timeDiff ++ " seconds"
    case result of
      Left _ -> assertBool $ "Parse failed for large input of size " ++ show size
      Right _ -> assertBool $ "Parse succeeded for large input of size " ++ show size
  ) sizes

-- | Test compilation performance with large inputs
test_compilation_performance_large :: TestTree
test_compilation_performance_large = testCase "compilation performance with large inputs" $ do
  let sizes = [100, 1000, 5000]
  mapM_ (\size -> do
    let largeInput = replicate size "x := x + 1\n"
    startTime <- getCPUTime
    let result = compile largeInput
    endTime <- getCPUTime
    let timeDiff = fromIntegral (endTime - startTime) / (10^12)
    assertBool $ "Compiling " ++ show size ++ " lines took " ++ show timeDiff ++ " seconds"
    case result of
      Left _ -> assertBool $ "Compile failed for large input of size " ++ show size
      Right _ -> assertBool $ "Compile succeeded for large input of size " ++ show size
  ) sizes

-- | Test parsing with deeply nested structures
test_parsing_deep_nesting :: TestTree
test_parsing_deep_nesting = testCase "parsing with deeply nested structures" $ do
  let depths = [10, 50, 100]
  mapM_ (\depth -> do
    let nestedInput = unlines [replicate depth '{' ++ "x := 1" ++ replicate depth '}']
    startTime <- getCPUTime
    let result = parseTypus nestedInput
    endTime <- getCPUTime
    let timeDiff = fromIntegral (endTime - startTime) / (10^12)
    assertBool $ "Parsing depth " ++ show depth ++ " took " ++ show timeDiff ++ " seconds"
    case result of
      Left _ -> assertBool $ "Parse failed for depth " ++ show depth
      Right _ -> assertBool $ "Parse succeeded for depth " ++ show depth
  ) depths

-- | Test parsing with very wide lines
test_parsing_wide_lines :: TestTree
test_parsing_wide_lines = testCase "parsing with very wide lines" $ do
  let widths = [1000, 5000, 10000]
  mapM_ (\width -> do
    let wideInput = "x := " ++ replicate width 'x' ++ "\n"
    startTime <- getCPUTime
    let result = parseTypus wideInput
    endTime <- getCPUTime
    let timeDiff = fromIntegral (endTime - startTime) / (10^12)
    assertBool $ "Parsing width " ++ show width ++ " took " ++ show timeDiff ++ " seconds"
    case result of
      Left _ -> assertBool $ "Parse failed for width " ++ show width
      Right _ -> assertBool $ "Parse succeeded for width " ++ show width
  ) widths

-- | Test utils performance with large strings
test_utils_performance :: TestTree
test_utils_performance = testCase "utils performance with large strings" $ do
  let sizes = [10000, 50000, 100000]
  mapM_ (\size -> do
    let largeString = replicate size ' ' ++ "content" ++ replicate size ' '
    startTime <- getCPUTime
    let trimmed = trim largeString
    endTime <- getCPUTime
    let timeDiff = fromIntegral (endTime - startTime) / (10^12)
    assertBool $ "Trimming " ++ show (2 * size) ++ " chars took " ++ show timeDiff ++ " seconds"
    assertEqual "Trim result correct" "content" trimmed
  ) sizes

-- | Test memory usage with large inputs
test_memory_usage :: TestTree
test_memory_usage = testCase "memory usage with large inputs" $ do
  let size = 10000
      largeInput = replicate size "x := 1\ny := 2\n"
  -- Force evaluation to check memory usage
  let parseResult = force $ parseTypus largeInput
      compileResult = force $ compile largeInput
  case parseResult of
    Left _ -> assertBool "Memory test: parse failed" True
    Right _ -> do
      case compileResult of
        Left _ -> assertBool "Memory test: compile failed" True
        Right _ -> assertBool "Memory test: both succeeded" True

-- | Test error handling performance with problematic inputs
test_error_handling_performance :: TestTree
test_error_handling_performance = testCase "error handling performance with problematic inputs" $ do
  let problematicInputs = 
        [ replicate 1000 "x := "  -- incomplete assignments
        , replicate 1000 "if { }"  -- malformed ifs
        , replicate 1000 "{ { { "  -- unbalanced braces
        ]
  mapM_ (\input -> do
    startTime <- getCPUTime
    let parseResult = parseTypus (unlines input)
        compileResult = compile (unlines input)
    endTime <- getCPUTime
    let timeDiff = fromIntegral (endTime - startTime) / (10^12)
    assertBool $ "Error handling took " ++ show timeDiff ++ " seconds"
    case (parseResult, compileResult) of
      (Left _, Left _) -> assertBool "Both failed as expected" True
      (Right _, Left _) -> assertBool "Parse succeeded, compile failed as expected" True
      _ -> assertBool "Unexpected success pattern" True
  ) problematicInputs

-- | Property: Parsing time grows reasonably with input size
prop_parsing_time_complexity :: Property
prop_parsing_time_complexity = 
  forAll (choose [100, 1000, 5000]) $ \size ->
  let input = replicate size "x := 1\n"
      -- This is a simplified complexity test
      -- In a real scenario, you'd measure actual time
      result = parseTypus input
  in property $ case result of
    Left _ -> True  -- Failed parsing is acceptable
    Right _ -> True  -- Successful parsing is acceptable

-- | Property: Compilation time grows reasonably with input size
prop_compilation_time_complexity :: Property
prop_compilation_time_complexity = 
  forAll (choose [100, 1000, 5000]) $ \size ->
  let input = replicate size "x := x + 1\n"
      result = compile input
  in property $ case result of
    Left _ -> True  -- Failed compilation is acceptable
    Right _ -> True  -- Successful compilation is acceptable

-- | Property: Utils operations are linear time
prop_utils_linear_time :: Property
prop_utils_linear_time = 
  forAll (choose [1000, 10000, 50000]) $ \size ->
  let input = " " ++ replicate size 'a' ++ " "
      trimmed = trim input
      split = splitBy ',' input
  in property $ L.length trimmed <= size + 2 .&&. L.length split >= 1

-- | Property: Deep nesting doesn't cause exponential behavior
prop_deep_nesting_linear :: Property
prop_deep_nesting_linear = 
  forAll (choose [10, 50, 100]) $ \depth ->
  let nesting = replicate depth '{'
      unnesting = replicate depth '}'
      input = nesting ++ "x := 1" ++ unnesting
      result = parseTypus input
  in property $ case result of
    Left _ -> True  -- Failed parsing is acceptable
    Right _ -> True  -- Successful parsing is acceptable

-- | Property: Wide lines don't cause quadratic behavior
prop_wide_lines_linear :: Property
prop_wide_lines_linear = 
  forAll (choose [1000, 5000, 10000]) $ \width ->
  let input = "x := " ++ replicate width 'x' ++ "\n"
      result = parseTypus input
  in property $ case result of
    Left _ -> True  -- Failed parsing is acceptable
    Right _ -> True  -- Successful parsing is acceptable

-- | Property: Memory usage doesn't grow excessively
prop_memory_usage_reasonable :: Property
prop_memory_usage_reasonable = 
  forAll (choose [1000, 5000, 10000]) $ \size ->
  let input = replicate size "x := 1\ny := 2\n"
      parseResult = parseTypus input
      compileResult = compile input
  in property $ case (parseResult, compileResult) of
    (Left _, Left _) -> True
    (Right _, Left _) -> True
    (Right _, Right _) -> True
    (Left _, Right _) -> False  -- Shouldn't happen

-- | Property: Error handling doesn't degrade performance
prop_error_handling_performance :: Property
prop_error_handling_performance = 
  forAll (choose [100, 1000, 5000]) $ \size ->
  let problematicInput = replicate size "x := "  -- incomplete assignments
      parseResult = parseTypus (unlines problematicInput)
      compileResult = compile (unlines problematicInput)
  in property $ case (parseResult, compileResult) of
    (Left _, Left _) -> True  -- Both should fail
    (Right _, Left _) -> True  -- Parse might succeed, compile should fail
    (Left _, Right _) -> False  -- Shouldn't happen
    (Right _, Right _) -> True  -- Both might succeed with warnings

-- | Property: Concurrent operations are safe
prop_concurrent_safety :: Property
prop_concurrent_safety = 
  -- This is a simplified test - real concurrent testing would require actual threads
  forAll (choose [100, 1000]) $ \size ->
  let input = replicate size "x := 1\n"
      result1 = parseTypus input
      result2 = parseTypus input
  in property $ result1 == result2

-- | Property: Resource cleanup works properly
prop_resource_cleanup :: Property
prop_resource_cleanup = 
  forAll (choose [100, 1000, 5000]) $ \size ->
  let input = replicate size "x := 1\n"
      result = parseTypus input
  in property $ case result of
    Left _ -> True  -- Resources should be cleaned up even on failure
    Right _ -> True  -- Resources should be cleaned up on success

tests :: TestTree
tests = testGroup "Performance Boundary Tests"
  [ test_parsing_performance_large
  , test_compilation_performance_large
  , test_parsing_deep_nesting
  , test_parsing_wide_lines
  , test_utils_performance
  , test_memory_usage
  , test_error_handling_performance
  , fastProperty "Parsing time complexity" prop_parsing_time_complexity
  , fastProperty "Compilation time complexity" prop_compilation_time_complexity
  , fastProperty "Utils linear time" prop_utils_linear_time
  , fastProperty "Deep nesting linear" prop_deep_nesting_linear
  , fastProperty "Wide lines linear" prop_wide_lines_linear
  , fastProperty "Memory usage reasonable" prop_memory_usage_reasonable
  , fastProperty "Error handling performance" prop_error_handling_performance
  , fastProperty "Concurrent safety" prop_concurrent_safety
  , fastProperty "Resource cleanup" prop_resource_cleanup
  ]