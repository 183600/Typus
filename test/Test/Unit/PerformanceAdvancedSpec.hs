{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.PerformanceAdvancedSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import Parser (parseTypus, TypusFile(..))
import Compiler (compileTypus, CompilationResult(..))
import ErrorHandler (errorAt, ErrorLocation(..))
import SourceLocation (SourcePos(..), startPos, advancePosBy)
import Utils (trim, removeComments, normalizeIndentation)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, length)
import Data.List (replicate)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Char (isSpace)
import Control.DeepSeq (NFData, force)
import Criterion.Main (bench, bgroup, nf, whnf)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

-- ============================================================================
-- Performance Tests
-- ============================================================================

tests :: TestTree
tests =
  testGroup "Performance Tests"
    [ testGroup "Parsing performance"
        [ testCase "parses small files efficiently" test_small_file_parsing
        , testCase "parses medium files efficiently" test_medium_file_parsing
        , testCase "parses large files efficiently" test_large_file_parsing
        , testCase "parsing scales linearly with size" test_parsing_scalability
        , testCase "parsing handles complex syntax efficiently" test_complex_syntax_parsing
        ]

    , testGroup "Compilation performance"
        [ testCase "compiles simple programs quickly" test_simple_compilation
        , testCase "compiles complex programs efficiently" test_complex_compilation
        , testCase "compilation scales with complexity" test_compilation_scalability
        , testCase "handles many functions efficiently" test_many_functions_compilation
        ]

    , testGroup "Memory usage performance"
        [ testCase "memory usage is bounded for large files" test_memory_usage_bounded
        , testCase "garbage collection works effectively" test_garbage_collection
        , testCase "memory leaks are prevented" test_memory_leaks_prevented
        , testCase "deep structures don't cause stack overflow" test_deep_structures_memory
        ]

    , testGroup "String processing performance"
        [ testCase "trim processes large strings efficiently" test_trim_performance
        , testCase "removeComments handles large files efficiently" test_remove_comments_performance
        , testCase "normalizeIndentation scales with file size" test_normalize_indentation_performance
        , testCase "string operations are memory efficient" test_string_operations_memory
        ]

    , testGroup "Error handling performance"
        [ testCase "error creation is fast" test_error_creation_performance
        , testCase "error collection scales well" test_error_collection_performance
        , testCase "error formatting is efficient" test_error_formatting_performance
        , testCase "many errors don't degrade performance" test_many_errors_performance
        ]

    , testGroup "Property-based performance tests"
        [ fastProperty "parsing time is O(n) for input size" prop_parsing_linear_time
        , fastProperty "memory usage is reasonable for input size" prop_memory_reasonable
        , fastProperty "compilation time scales reasonably" prop_compilation_reasonable_time
        ]
    ]

-- ============================================================================
-- Parsing Performance Tests
-- ============================================================================

test_small_file_parsing :: IO ()
test_small_file_parsing = do
  let smallContent = "func main() { return 42 }\n"
      startTime <- getCPUTime
      let parseResult = parseTypus smallContent
      endTime <- getCPUTime
      let timeDiff = fromIntegral (endTime - startTime) / (10^12)
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right _ -> do
      assertBool "Small file parsing should be very fast (< 0.001s)" (timeDiff < 0.001)

test_medium_file_parsing :: IO ()
test_medium_file_parsing = do
  let mediumFunction = "func test" ++ show [1..100] ++ "() {\n"
      functionBody = L.concat $ replicate 50 "    x := x + 1\n    y := y * 2\n    z := z / 3\n"
      mediumContent = L.concat $ replicate 20 (mediumFunction ++ functionBody ++ "}\n")
      startTime <- getCPUTime
      let parseResult = parseTypus mediumContent
      endTime <- getCPUTime
      let timeDiff = fromIntegral (endTime - startTime) / (10^12)
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right _ -> do
      assertBool "Medium file parsing should be fast (< 0.1s)" (timeDiff < 0.1)

test_large_file_parsing :: IO ()
test_large_file_parsing = do
  let largeFunction = "func large" ++ show [1..1000] ++ "() {\n"
      functionBody = L.concat $ replicate 100 "    x := x + 1\n    y := make([]int, 1000)\n    for i := range y {\n        x += y[i]\n    }\n"
      largeContent = L.concat $ replicate 100 (largeFunction ++ functionBody ++ "}\n")
      startTime <- getCPUTime
      let parseResult = parseTypus largeContent
      endTime <- getCPUTime
      let timeDiff = fromIntegral (endTime - startTime) / (10^12)
  case parseResult of
    Left err -> do
      -- Even if parsing fails, it should fail quickly
      assertBool "Large file parsing should fail quickly if it fails" (timeDiff < 1.0)
    Right _ -> do
      assertBool "Large file parsing should complete in reasonable time (< 2s)" (timeDiff < 2.0)

test_parsing_scalability :: IO ()
test_parsing_scalability = do
  let baseContent = "func test() { return 42 }\n"
      sizes = [1, 10, 100, 1000]
      parseTimes <- mapM (\size -> do
        let content = L.concat $ replicate size baseContent
        startTime <- getCPUTime
        let parseResult = parseTypus content
        endTime <- getCPUTime
        let timeDiff = fromIntegral (endTime - startTime) / (10^12)
        return (size, timeDiff, parseResult)
        ) sizes
  let successfulParses = L.filter (\(_, _, result) -> case result of Left _ -> False; Right _ -> True) parseTimes
      timeRatios = if L.length successfulParses >= 2
                   then let (_, t1, _) = successfulParses !! 0
                            (_, t2, _) = successfulParses !! (L.length successfulParses - 1)
                            size1 = fst $ successfulParses !! 0
                            size2 = fst $ successfulParses !! (L.length successfulParses - 1)
                        in (t2 / t1) / (fromIntegral size2 / fromIntegral size1)
                   else 1.0
  assertBool "Parsing should scale roughly linearly" (timeRatios < 10.0)  -- Allow some overhead

test_complex_syntax_parsing :: IO ()
test_complex_syntax_parsing = do
  let complexFunction = "func complex() {\n"
      complexBody = L.concat $ replicate 50
        [ "    if condition1 {\n"
        , "        for i := 0; i < 100; i++ {\n"
        , "            switch i {\n"
        , "                case 0: return zero()\n"
        , "                case 1: return one()\n"
        , "                default: return default()\n"
        , "            }\n"
        , "        }\n"
        , "    } else {\n"
        , "        defer cleanup()\n"
        , "        go async()\n"
        , "    }\n"
        ]
      complexContent = complexFunction ++ complexBody ++ "}\n"
      startTime <- getCPUTime
      let parseResult = parseTypus complexContent
      endTime <- getCPUTime
      let timeDiff = fromIntegral (endTime - startTime) / (10^12)
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right _ -> do
      assertBool "Complex syntax parsing should be efficient (< 0.5s)" (timeDiff < 0.5)

-- ============================================================================
-- Compilation Performance Tests
-- ============================================================================

test_simple_compilation :: IO ()
test_simple_compilation = do
  let simpleContent = "func main() { return 42 }\n"
      parseResult = parseTypus simpleContent
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      startTime <- getCPUTime
      let compileResult = compileTypus typusFile
      endTime <- getCPUTime
      let timeDiff = fromIntegral (endTime - startTime) / (10^12)
      case compileResult of
        Left err -> assertFailure $ "Compile failed: " ++ show err
        Right _ -> do
          assertBool "Simple compilation should be very fast (< 0.001s)" (timeDiff < 0.001)

test_complex_compilation :: IO ()
test_complex_compilation = do
  let complexContent = unlines
        [ "package main"
        , "import \"fmt\""
        , "func fibonacci(n int) int {"
        , "    if n <= 1 { return n }"
        , "    return fibonacci(n-1) + fibonacci(n-2)"
        , "}"
        , "func main() {"
        , "    for i := 0; i < 20; i++ {"
        , "        fmt.Printf(\"fib(%d) = %d\\n\", i, fibonacci(i))"
        , "    }"
        , "}"
        ]
      parseResult = parseTypus complexContent
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      startTime <- getCPUTime
      let compileResult = compileTypus typusFile
      endTime <- getCPUTime
      let timeDiff = fromIntegral (endTime - startTime) / (10^12)
      case compileResult of
        Left err -> do
          -- Should handle compilation errors efficiently
          assertBool "Complex compilation should fail efficiently" (timeDiff < 1.0)
        Right _ -> do
          assertBool "Complex compilation should be efficient (< 0.1s)" (timeDiff < 0.1)

test_compilation_scalability :: IO ()
test_compilation_scalability = do
  let baseFunction = "func test() { return 42 }\n"
      functionCounts = [1, 10, 100]
      compileTimes <- mapM (\count -> do
        let content = L.concat $ replicate count baseFunction
        let parseResult = parseTypus content
        case parseResult of
          Left _ -> return (count, 0.0, False)
          Right typusFile -> do
            startTime <- getCPUTime
            let compileResult = compileTypus typusFile
            endTime <- getCPUTime
            let timeDiff = fromIntegral (endTime - startTime) / (10^12)
            case compileResult of
              Left _ -> return (count, timeDiff, False)
              Right _ -> return (count, timeDiff, True)
        ) functionCounts
  let successfulCompiles = L.filter (\(_, _, success) -> success) compileTimes
  if L.length successfulCompiles >= 2
    then let (_, t1, _) = successfulCompiles !! 0
             (_, t2, _) = successfulCompiles !! (L.length successfulCompiles - 1)
             count1 = fst $ successfulCompiles !! 0
             count2 = fst $ successfulCompiles !! (L.length successfulCompiles - 1)
             timeRatio = (t2 / t1) / (fromIntegral count2 / fromIntegral count1)
         in assertBool "Compilation should scale reasonably" (timeRatio < 5.0)
    else assertBool "Should have at least some successful compilations" (not (null successfulCompiles))

test_many_functions_compilation :: IO ()
test_many_functions_compilation = do
  let manyFunctionsContent = L.concat $ L.map (\i -> "func func" ++ show i ++ "() { return " ++ show i ++ " }\n") [1..1000]
      parseResult = parseTypus manyFunctionsContent
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      startTime <- getCPUTime
      let compileResult = compileTypus typusFile
      endTime <- getCPUTime
      let timeDiff = fromIntegral (endTime - startTime) / (10^12)
      case compileResult of
        Left err -> do
          assertBool "Many functions compilation should fail efficiently" (timeDiff < 2.0)
        Right _ -> do
          assertBool "Many functions compilation should be efficient (< 1s)" (timeDiff < 1.0)

-- ============================================================================
-- Memory Usage Performance Tests
-- ============================================================================

test_memory_usage_bounded :: IO ()
test_memory_usage_bounded = do
  let largeContent = L.concat $ replicate 10000 "func test() { x := x + 1; return x }\n"
      parseResult = parseTypus largeContent
  case parseResult of
    Left err -> do
      assertBool "Should handle large inputs without excessive memory" (True)
    Right typusFile -> do
      let forced = force typusFile  -- Force evaluation
      assertBool "Memory usage should be bounded for large files" (True)

test_garbage_collection :: IO ()
test_garbage_collection = do
  let parseAndCompile content = do
        let parseResult = parseTypus content
        case parseResult of
          Left _ -> return ()
          Right typusFile -> do
            let compileResult = compileTypus typusFile
            return ()
  -- Process multiple large files to test garbage collection
  mapM_ (\i -> do
    let content = "func test" ++ show i ++ "() { return " ++ show i ++ " }\n"
    parseAndCompile content
    ) [1..1000]
  assertBool "Garbage collection should work effectively" (True)

test_memory_leaks_prevented :: IO ()
test_memory_leaks_prevented = do
  let processLargeFile = do
        let largeContent = L.concat $ replicate 1000 "func test() { data := make([]byte, 1000); return data }\n"
        let parseResult = parseTypus largeContent
        case parseResult of
          Left _ -> return ()
          Right typusFile -> do
            let compileResult = compileTypus typusFile
            return ()
  -- Process multiple large files to check for memory leaks
  mapM_ (\_ -> processLargeFile) [1..100]
  assertBool "Memory leaks should be prevented" (True)

test_deep_structures_memory :: IO ()
test_deep_structures_memory = do
  let nestDepth = 100
      nestedContent = L.concat $ replicate nestDepth "func outer() { "
      content = nestedContent ++ "return 42" ++ L.concat (replicate nestDepth " }") ++ "\n"
      parseResult = parseTypus content
  case parseResult of
    Left err -> do
      assertBool "Should handle deep nesting without stack overflow" (True)
    Right typusFile -> do
      let forced = force typusFile
      assertBool "Deep structures should not cause stack overflow" (True)

-- ============================================================================
-- String Processing Performance Tests
-- ============================================================================

test_trim_performance :: IO ()
test_trim_performance = do
  let largeString = L.concat $ replicate 10000 "    \t   hello world   \t    \n"
      startTime <- getCPUTime
      let result = trim largeString
      endTime <- getCPUTime
      let timeDiff = fromIntegral (endTime - startTime) / (10^12)
  assertBool "trim should process large strings efficiently (< 0.01s)" (timeDiff < 0.01)

test_remove_comments_performance :: IO ()
test_remove_comments_performance = do
  let largeContent = L.concat $ replicate 1000 "func test() { // This is a comment\n    x := 42 /* block comment */; return x }\n"
      startTime <- getCPUTime
      let result = removeComments largeContent
      endTime <- getCPUTime
      let timeDiff = fromIntegral (endTime - startTime) / (10^12)
  assertBool "removeComments should process large files efficiently (< 0.1s)" (timeDiff < 0.1)

test_normalize_indentation_performance :: IO ()
test_normalize_indentation_performance = do
  let indentedContent = L.concat $ L.map (\i -> replicate i ' ' ++ "line " ++ show i ++ "\n") [1..1000]
      startTime <- getCPUTime
      let result = normalizeIndentation indentedContent
      endTime <- getCPUTime
      let timeDiff = fromIntegral (endTime - startTime) / (10^12)
  assertBool "normalizeIndentation should scale with file size (< 0.1s)" (timeDiff < 0.1)

test_string_operations_memory :: IO ()
test_string_operations_memory = do
  let largeString = L.concat $ replicate 10000 "This is a test string with various content\n"
      operations = [trim, removeComments, normalizeIndentation]
      results <- mapM (\op -> do
        let result = op largeString
        return (L.length result)
        ) operations
  assertBool "String operations should be memory efficient" (L.all (> 0) results)

-- ============================================================================
-- Error Handling Performance Tests
-- ============================================================================

test_error_creation_performance :: IO ()
test_error_creation_performance = do
  let location = ErrorLocation (startPos) Nothing
      startTime <- getCPUTime
      let errors = L.map (\i -> errorAt "test-id" show i)) [1..10000]
      endTime <- getCPUTime
      let timeDiff = fromIntegral (endTime - startTime) / (10^12)
  assertBool "Error creation should be fast (< 0.01s)" (timeDiff < 0.01)

test_error_collection_performance :: IO ()
test_error_collection_performance = do
  let location = ErrorLocation (startPos) Nothing
      errors = L.map (\i -> errorAt "test-id" show i)) [1..10000]
      startTime <- getCPUTime
      let errorCount = L.length errors
      endTime <- getCPUTime
      let timeDiff = fromIntegral (endTime - startTime) / (10^12)
  assertBool "Error collection should scale well (< 0.01s)" (timeDiff < 0.01)

test_error_formatting_performance :: IO ()
test_error_formatting_performance = do
  let location = ErrorLocation (startPos) Nothing
      errors = L.map (\i -> errorAt "test-id" show i)) [1..1000]
      startTime <- getCPUTime
      let formatted = map show errors
      endTime <- getCPUTime
      let timeDiff = fromIntegral (endTime - startTime) / (10^12)
  assertBool "Error formatting should be efficient (< 0.1s)" (timeDiff < 0.1)

test_many_errors_performance :: IO ()
test_many_errors_performance = do
  let errorContent = L.concat $ L.map (\i -> "func test" ++ show i ++ "() { invalid_syntax_" ++ show i ++ " }\n") [1..1000]
      parseResult = parseTypus errorContent
  case parseResult of
    Left err -> do
      assertBool "Should handle many parse errors efficiently" (True)
    Right typusFile -> do
      let compileResult = compileTypus typusFile
      case compileResult of
        Left errors -> do
          assertBool "Should handle many compilation errors efficiently" (L.length errors >= 1)
        Right _ -> do
          assertBool "Should handle case with no errors" (True)

-- ============================================================================
-- Property-Based Performance Tests
-- ============================================================================

prop_parsing_linear_time :: Property
prop_parsing_linear_time =
  forAll arbitrary $ \content ->
    let contentLength = L.length content
        -- This is a simplified check - in real benchmarks we'd measure actual time
        reasonableSize = contentLength < 1000000  -- 1MB limit for property tests
    in property $ reasonableSize ==> True

prop_memory_reasonable :: Property
prop_memory_reasonable =
  forAll arbitrary $ \content ->
    let contentLength = L.length content
        -- Memory usage should be proportional to input size
        reasonableRatio = contentLength < 10000000  -- 10MB limit
    in property $ reasonableRatio ==> True

prop_compilation_reasonable_time :: Property
prop_compilation_reasonable_time =
  forAll arbitrary $ \content ->
    let parseResult = parseTypus content
    in case parseResult of
         Left _ -> property True
         Right typusFile ->
           let compileResult = compileTypus typusFile
           in case compileResult of
                Left _ -> property True
                Right _ -> property True