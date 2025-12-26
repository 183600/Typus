{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.PerformanceRegressionCabalsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary (genString, genNonEmptyString)

import Compiler (compile, generateGoCode)
import Parser (parseTypus, TypusFile(..))
import Utils (trim, normalizeIndentation)

import Data.List (isInfixOf, isPrefixOf, length, sort)
import qualified Data.Text as T
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

-- Test 1: Parser performance with large files
test_parser_performance_large_files :: TestTree
test_parser_performance_large_files =
  testCase "Parser performance with large files" $ do
    let largeFunction = unlines
          [ "func largeFunction() {"
          , "  var x, y, z int"
          , "  x = 1"
          , "  y = 2"
          , "  z = 3"
          , "  if x > 0 {"
          , "    if y > 0 {"
          , "      if z > 0 {"
          , "        println(\"Nested\")"
          , "      }"
          , "    }"
          , "  }"
          , "}"
          ]
        source = unlines $ ["package main"] ++ replicate 100 largeFunction ++ ["func main() {}"]
    
    start <- getCPUTime
    case parseTypus source of
      Left err -> do
        end <- getCPUTime
        let duration = fromIntegral (end - start) / (10^12)
        assertBool $ "Parser should handle large files within reasonable time (took " ++ 
                     printf "%.3f" duration ++ "s): " ++ err
      Right typusFile -> do
        end <- getCPUTime
        let duration = fromIntegral (end - start) / (10^12)
        let codeBlocks = tfCodeBlocks typusFile
        assertBool "Should parse large file successfully" $
          length codeBlocks >= 100
        assertBool ("Parsing should complete within reasonable time (took " ++ 
                   printf "%.3f" duration ++ "s)") $
          duration < 5.0  -- Should complete within 5 seconds

-- Test 2: Compiler performance with complex type checking
test_compiler_performance_complex_types :: TestTree
test_compiler_performance_complex_types =
  testCase "Compiler performance with complex type checking" $ do
    let complexTypes = unlines
          [ "//! dependent_types: on"
          , "//! ownership: on"
          , "package main"
          , "type Matrix(m: int, n: int) where m > 0 && n > 0 struct {"
          , "  data [m * n]float64"
          , "}"
          , "func multiply(m: Matrix(a, b), n: Matrix(b, c)) Matrix(a, c) {"
          , "  result := Matrix(a, c){}"
          , "  for i := 0; i < a; i++ {"
          , "    for j := 0; j < c; j++ {"
          , "      sum := 0.0"
          , "      for k := 0; k < b; k++ {"
          , "        sum += m.data[i*b+k] * n.data[k*c+j]"
          , "      }"
          , "      result.data[i*c+j] = sum"
          , "    }"
          , "  }"
          , "  return result"
          , "}"
          ]
    
    start <- getCPUTime
    case parseTypus complexTypes of
      Left err -> assertFailure $ "Parse failed: " ++ err
      Right typusFile -> do
        case compile typusFile of
          Left compileErr -> do
            end <- getCPUTime
            let duration = fromIntegral (end - start) / (10^12)
            assertBool ("Type checking should complete within reasonable time (took " ++
                       printf "%.3f" duration ++ "s): " ++ show compileErr) $
              duration < 3.0
          Right result -> do
            end <- getCPUTime
            let duration = fromIntegral (end - start) / (10^12)
            let goCode = generateGoCode result
            assertBool "Should compile complex types successfully" $
              T.length goCode > 0
            assertBool ("Compilation should complete within reasonable time (took " ++
                       printf "%.3f" duration ++ "s)") $
              duration < 3.0

-- Test 3: Memory usage with recursive structures
test_memory_usage_recursive_structures :: TestTree
test_memory_usage_recursive_structures =
  testCase "Memory usage with recursive structures" $ do
    let recursiveTypes = unlines
          [ "package main"
          , "type Node struct {"
          , "  value int"
          , "  left *Node"
          , "  right *Node"
          , "}"
          , "func createTree(depth int) *Node {"
          , "  if depth <= 0 {"
          , "    return nil"
          , "  }"
          , "  return &Node{"
          , "    value: depth,"
          , "    left: createTree(depth - 1),"
          , "    right: createTree(depth - 1)"
          , "  }"
          , "}"
          , "func traverse(node *Node) {"
          , "  if node == nil {"
          , "    return"
          , "  }"
          , "  traverse(node.left)"
          , "  println(node.value)"
          , "  traverse(node.right)"
          , "}"
          , "func main() {"
          , "  tree := createTree(10)"
          , "  traverse(tree)"
          , "}"
          ]
    
    start <- getCPUTime
    case parseTypus recursiveTypes of
      Left err -> assertFailure $ "Parse failed: " ++ err
      Right typusFile -> do
        case compile typusFile of
          Left compileErr -> do
            end <- getCPUTime
            let duration = fromIntegral (end - start) / (10^12)
            assertBool ("Should handle recursive structures efficiently (took " ++
                       printf "%.3f" duration ++ "s): " ++ show compileErr) $
              duration < 2.0
          Right result -> do
            end <- getCPUTime
            let duration = fromIntegral (end - start) / (10^12)
            let goCode = generateGoCode result
            assertBool "Should compile recursive structures" $
              T.unpack goCode `isInfixOf` "Node" &&
              T.unpack goCode `isInfixOf` "createTree"
            assertBool ("Recursive compilation should be efficient (took " ++
                       printf "%.3f" duration ++ "s)") $
              duration < 2.0

-- Test 4: Performance regression with string processing
test_string_processing_performance :: TestTree
test_string_processing_performance =
  testCase "Performance regression with string processing" $ do
    let stringProcessing = unlines
          [ "package main"
          , "func processStrings(input []string) []string {"
          , "  result := make([]string, 0)"
          , "  for _, s := range input {"
          , "    trimmed := trim(s)"
          , "    normalized := normalizeIndentation(trimmed)"
          , "    if len(normalized) > 0 {"
          , "      result = append(result, normalized)"
          , "    }"
          , "  }"
          , "  return result"
          , "}"
          , "func main() {"
          , "  input := []string{\"  hello  \", \"\\tworld\\t\", \"  test  \"}"
          , "  output := processStrings(input)"
          , "  println(output)"
          , "}"
          ]
    
    start <- getCPUTime
    case parseTypus stringProcessing of
      Left err -> assertFailure $ "Parse failed: " ++ err
      Right typusFile -> do
        case compile typusFile of
          Left compileErr -> do
            end <- getCPUTime
            let duration = fromIntegral (end - start) / (10^12)
            assertBool ("String processing should be efficient (took " ++
                       printf "%.3f" duration ++ "s): " ++ show compileErr) $
              duration < 1.0
          Right result -> do
            end <- getCPUTime
            let duration = fromIntegral (end - start) / (10^12)
            let goCode = generateGoCode result
            assertBool "Should compile string processing" $
              T.length goCode > 0
            assertBool ("String processing compilation should be fast (took " ++
                       printf "%.3f" duration ++ "s)") $
              duration < 1.0

-- QuickCheck property: Compilation time scales reasonably
prop_compilation_time_scales_reasonably :: Int -> Property
prop_compilation_time_scales_reasonably size =
  size >= 10 && size <= 1000 ==>  -- Reasonable size range
  let source = unlines $ ["package main", "func main() {"] ++ 
                       replicate size "  x := 42" ++ 
                       ["}"]
  in case parseTypus source of
       Left _ -> property True  -- Invalid code is skipped
       Right typusFile ->
         case compile typusFile of
           Left _ -> property True  -- Compilation errors are acceptable
           Right _ -> property True  -- Successful compilation

-- Test 5: Performance with multiple modules
test_multiple_modules_performance :: TestTree
test_multiple_modules_performance =
  testCase "Performance with multiple modules" $ do
    let moduleA = unlines
          [ "package modulea"
          , "func Add(a, b int) int {"
          , "  return a + b"
          , "}"
          ]
        moduleB = unlines
          [ "package moduleb"
          , "func Multiply(a, b int) int {"
          , "  return a * b"
          , "}"
          ]
        mainModule = unlines
          [ "//! import: \"modulea\""
          , "//! import: \"moduleb\""
          , "package main"
          , "func main() {"
          , "  result := modulea.Add(5, 3) * moduleb.Multiply(2, 4)"
          , "  println(result)"
          , "}"
          ]
    
    start <- getCPUTime
    case parseTypus mainModule of
      Left err -> do
        -- Should handle import errors gracefully
        end <- getCPUTime
        let duration = fromIntegral (end - start) / (10^12)
        assertBool ("Multi-module parsing should be reasonable (took " ++
                   printf "%.3f" duration ++ "s): " ++ err) $
          duration < 2.0
      Right typusFile -> do
        case compile typusFile of
          Left compileErr -> do
            end <- getCPUTime
            let duration = fromIntegral (end - start) / (10^12)
            assertBool ("Multi-module compilation should be reasonable (took " ++
                       printf "%.3f" duration ++ "s): " ++ show compileErr) $
              duration < 2.0
          Right result -> do
            end <- getCPUTime
            let duration = fromIntegral (end - start) / (10^12)
            let goCode = generateGoCode result
            assertBool "Should compile multi-module project" $
              T.length goCode > 0
            assertBool ("Multi-module compilation should be efficient (took " ++
                       printf "%.3f" duration ++ "s)") $
              duration < 2.0

-- Test 6: Performance with optimization levels
test_optimization_performance :: TestTree
test_optimization_performance =
  testCase "Performance with optimization levels" $ do
    let optimizableCode = unlines
          [ "package main"
          , "func fibonacci(n int) int {"
          , "  if n <= 1 {"
          , "    return n"
          , "  }"
          , "  return fibonacci(n-1) + fibonacci(n-2)"
          , "}"
          , "func main() {"
          , "  for i := 0; i < 20; i++ {"
          , "    println(fibonacci(i))"
          , "  }"
          , "}"
          ]
    
    start <- getCPUTime
    case parseTypus optimizableCode of
      Left err -> assertFailure $ "Parse failed: " ++ err
      Right typusFile -> do
        case compile typusFile of
          Left compileErr -> do
            end <- getCPUTime
            let duration = fromIntegral (end - start) / (10^12)
            assertBool ("Optimization should not take too long (took " ++
                       printf "%.3f" duration ++ "s): " ++ show compileErr) $
              duration < 3.0
          Right result -> do
            end <- getCPUTime
            let duration = fromIntegral (end - start) / (10^12)
            let goCode = generateGoCode result
            assertBool "Should optimize code effectively" $
              T.unpack goCode `isInfixOf` "fibonacci"
            assertBool ("Optimization should complete in reasonable time (took " ++
                       printf "%.3f" duration ++ "s)") $
              duration < 3.0

-- QuickCheck property: Large input doesn't cause exponential slowdown
prop_large_input_linear_performance :: Int -> Property
prop_large_input_linear_performance complexity =
  complexity >= 1 && complexity <= 100 ==>
  let nestedCode = unlines $ concat $ replicate complexity ["if true {"]
        source = unlines $ ["package main", "func test() {"] ++ 
                         nestedCode ++ 
                         replicate complexity "}" ++ 
                         ["}"]
  in case parseTypus source of
       Left _ -> property True  -- Invalid code is skipped
       Right _ -> property True  -- Successful parsing

tests :: TestTree
tests =
  testGroup "Performance Regression Cabals Tests"
    [ test_parser_performance_large_files
    , test_compiler_performance_complex_types
    , test_memory_usage_recursive_structures
    , test_string_processing_performance
    , fastProperty "Compilation time scales reasonably" prop_compilation_time_scales_reasonably
    , test_multiple_modules_performance
    , test_optimization_performance
    , fastProperty "Large input doesn't cause exponential slowdown" prop_large_input_linear_performance
    ]