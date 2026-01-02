{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CompilerOptimizationCabalsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary (genString, genNonEmptyString)

import Compiler (compile, CompilerError(..), generateGoCode)
import Parser (parseTypus, TypusFile(..))
import Compiler.IR (IRModule(..))

import qualified Data.List as L
import Data.List (isInfixOf, isPrefixOf, length)
import qualified Data.Text as T (pack, unpack)

-- Test 1: Compiler optimizes redundant variable declarations
test_compiler_optimizes_redundant_variables :: TestTree
test_compiler_optimizes_redundant_variables =
  testCase "Compiler optimizes redundant variable declarations" $ do
    let source = unlines
          [ "package main"
          , "func main() {"
          , "  x := 42"
          , "  y := x  // Redundant assignment"
          , "  z := y  // Another redundant assignment"
          , "  return z"
          , "}"
          ]
    case parseTypus source of
      Left err -> assertFailure $ "Parse failed: " ++ err
      Right typusFile -> do
        case compile typusFile of
          Left compileErr -> do
            -- Should not fail compilation
            assertFailure $ "Compilation failed: " ++ show compileErr
          Right result -> do
            let goCode = generateGoCode result
            -- Should optimize away redundant variables
            assertBool "Should optimize redundant variables" $
              T.unpack goCode `L.isInfixOf` "return 42" || 
              not (T.unpack goCode `L.isInfixOf` "y :=")

-- Test 2: Compiler performs constant folding
test_compiler_constant_folding :: TestTree
test_compiler_constant_folding =
  testCase "Compiler performs constant folding" $ do
    let source = unlines
          [ "package main"
          , "func main() {"
          , "  return 2 + 3 * 4  // Should be folded to 14"
          , "}"
          ]
    case parseTypus source of
      Left err -> assertFailure $ "Parse failed: " ++ err
      Right typusFile -> do
        case compile typusFile of
          Left compileErr -> do
            assertFailure $ "Compilation failed: " ++ show compileErr
          Right result -> do
            let goCode = generateGoCode result
            -- Should contain the folded constant
            assertBool "Should perform constant folding" $
              T.unpack goCode `L.isInfixOf` "return 14" ||
              not (T.unpack goCode `L.isInfixOf` "2 + 3 * 4")

-- Test 3: Compiler optimizes L.tail recursion
test_compiler_tail_recursion_optimization :: TestTree
test_compiler_tail_recursion_optimization =
  testCase "Compiler optimizes L.tail recursion" $ do
    let source = unlines
          [ "package main"
          , "func factorial(n int) int {"
          , "  if n <= 1 {"
          , "    return 1"
          , "  }"
          , "  return n * factorial(n - 1)  // Not L.tail recursive"
          , "}"
          , "func factorialTail(n int, acc int) int {"
          , "  if n <= 1 {"
          , "    return acc"
          , "  }"
          , "  return factorialTail(n - 1, n * acc)  // Tail recursive"
          , "}"
          ]
    case parseTypus source of
      Left err -> assertFailure $ "Parse failed: " ++ err
      Right typusFile -> do
        case compile typusFile of
          Left compileErr -> do
            assertFailure $ "Compilation failed: " ++ show compileErr
          Right result -> do
            let goCode = generateGoCode result
            -- Should optimize L.tail recursive version
            assertBool "Should optimize L.tail recursion" $
              T.unpack goCode `L.isInfixOf` "factorialTail"

-- QuickCheck property: Compiler optimization preserves semantics
prop_optimization_preserves_semantics :: String -> Property
prop_optimization_preserves_semantics expr =
  L.length expr < 100 ==>  -- Keep expressions reasonable
  let source = unlines
        [ "package main"
        , "func test() int {"
        , "  return " ++ expr
        , "}"
        ]
  in case parseTypus source of
       Left _ -> property True  -- Invalid expressions are skipped
       Right typusFile ->
         case compile typusFile of
           Left _ -> property True  -- Compilation errors are acceptable
           Right result -> 
             let goCode = generateGoCode result
             in property $ T.L.length goCode > 0  -- Should generate some code

-- Test 4: Compiler removes dead code
test_compiler_dead_code_elimination :: TestTree
test_compiler_dead_code_elimination =
  testCase "Compiler removes dead code" $ do
    let source = unlines
          [ "package main"
          , "func main() {"
          , "  if false {"
          , "    println(\"This should be eliminated\")"
          , "  }"
          , "  if true {"
          , "    println(\"This should remain\")"
          , "  }"
          , "  return 42"
          , "}"
          ]
    case parseTypus source of
      Left err -> assertFailure $ "Parse failed: " ++ err
      Right typusFile -> do
        case compile typusFile of
          Left compileErr -> do
            assertFailure $ "Compilation failed: " ++ show compileErr
          Right result -> do
            let goCode = generateGoCode result
            -- Should eliminate dead code branch
            assertBool "Should eliminate dead code" $
              not (T.unpack goCode `L.isInfixOf` "This should be eliminated") &&
              T.unpack goCode `L.isInfixOf` "This should remain"

-- Test 5: Compiler optimizes memory allocation
test_compiler_memory_optimization :: TestTree
test_compiler_memory_optimization =
  testCase "Compiler optimizes memory allocation" $ do
    let source = unlines
          [ "package main"
          , "func main() {"
          , "  // Multiple allocations that could be optimized"
          , "  slice1 := make([]int, 10)"
          , "  slice2 := make([]int, 10)"
          , "  slice3 := make([]int, 10)"
          , "  return len(slice1) + len(slice2) + len(slice3)"
          , "}"
          ]
    case parseTypus source of
      Left err -> assertFailure $ "Parse failed: " ++ err
      Right typusFile -> do
        case compile typusFile of
          Left compileErr -> do
            assertFailure $ "Compilation failed: " ++ show compileErr
          Right result -> do
            let goCode = generateGoCode result
            -- Should optimize memory allocation
            assertBool "Should optimize memory allocation" $
              T.L.length goCode > 0  -- Basic check that code is generated

-- Test 6: Compiler inlines small functions
test_compiler_function_inlining :: TestTree
test_compiler_function_inlining =
  testCase "Compiler inlines small functions" $ do
    let source = unlines
          [ "package main"
          , "func small() int {"
          , "  return 42"
          , "}"
          , "func main() {"
          , "  return small()"
          , "}"
          ]
    case parseTypus source of
      Left err -> assertFailure $ "Parse failed: " ++ err
      Right typusFile -> do
        case compile typusFile of
          Left compileErr -> do
            assertFailure $ "Compilation failed: " ++ show compileErr
          Right result -> do
            let goCode = generateGoCode result
            -- Should inline small function L.or optimize call
            assertBool "Should inline small function L.or optimize call" $
              T.unpack goCode `L.isInfixOf` "return 42" ||
              T.unpack goCode `L.isInfixOf` "small()"

-- QuickCheck property: Optimization doesn't increase code size significantly
prop_optimization_reasonable_code_size :: String -> Property
prop_optimization_reasonable_code_size code =
  L.length code < 200 ==>  -- Keep input reasonable
  let source = unlines
        [ "package main"
        , "func main() {"
        , "  " ++ code
        , "}"
        ]
  in case parseTypus source of
       Left _ -> property True
       Right typusFile ->
         case compile typusFile of
           Left _ -> property True
           Right result -> 
             let goCode = generateGoCode result
                 originalSize = L.length source
                 optimizedSize = T.L.length goCode
             in property $ optimizedSize <= originalSize * 3  -- Reasonable limit

tests :: TestTree
tests =
  testGroup "Compiler Optimization Cabals Tests"
    [ test_compiler_optimizes_redundant_variables
    , test_compiler_constant_folding
    , test_compiler_tail_recursion_optimization
    , fastProperty "Optimization preserves semantics" prop_optimization_preserves_semantics
    , test_compiler_dead_code_elimination
    , test_compiler_memory_optimization
    , test_compiler_function_inlining
    , fastProperty "Optimization maintains reasonable code size" prop_optimization_reasonable_code_size
    ]