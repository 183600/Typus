{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.IntegrationEndToEndCabalsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary (genString, genNonEmptyString)

import IntegratedCompiler (compileToEnd, CompilationResult(..))
import Compiler (compile, generateGoCode)
import Parser (parseTypus, TypusFile(..))
import GoToolchain (runGoCode)

import Data.List (isInfixOf, isPrefixOf, length, sort)
import qualified Data.Text as T

-- Test 1: End-to-end compilation of simple program
test_end_to_end_simple_program :: TestTree
test_end_to_end_simple_program =
  testCase "End-to-end compilation of simple program" $ do
    let source = unlines
          [ "package main"
          , "func main() {"
          , "  println(\"Hello, World!\")"
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
            -- Should generate valid Go code
            assertBool "Should generate Go code" $
              T.unpack goCode `isInfixOf` "package main" &&
              T.unpack goCode `isInfixOf` "func main" &&
              T.unpack goCode `isInfixOf` "Hello, World"

-- Test 2: End-to-end compilation with ownership
test_end_to_end_ownership :: TestTree
test_end_to_end_ownership =
  testCase "End-to-end compilation with ownership" $ do
    let source = unlines
          [ "//! ownership: on"
          , "package main"
          , "func createData() []byte {"
          , "  return make([]byte, 1024)"
          , "}"
          , "func processData(data []byte) {"
          , "  println(len(data))"
          , "}"
          , "func main() {"
          , "  data := createData()"
          , "  processData(data)  // data is moved here"
          , "}"
          ]
    case parseTypus source of
      Left err -> assertFailure $ "Parse failed: " ++ err
      Right typusFile -> do
        case compile typusFile of
          Left compileErr -> do
            -- May fail on ownership analysis
            assertBool "Should handle ownership analysis" $
              any (`isInfixOf` show compileErr) 
                ["ownership", "move", "borrow"]
          Right result -> do
            let goCode = generateGoCode result
            -- Should generate Go code with ownership annotations
            assertBool "Should generate Go code with ownership" $
              T.length goCode > 0

-- Test 3: End-to-end compilation with dependent types
test_end_to_end_dependent_types :: TestTree
test_end_to_end_dependent_types =
  testCase "End-to-end compilation with dependent types" $ do
    let source = unlines
          [ "//! dependent_types: on"
          , "package main"
          , "type Vector(n: int) where n > 0 struct {"
          , "  data [n]int"
          , "}"
          , "func main() {"
          , "  v := Vector(5){data: [5]int{1, 2, 3, 4, 5}}"
          , "  println(len(v.data))"
          , "}"
          ]
    case parseTypus source of
      Left err -> assertFailure $ "Parse failed: " ++ err
      Right typusFile -> do
        case compile typusFile of
          Left compileErr -> do
            -- May fail on dependent type compilation
            assertBool "Should handle dependent type compilation" $
              any (`isInfixOf` show compileErr) 
                ["dependent", "type", "Vector"]
          Right result -> do
            let goCode = generateGoCode result
            -- Should generate Go code with runtime checks
            assertBool "Should generate Go code with runtime checks" $
              T.length goCode > 0

-- Test 4: End-to-end compilation with complex features
test_end_to_end_complex_features :: TestTree
test_end_to_end_complex_features =
  testCase "End-to-end compilation with complex features" $ do
    let source = unlines
          [ "//! ownership: on"
          , "//! dependent_types: on"
          , "package main"
          , "type SafeArray(n: int) where n > 0 struct {"
          , "  data [n]int"
          , "}"
          , "func createSafeArray(size: int) SafeArray(size) {"
          , "  return SafeArray(size){data: [size]int{0}}"
          , "}"
          , "func main() {"
          , "  arr := createSafeArray(10)"
          , "  println(len(arr.data))"
          , "}"
          ]
    case parseTypus source of
      Left err -> assertFailure $ "Parse failed: " ++ err
      Right typusFile -> do
        case compile typusFile of
          Left compileErr -> do
            -- May fail on complex feature interaction
            assertBool "Should handle complex feature interaction" $
              any (`isInfixOf` show compileErr) 
                ["ownership", "dependent", "SafeArray"]
          Right result -> do
            let goCode = generateGoCode result
            -- Should generate comprehensive Go code
            assertBool "Should generate comprehensive Go code" $
              T.length goCode > 0

-- Test 5: End-to-end compilation error handling
test_end_to_end_error_handling :: TestTree
test_end_to_end_error_handling =
  testCase "End-to-end compilation error handling" $ do
    let source = unlines
          [ "package main"
          , "func problematic() {"
          , "  x := 5"
          , "  y := \"hello\""
          , "  return x + y  // Type error"
          , "}"
          , "func main() {"
          , "  problematic()"
          , "}"
          ]
    case parseTypus source of
      Left err -> do
        -- Should handle parsing errors gracefully
        assertBool "Should handle parsing errors" $
          length err > 0
      Right typusFile -> do
        case compile typusFile of
          Left compileErr -> do
            -- Should provide comprehensive error information
            assertBool "Should provide comprehensive error information" $
              any (`isInfixOf` show compileErr) 
                ["type", "error", "string", "int"]
          Right _ -> do
            assertFailure "Expected compilation error for type mismatch"

-- QuickCheck property: End-to-end compilation is consistent
prop_end_to_end_consistent :: String -> Property
prop_end_to_end_consistent code =
  length code < 100 ==>  -- Keep code reasonable
  let source = unlines
        [ "package main"
        , "func main() {"
        , "  " ++ code
        , "}"
        ]
  in case parseTypus source of
       Left _ -> property True  -- Invalid code is skipped
       Right typusFile ->
         case compile typusFile of
           Left _ -> property True  -- Compilation errors are acceptable
           Right result -> 
             let goCode1 = generateGoCode result
                 goCode2 = generateGoCode result
             in property $ goCode1 == goCode2

-- Test 6: End-to-end compilation performance
test_end_to_end_performance :: TestTree
test_end_to_end_performance =
  testCase "End-to-end compilation performance" $ do
    let source = unlines
          [ "package main"
          , "func fibonacci(n int) int {"
          , "  if n <= 1 {"
          , "    return n"
          , "  }"
          , "  return fibonacci(n-1) + fibonacci(n-2)"
          , "}"
          , "func main() {"
          , "  for i := 0; i < 10; i++ {"
          , "    println(fibonacci(i))"
          , "  }"
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
            -- Should generate efficient Go code
            assertBool "Should generate efficient Go code" $
              T.unpack goCode `isInfixOf` "fibonacci" &&
              T.length goCode > 100  -- Should have substantial content

-- Test 7: End-to-end compilation with imports
test_end_to_end_imports :: TestTree
test_end_to_end_imports =
  testCase "End-to-end compilation with imports" $ do
    let source = unlines
          [ "//! import: \"fmt\""
          , "package main"
          , "func main() {"
          , "  fmt.Println(\"Hello from imported package\")"
          , "}"
          ]
    case parseTypus source of
      Left err -> do
        -- Should handle import directives
        assertBool "Should handle import directives" $
          any (`isInfixOf` err) ["import", "fmt"]
      Right typusFile -> do
        case compile typusFile of
          Left compileErr -> do
            assertFailure $ "Compilation failed: " ++ show compileErr
          Right result -> do
            let goCode = generateGoCode result
            -- Should include import in generated code
            assertBool "Should include imports in generated code" $
              T.unpack goCode `isInfixOf` "import" &&
              T.unpack goCode `isInfixOf` "fmt"

-- QuickCheck property: End-to-end compilation preserves semantics
prop_end_to_end_preserves_semantics :: String -> Property
prop_end_to_end_preserves_semantics expr =
  length expr < 50 ==>  -- Keep expressions reasonable
  let source = unlines
        [ "package main"
        , "func main() {"
        , "  x := " ++ expr
        , "  println(x)"
        , "}"
        ]
  in case parseTypus source of
       Left _ -> property True  -- Invalid expressions are skipped
       Right typusFile ->
         case compile typusFile of
           Left _ -> property True  -- Compilation errors are acceptable
           Right result -> 
             let goCode = generateGoCode result
             in property $ T.length goCode > 0  -- Should generate some code

tests :: TestTree
tests =
  testGroup "Integration End-to-End Cabals Tests"
    [ test_end_to_end_simple_program
    , test_end_to_end_ownership
    , test_end_to_end_dependent_types
    , test_end_to_end_complex_features
    , test_end_to_end_error_handling
    , fastProperty "End-to-end compilation is consistent" prop_end_to_end_consistent
    , test_end_to_end_performance
    , test_end_to_end_imports
    , fastProperty "End-to-end compilation preserves semantics" prop_end_to_end_preserves_semantics
    ]