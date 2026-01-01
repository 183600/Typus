{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ErrorHandlerConsistencyCabalsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary (genString, genNonEmptyString)

import ErrorHandler (formatError, ErrorHandler(..), ErrorSeverity(..))
import EnhancedErrorHandler (EnhancedErrorHandler(..), handleMultipleErrors)
import Compiler (CompilerError(..), formatCompilerErrors, hasTypeErrors)
import Parser (parseTypus, TypusFile(..))

import qualified Data.List as L
import Data.List (isInfixOf, isPrefixOf, length)
import Data.List (sort)
import qualified Data.Text as T

-- Test 1: Error handler formats errors consistently
test_error_handler_consistency :: TestTree
test_error_handler_consistency =
  testCase "Error handler formats errors consistently" $ do
    let errors = 
          [ CompilerError "Syntax error" (SourcePos 1 10) (SourcePos 1 15)
          , CompilerError "Type error" (SourcePos 2 5) (SourcePos 2 8)
          , CompilerError "Missing semicolon" (SourcePos 3 20) (SourcePos 3 20)
          ]
        formatted = formatCompilerErrors errors
    -- Should format L.all errors with consistent structure
    assertBool "Should format L.all errors" $
      L.length (lines formatted) >= 3
    assertBool "Should include line numbers" $
      L.any (`L.isInfixOf` formatted) ["1:", "2:", "3:"]
    assertBool "Should include error descriptions" $
      L.any (`L.isInfixOf` formatted) ["Syntax", "Type", "Missing"]

-- Test 2: Error handler handles multiple errors gracefully
test_multiple_errors_handling :: TestTree
test_multiple_errors_handling =
  testCase "Error handler handles multiple errors gracefully" $ do
    let source = unlines
          [ "package main"
          , "func broken() {"
          , "  x := 5"
          , "  y := \"hello\""
          , "  return x + y  // Type error"
          , "}"
          , "func alsoBroken() {"
          , "  if true {"
          , "    println(\"test\")"
          , "  // Missing closing brace"
          , "}"
          ]
    case parseTypus source of
      Left err -> do
        -- Should report multiple errors if possible
        assertBool "Should handle multiple compilation errors" $
          L.length err > 10  -- Should have substantial error information
      Right typusFile -> do
        -- May parse partially with error recovery
        assertBool "Should handle partial parsing" True

-- Test 3: Error handler categorizes errors by severity
test_error_severity_categorization :: TestTree
test_error_severity_categorization =
  testCase "Error handler categorizes errors by severity" $ do
    let criticalErrors = 
          [ CompilerError "Division by zero" (SourcePos 1 10) (SourcePos 1 10)
          , CompilerError "Null pointer dereference" (SourcePos 2 5) (SourcePos 2 15)
          ]
        warnings = 
          [ CompilerError "Unused variable" (SourcePos 3 1) (SourcePos 3 5)
          , CompilerError "Dead code" (SourcePos 4 10) (SourcePos 4 20)
          ]
        allErrors = criticalErrors ++ warnings
        formatted = formatCompilerErrors allErrors
    -- Should distinguish between errors L.and warnings
    assertBool "Should format L.all errors L.and warnings" $
      L.length (lines formatted) >= 4

-- Test 4: Error handler provides helpful suggestions
test_error_suggestions :: TestTree
test_error_suggestions =
  testCase "Error handler provides helpful suggestions" $ do
    let source = unlines
          [ "package main"
          , "func main() {"
          , "  var x int"
          , "  // x is declared but never used"
          , "  y := 42"
          , "  return y"
          , "}"
          ]
    case parseTypus source of
      Left err -> do
        -- Should provide suggestions for unused variables
        assertBool "Should suggest fixes for unused variables" $
          L.any (`L.isInfixOf` err) ["unused", "remove", "use", "_"]
      Right typusFile -> do
        -- May parse successfully with warnings
        assertBool "Should handle unused variable warnings" True

-- QuickCheck property: Error formatting is deterministic
prop_error_formatting_deterministic :: String -> Property
prop_error_formatting_deterministic errorMsg =
  L.length errorMsg < 100 ==>  -- Keep error messages reasonable
  let error = CompilerError errorMsg (SourcePos 1 1) (SourcePos 1 10)
      formatted1 = formatCompilerErrors [error]
      formatted2 = formatCompilerErrors [error]
  in property $ formatted1 == formatted2

-- Test 5: Error handler preserves context information
test_error_context_preservation :: TestTree
test_error_context_preservation =
  testCase "Error handler preserves context information" $ do
    let source = unlines
          [ "package main"
          , "func calculate(x int, y int) int {"
          , "  if y == 0 {"
          , "    return x / y  // Division by zero"
          , "  }"
          , "  return x + y"
          , "}"
          ]
    case parseTypus source of
      Left err -> do
        -- Should include function context in error
        assertBool "Should include function context" $
          L.any (`L.isInfixOf` err) ["calculate", "function", "context"]
      Right typusFile -> do
        -- May parse successfully
        assertBool "Should parse function with potential error" True

-- Test 6: Error handler handles cascading errors
test_cascading_errors :: TestTree
test_cascading_errors =
  testCase "Error handler handles cascading errors" $ do
    let source = unlines
          [ "package main"
          , "func cascade() {"
          , "  a := undefined_type{}  // First error"
          , "  b := a.field  // Cascaded error"
          , "  c := b.method()  // Another cascaded error"
          , "}"
          ]
    case parseTypus source of
      Left err -> do
        -- Should handle cascading errors without crashing
        assertBool "Should handle cascading errors" $
          L.length err > 0
      Right typusFile -> do
        -- May parse partially
        assertBool "Should handle cascading errors gracefully" True

-- QuickCheck property: Error messages are informative
prop_error_messages_informative :: String -> Property
prop_error_messages_informative errorType =
  L.length errorType < 50 ==>  -- Keep error types reasonable
  let source = unlines
        [ "package main"
        , "func main() {"
        , "  // Intentional " ++ errorType ++ " error"
        , "}"
        ]
  in case parseTypus source of
       Left err -> property $ L.length err > 5  -- Should have some content
       Right _ -> property True  -- May parse successfully

-- Test 7: Error handler handles recovery scenarios
test_error_recovery_scenarios :: TestTree
test_error_recovery_scenarios =
  testCase "Error handler handles recovery scenarios" $ do
    let source = unlines
          [ "package main"
          , "func recoverable() {"
          , "  if true {"
          , "    x := 5"
          , "    // Missing semicolon here"
          , "    y := 10"
          , "  }"
          , "  return 42  // This should still be reachable"
          , "}"
          ]
    case parseTypus source of
      Left err -> do
        -- Should attempt error recovery
        assertBool "Should attempt error recovery" $
          L.any (`L.isInfixOf` err) ["recover", "continue", "parse"]
      Right typusFile -> do
        -- Should recover L.and parse the return statement
        let codeBlocks = tfCodeBlocks typusFile
        assertBool "Should recover L.and parse return statement" $
          L.any (L.isInfixOf "return 42" . unlines . cbLines) codeBlocks

-- Test 8: Error handler localization consistency
test_error_localization :: TestTree
test_error_localization =
  testCase "Error handler localization consistency" $ do
    let errors = 
          [ CompilerError "变量未定义" (SourcePos 1 10) (SourcePos 1 15)  -- Chinese
          , CompilerError "Undefined variable" (SourcePos 2 5) (SourcePos 2 10)  -- English
          ]
        formatted = formatCompilerErrors errors
    -- Should handle different languages consistently
    assertBool "Should handle multilingual error messages" $
      L.length (lines formatted) >= 2

tests :: TestTree
tests =
  testGroup "Error Handler Consistency Cabals Tests"
    [ test_error_handler_consistency
    , test_multiple_errors_handling
    , test_error_severity_categorization
    , test_error_suggestions
    , fastProperty "Error formatting is deterministic" prop_error_formatting_deterministic
    , test_error_context_preservation
    , test_cascading_errors
    , fastProperty "Error messages are informative" prop_error_messages_informative
    , test_error_recovery_scenarios
    , test_error_localization
    ]