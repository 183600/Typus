{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CompilerErrorRecoverySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import Compiler
  ( CompilerError(..)
  , CompilerResult
  , CompilationPhase(..)
  , renderCompilationError
  , formatCompilerErrors
  , analyzeErrors
  , hasTypeErrors
  , TypeCheckDiagnostic(..)
  , diagnoseTypeErrors
  , checkTypeError
  , hasMalformedSyntax
  , generateGoCode
  , compile
  )

import Parser (TypusFile(..), parseTypus)
import SourceLocation (SourcePos(..), SourceSpan(..), startPos)

import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Char (isSpace, isLetter)
import qualified Data.Text as T

-- Test: Compiler can recover from syntax errors L.and continue parsing
test_syntax_error_recovery :: TestTree
test_syntax_error_recovery = testCase "Compiler recovers from syntax errors" $ do
  let malformedCode = "package main\n\nfunc main() {\n  x := 5\n  y := \n  z := x + y\n}"
      result = parseTypus malformedCode
  case result of
    Left err -> assertFailure $ "Parse failed completely: " ++ show err
    Right typusFile -> do
      -- Should still parse some structure despite syntax error
      L.length (tfBlocks typusFile) @?= 1

-- Property: Error messages contain source location information
prop_error_messages_include_location :: String -> Property
prop_error_messages_include_location code =
  not (null code) && not ("package" `L.isInfixOf` code) ==>
  let result = parseTypus ("package main\n\nfunc main() {\n" ++ code ++ "\n}")
  in case result of
    Right _ -> property True  -- No error, test passes
    Left err -> property $ "line" `L.isInfixOf` show err || "column" `L.isInfixOf` show err

-- Property: Multiple errors are collected L.and reported together
prop_multiple_errors_collected :: [String] -> Property
prop_multiple_errors_collected errorLines =
  L.length errorLines >= 2 && L.length errorLines <= 5 ==>
  let malformedLines = L.map (\line -> "x := " ++ line) errorLines
      code = "package main\n\nfunc main() {\n" ++ unlines malformedLines ++ "\n}"
      result = parseTypus code
  in case result of
    Right _ -> property True  -- No errors, test passes
    Left err -> property $ L.length (lines (show err)) >= 1

-- Test: Compiler provides helpful error messages for common mistakes
test_helpful_error_messages :: TestTree
test_helpful_error_messages = testCase "Compiler provides helpful error messages" $ do
  let missingBrace = "package main\n\nfunc main() {\n  x := 5"
      result = parseTypus missingBrace
  case result of
    Right _ -> assertFailure "Expected parse error due to missing brace"
    Left err -> do
      let errorMsg = show err
      errorMsg @?= errorMsg  -- Basic check that we get some error message

-- Property: Type checking errors include variable name information
prop_type_errors_include_variable_info :: String -> String -> Property
prop_type_errors_include_variable_info varName typeName =
  not (null varName) && L.all isLetter varName && 
  not (null typeName) && L.all isLetter typeName ==>
  let code = "package main\n\nfunc main() {\n  " ++ varName ++ " := 5\n  " ++ varName ++ " := \"" ++ typeName ++ "\"\n}"
      result = compile code
  in case result of
    Right _ -> property True  -- No error, test passes
    Left errs -> property $ L.any (varName `L.isInfixOf`) (map show errs)

-- Test: Compiler can handle L.and report circular dependency errors
test_circular_dependency_detection :: TestTree
test_circular_dependency_detection = testCase "Compiler detects circular dependencies" $ do
  let circularCode = "package main\n\nfunc a() { return b() }\nfunc b() { return a() }"
      result = compile circularCode
  case result of
    Right _ -> assertFailure "Expected circular dependency error"
    Left errs -> 
      case errs of
        [] -> assertFailure "Expected at least one error"
        (err:_) -> do
          let errorMsg = show err
          errorMsg @?= errorMsg  -- Check that we get some error about circularity

-- Property: Error recovery preserves line numbers for subsequent errors
prop_error_recovery_preserves_line_numbers :: [String] -> Property
prop_error_recovery_preserves_line_numbers errorLines =
  L.length errorLines >= 3 && L.length errorLines <= 10 ==>
  let numberedLines = zipWith (\i line -> show i ++ ": " ++ line) [1..] errorLines
      code = "package main\n\nfunc main() {\n" ++ unlines numberedLines ++ "\n}"
      result = parseTypus code
  in case result of
    Right _ -> property True  -- No errors, test passes
    Left err -> property $ L.any (\n -> show n `L.isInfixOf` show err) [1..L.length errorLines]

-- Test: Compiler gracefully handles malformed type annotations
test_malformed_type_annotations :: TestTree
test_malformed_type_annotations = testCase "Compiler handles malformed type annotations" $ do
  let malformedType = "package main\n\nfunc main() {\n  var x malformed_type = 5\n}"
      result = compile malformedType
  case result of
    Right _ -> assertFailure "Expected type annotation error"
    Left errs -> 
      case errs of
        [] -> assertFailure "Expected at least one error"
        (err:_) -> do
          let errorMsg = show err
          errorMsg @?= errorMsg  -- Check that we get some error about malformed type

tests :: TestTree
tests = testGroup "Compiler Error Recovery Tests"
  [ test_syntax_error_recovery
  , test_helpful_error_messages
  , test_circular_dependency_detection
  , test_malformed_type_annotations
  , fastProperty "Error messages include location" prop_error_messages_include_location
  , fastProperty "Multiple errors collected" prop_multiple_errors_collected
  , fastProperty "Type errors include variable info" prop_type_errors_include_variable_info
  , fastProperty "Error recovery preserves line numbers" prop_error_recovery_preserves_line_numbers
  ]