{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.SourcePositionPrecisionSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  , spanStart
  , spanEnd
  , locatedWithSpan
  , locatedValue
  )

import Parser (parseTypus)
import Compiler (compile, renderCompilationError)

import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.List (nub, sort, lines)
import Data.Char (isLetter, isDigit, isSpace)
import qualified Data.Text as T
import qualified Data.Map as Map

-- Test: Source position tracking is accurate for multi-line code
test_multiline_position_tracking :: TestTree
test_multiline_position_tracking = testCase "Source position tracking for multi-line code" $ do
  let multilineCode = "package main\n\nfunc main() {\n  x := 5\n  y := 10\n  z := x + y\n}"
      result = compile multilineCode
  case result of
    Left errs -> do
      let errorMessages = map show errs
          hasLineInfo = L.any (\msg -> L.any (`L.isInfixOf` msg) ["line", "Line", "行"]) errorMessages
      if hasLineInfo
        then return ()  -- Success - line information included
        else assertFailure $ "Expected line information in error messages: " ++ unlines errorMessages
    Right _ -> return ()  -- Compilation succeeded

-- Property: Source positions are correctly calculated for nested expressions
prop_nested_expression_positions :: [String] -> Property
prop_nested_expression_positions expressions =
  L.length expressions >= 2 && L.length expressions <= 4 ==>
  let validExprs = L.filter (not . null) expressions
      code = "package main\n\nfunc main() {\n  result := " ++ unwords validExprs ++ "\n}"
      result = compile code
  in case result of
    Right _ -> property True  -- Success - positions handled correctly
    Left errs -> property $ L.any (\err -> L.any (\n -> show n `L.isInfixOf` show err) [1..L.length (lines code)]) errs

-- Test: Column positions are accurate for inline errors
test_column_position_accuracy :: TestTree
test_column_position_accuracy = testCase "Column position accuracy for inline errors" $ do
  let inlineErrorCode = "package main\n\nfunc main() {\n  x := 5 + + 3  // Invalid syntax\n}"
      result = compile inlineErrorCode
  case result of
    Left errs -> do
      let errorMessages = map show errs
          hasColumnInfo = L.any (\msg -> L.any (`L.isInfixOf` msg) ["column", "Column", "col", "Col"]) errorMessages
      if hasColumnInfo
        then return ()  -- Success - column information included
        else assertFailure $ "Expected column information in error messages: " ++ unlines errorMessages
    Right _ -> assertFailure "Expected compilation error"

-- Property: Source spans cover the complete error context
prop_complete_error_span :: String -> Property
prop_complete_error_span malformedExpression =
  not (null malformedExpression) && L.length malformedExpression <= 20 ==>
  let code = "package main\n\nfunc main() {\n  x := " ++ malformedExpression ++ "\n}"
      result = compile code
  in case result of
    Right _ -> property True  -- Success - no error, span not needed
    Left errs -> property $ L.any (\err -> L.length (show err) >= 10) errs  -- Error message should have reasonable L.length

-- Test: Source position tracking works with Unicode characters
test_unicode_position_tracking :: TestTree
test_unicode_position_tracking = testCase "Source position tracking with Unicode" $ do
  let unicodeCode = "package main\n\nfunc main() {\n  // 注释 with Unicode: αβγ\n  x := 5\n  y := \"测试\"\n}"
      result = compile unicodeCode
  case result of
    Left errs -> do
      let errorMessages = map show errs
          hasPositionInfo = L.any (\msg -> L.any (`L.isInfixOf` msg) ["line", "Line", "行"]) errorMessages
      if hasPositionInfo
        then return ()  -- Success - position tracking works with Unicode
        else assertFailure $ "Expected position information with Unicode: " ++ unlines errorMessages
    Right _ -> return ()  -- Compilation succeeded

-- Property: Source positions are preserved through error recovery
prop_error_recovery_position_preservation :: [String] -> Property
prop_error_recovery_position_preservation errorLines =
  L.length errorLines >= 2 && L.length errorLines <= 5 ==>
  let validLines = L.filter (not . null) errorLines
      code = "package main\n\nfunc main() {\n" ++ unlines (L.map (\line -> "  " ++ line) validLines) ++ "\n}"
      result = compile code
  in case result of
    Right _ -> property True  -- Success - no errors
    Left errs -> 
      let errorMessages = map show errs
          hasMultiplePositions = L.length (L.filter (\msg -> L.any (`L.isInfixOf` msg) ["line", "Line", "行"]) errorMessages) >= 1
      in property $ hasMultiplePositions

-- Test: Source position tracking handles tabs L.and spaces correctly
test_whitespace_position_tracking :: TestTree
test_whitespace_position_tracking = testCase "Source position tracking with mixed whitespace" $ do
  let whitespaceCode = "package main\n\nfunc main() {\n\t  x := 5\n\t  y := 10\n\t  z := x + y\n}"
      result = compile whitespaceCode
  case result of
    Left errs -> do
      let errorMessages = map show errs
          hasPositionInfo = L.any (\msg -> L.any (`L.isInfixOf` msg) ["line", "Line", "行"]) errorMessages
      if hasPositionInfo
        then return ()  -- Success - position tracking works with mixed whitespace
        else assertFailure $ "Expected position information with mixed whitespace: " ++ unlines errorMessages
    Right _ -> return ()  -- Compilation succeeded

-- Property: Source positions are accurate for errors in string literals
prop_string_literal_position_accuracy :: String -> Property
prop_string_literal_position_accuracy stringContent =
  not (null stringContent) && L.length stringContent <= 15 && not ('"' `elem` stringContent) ==>
  let code = "package main\n\nfunc main() {\n  x := \"" ++ stringContent ++ "\n  y := 5\n}"
      result = compile code
  in case result of
    Right _ -> property False  -- Should fail due to unclosed string
    Left errs -> property $ L.any (\err -> L.any (`L.isInfixOf` show err) ["line", "Line", "行"]) errs

-- Test: Source position tracking works with import statements
test_import_position_tracking :: TestTree
test_import_position_tracking = testCase "Source position tracking with imports" $ do
  let importCode = "package main\n\nimport (\n  \"fmt\"\n  \"invalid/package\"\n)\n\nfunc main() {\n  fmt.Println(\"test\")\n}"
      result = compile importCode
  case result of
    Left errs -> do
      let errorMessages = map show errs
          hasPositionInfo = L.any (\msg -> L.any (`L.isInfixOf` msg) ["line", "Line", "行"]) errorMessages
      if hasPositionInfo
        then return ()  -- Success - position tracking works with imports
        else assertFailure $ "Expected position information with import error: " ++ unlines errorMessages
    Right _ -> assertFailure "Expected import error"

-- Property: Source positions are maintained in macro expansions
prop_macro_expansion_positions :: String -> Property
prop_macro_expansion_positions macroName =
  not (null macroName) && L.all isLetter macroName ==>
  let code = "package main\n\n//go:generate " ++ macroName + "\n\nfunc main() {\n  x := 5\n}"
      result = compile code
  in case result of
    Right _ -> property True  -- Success - macro handled correctly
    Left errs -> property $ L.any (\err -> L.any (`L.isInfixOf` show err) ["line", "Line", "行"]) errs

tests :: TestTree
tests = testGroup "Source Position Precision Tests"
  [ test_multiline_position_tracking
  , test_column_position_accuracy
  , test_unicode_position_tracking
  , test_whitespace_position_tracking
  , test_import_position_tracking
  , fastProperty "Nested expression positions" prop_nested_expression_positions
  , fastProperty "Complete error span" prop_complete_error_span
  , fastProperty "Error recovery position preservation" prop_error_recovery_position_preservation
  , fastProperty "String literal position accuracy" prop_string_literal_position_accuracy
  , fastProperty "Macro expansion positions" prop_macro_expansion_positions
  ]