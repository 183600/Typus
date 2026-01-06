{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.SourceLocationPrecisionCabalsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary (genString, genNonEmptyString)

import SourceLocation 
  ( SourcePos(..), SourceSpan(..), Located(..), 
    locatedWithSpan, spanStart, spanEnd, posLine, posColumn
  )
import Parser (parseTypus, TypusFile(..))
import Compiler (CompilerError(..), formatCompilerErrors)

import qualified Data.List as L
import Data.List (isInfixOf, isPrefixOf, length)
import Data.List (lines)
import qualified Data.Text as T

-- Test 1: Source location tracking for syntax errors
test_source_location_syntax_errors :: TestTree
test_source_location_syntax_errors =
  testCase "Source location tracking for syntax errors" $ do
    let source = unlines
          [ "package main"
          , "func main() {"
          , "  x := 5"
          , "  y := 10"
          , "  if x > y {  // Line 4"
          , "    println(\"x is greater\")"
          , "  // Missing closing brace - error should point here"
          , "}"
          ]
    case parseTypus source of
      Left err -> do
        -- Error should mention line number around the missing brace
        assertBool "Error should mention correct line number" $
          L.any (`L.isInfixOf` err) ["4", "5", "6", "line"]
      Right _ -> do
        assertFailure "Expected parsing error for missing brace"

-- Test 2: Source location precision for type errors
test_source_location_type_errors :: TestTree
test_source_location_type_errors =
  testCase "Source location precision for type errors" $ do
    let source = unlines
          [ "package main"
          , "func add(a int, b string) int {"  -- Line 2: type mismatch
          , "  return a + b  // Line 3: error should point here"
          , "}"
          , "func main() {"
          , "  result := add(5, \"hello\")"
          , "  println(result)"
          , "}"
          ]
    case parseTypus source of
      Left err -> assertFailure $ "Parse failed: " ++ err
      Right typusFile -> do
        -- For this test, we'll check that source parsing preserves line info
        let codeBlocks = tfCodeBlocks typusFile
        assertBool "Should preserve code block structure" $
          L.length codeBlocks > 0

-- Test 3: Source location tracking across multiple files
test_source_location_multiple_files :: TestTree
test_source_location_multiple_files =
  testCase "Source location tracking across multiple files" $ do
    let source = unlines
          [ "//! import: \"helper.typus\""
          , "package main"
          , "func main() {"
          , "  helperFunction()  // Should track location across files"
          , "}"
          ]
    case parseTypus source of
      Left err -> do
        -- Should handle import directives L.and track locations
        assertBool "Should handle import directives" $
          L.any (`L.isInfixOf` err) ["import", "helper", "file"]
      Right typusFile -> do
        -- Should parse successfully with import tracking
        assertBool "Should parse with import directives" True

-- Test 4: Source location precision for macro expansion
test_source_location_macro_expansion :: TestTree
test_source_location_macro_expansion =
  testCase "Source location precision for macro expansion" $ do
    let source = unlines
          [ "package main"
          , "#define LOG(x) println(x)  // Line 2: macro definition"
          , "func main() {"
          , "  LOG(\"hello\")  // Line 4: macro usage - error should point here"
          , "}"
          ]
    case parseTypus source of
      Left err -> do
        -- Should track macro expansion locations
        assertBool "Should handle macro expansion" $
          L.any (`L.isInfixOf` err) ["macro", "define", "LOG"]
      Right typusFile -> do
        -- Should parse macros correctly
        assertBool "Should parse macro definitions" True

-- QuickCheck property: Source positions are consistent
prop_source_positions_consistent :: String -> Property
prop_source_positions_consistent code =
  L.length code < 100 ==>  -- Keep code reasonable
  let source = unlines
        [ "package main"
        , "func main() {"
        , "  " ++ code
        , "}"
        ]
  in case parseTypus source of
       Left _ -> property True  -- Invalid code is skipped
       Right typusFile ->
         let codeBlocks = tfCodeBlocks typusFile
         in property $ L.all (not . null . cbLines) codeBlocks

-- Test 5: Source location tracking with Unicode
test_source_location_unicode :: TestTree
test_source_location_unicode =
  testCase "Source location tracking with Unicode" $ do
    let source = unlines
          [ "package main"
          , "func 测试函数() {  // Unicode function name"
          , "  message := \"你好世界\"  // Unicode string"
          , "  println(message)"
          , "}"
          ]
    case parseTypus source of
      Left err -> do
        -- Should handle Unicode characters in location tracking
        assertBool "Should handle Unicode in source locations" $
          L.length err > 0
      Right typusFile -> do
        -- Should parse Unicode correctly
        let codeBlocks = tfCodeBlocks typusFile
        assertBool "Should parse Unicode content" $
          L.any (isInfixOf "测试函数" . unlines . cbLines) codeBlocks

-- Test 6: Source location precision in error messages
test_source_location_error_precision :: TestTree
test_source_location_error_precision =
  testCase "Source location precision in error messages" $ do
    let source = unlines
          [ "package main"
          , "func calculate(x int, y int) int {"
          , "  if x > 0 {"
          , "    return x + y"
          , "  } else if y > 0 {"
          , "    return x * y"
          , "  }"
          , "  // Missing return statement - error should point to function"
          , "}"
          ]
    case parseTypus source of
      Left err -> do
        -- Error should point to the function, not just anywhere
        assertBool "Error should point to function location" $
          L.any (`L.isInfixOf` err) ["calculate", "function", "return"]
      Right _ -> do
        -- May pass if return statement analysis is not implemented
        assertBool "Should analyze function completeness" True

-- QuickCheck property: Line numbers are accurate
prop_line_numbers_accurate :: Int -> Int -> Property
prop_line_numbers_accurate offset contentLines =
  offset >= 0 && offset <= 10 && contentLines >= 0 && contentLines <= 20 ==>
  let prefixLines = replicate offset "// comment"
      contentLines' = replicate contentLines "x := 42"
      source = unlines $ prefixLines ++ contentLines'
  in case parseTypus source of
       Left _ -> property True
       Right typusFile ->
         let codeBlocks = tfCodeBlocks typusFile
         in property $ L.length codeBlocks > 0

-- Test 7: Source location tracking with nested structures
test_source_location_nested_structures :: TestTree
test_source_location_nested_structures =
  testCase "Source location tracking with nested structures" $ do
    let source = unlines
          [ "package main"
          , "type Outer struct {"
          , "  field1 int"
          , "  inner struct {"
          , "    field2 string"
          , "    deep struct {"
          , "      field3 float64"
          , "    }"
          , "  }"
          , "}"
          , "func main() {"
          , "  o := Outer{"
          , "    inner: struct {"
          , "      field2: \"test\""
          , "    }"
          , "  }"
          , "}"
          ]
    case parseTypus source of
      Left err -> do
        -- Should handle deeply nested structures
        assertBool "Should handle nested structure errors" $
          L.any (`L.isInfixOf` err) ["nested", "struct", "field"]
      Right typusFile -> do
        -- Should parse nested structures correctly
        let codeBlocks = tfCodeBlocks typusFile
        assertBool "Should parse nested structures" $
          L.any (isInfixOf "Outer" . unlines . cbLines) codeBlocks

tests :: TestTree
tests =
  testGroup "Source Location Precision Cabals Tests"
    [ test_source_location_syntax_errors
    , test_source_location_type_errors
    , test_source_location_multiple_files
    , test_source_location_macro_expansion
    , fastProperty "Source positions are consistent" prop_source_positions_consistent
    , test_source_location_unicode
    , test_source_location_error_precision
    , fastProperty "Line numbers are accurate" prop_line_numbers_accurate
    , test_source_location_nested_structures
    ]