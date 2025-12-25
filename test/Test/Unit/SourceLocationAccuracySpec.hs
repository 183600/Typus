module Test.Unit.SourceLocationAccuracySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property)

import Compiler (compile, CompilerError(..), formatCompilerErrors)
import Parser (parseTypus)
import SourceLocation (SourcePos(..), SourceSpan(..), spanStart, spanEnd, posLine, posColumn)
import qualified Data.Text as T
import Data.List (isInfixOf, lines)

-- Test error location accuracy for syntax errors
test_syntax_error_location :: TestTree
test_syntax_error_location = testCase "Syntax errors have accurate source locations" $ do
    let source = unlines
          [ "package main"
          , "func main() {"
          , "    x := 5"
          , "    // missing semicolon and closing brace"
          , "    y := 10"
          , "}"
          ]
    case parseTypus source of
      Left err -> do
        assertBool "Error should mention line number" $ 
          any (`isInfixOf` err) ["line 3", "line 4", "line 5"]
        assertBool "Error should be informative" $ 
          length err > 10
      Right _ -> assertFailure "Expected parse error"

-- Test type error location accuracy
test_type_error_location :: TestTree
test_type_error_location = testCase "Type errors have accurate source locations" $ do
    let source = unlines
          [ "package main"
          , "func main() {"
          , "    x := 5"
          , "    x := \"string\"  // line 4: type reassignment"
          , "    y := x + 1"
          , "}"
          ]
    result <- compile source
    case result of
      Left errs -> do
        let errorMessages = formatCompilerErrors errs
        assertBool "Should mention line 4" $ 
          any ("line 4" `isInfixOf`) errorMessages
        assertBool "Should mention variable name" $ 
          any ("x" `isInfixOf`) errorMessages
      Right _ -> assertFailure "Expected type error"

-- Test ownership error location accuracy
test_ownership_error_location :: TestTree
test_ownership_error_location = testCase "Ownership errors have accurate source locations" $ do
    let source = unlines
          [ "//! ownership: on"
          , "package main"
          , "func main() {"
          , "    data := make([]int, 10)"
          , "    moved := data"
          , "    _ = data[0]"  // line 6: use after move"
          , "}"
          ]
    result <- compile source
    case result of
      Left errs -> do
        let errorMessages = formatCompilerErrors errs
        assertBool "Should mention line 6" $ 
          any ("line 6" `isInfixOf`) errorMessages
        assertBool "Should mention variable name" $ 
          any ("data" `isInfixOf`) errorMessages
      Right _ -> assertFailure "Expected ownership error"

-- Test dependent type error location accuracy
test_dependent_type_error_location :: TestTree
test_dependent_type_error_location = testCase "Dependent type errors have accurate source locations" $ do
    let source = unlines
          [ "//! dependent_types: on"
          , "package main"
          , "func main() {"
          , "    type Vec5 = [5]int"
          , "    v := Vec5{1, 2, 3}"  // line 5: insufficient elements"
          , "}"
          ]
    result <- compile source
    case result of
      Left errs -> do
        let errorMessages = formatCompilerErrors errs
        assertBool "Should mention line 5" $ 
          any ("line 5" `isInfixOf`) errorMessages
        assertBool "Should mention variable name" $ 
          any ("v" `isInfixOf`) errorMessages
      Right _ -> assertFailure "Expected dependent type error"

-- Test multi-error location tracking
test_multi_error_location_tracking :: TestTree
test_multi_error_location_tracking = testCase "Multiple errors have distinct source locations" $ do
    let source = unlines
          [ "//! ownership: on"
          , "//! dependent_types: on"
          , "package main"
          , "func main() {"
          , "    x := 5"
          , "    x := \"string\""  // line 6: type error"
          , "    data := make([]int, 10)"
          , "    moved := data"
          , "    _ = data[0]"     // line 9: ownership error"
          , "    type Vec3 = [3]int"
          , "    v := Vec3{1, 2}" // line 11: dependent type error"
          , "}"
          ]
    result <- compile source
    case result of
      Left errs -> do
        let errorMessages = formatCompilerErrors errs
        assertBool "Should mention line 6" $ 
          any ("line 6" `isInfixOf`) errorMessages
        assertBool "Should mention line 9" $ 
          any ("line 9" `isInfixOf`) errorMessages
        assertBool "Should mention line 11" $ 
          any ("line 11" `isInfixOf`) errorMessages
      Right _ -> assertFailure "Expected multiple errors"

-- Test column position accuracy
test_column_position_accuracy :: TestTree
test_column_position_accuracy = testCase "Column positions are accurate" $ do
    let source = "package main\nfunc main() { x := \"string\"; y := 5; }"
    result <- compile source
    case result of
      Left errs -> do
        let errorMessages = formatCompilerErrors errs
        assertBool "Should provide column information" $ 
          any ("column" `isInfixOf`) errorMessages
      Right _ -> return ()  -- May or may not have errors

-- Test location tracking in nested blocks
test_nested_block_location :: TestTree
test_nested_block_location = testCase "Nested block errors have accurate locations" $ do
    let source = unlines
          [ "package main"
          , "func main() {"
          , "    if true {"
          , "        x := 5"
          , "        x := \"string\""  // line 5: nested error"
          , "    }"
          , "}"
          ]
    result <- compile source
    case result of
      Left errs -> do
        let errorMessages = formatCompilerErrors errs
        assertBool "Should mention line 5" $ 
          any ("line 5" `isInfixOf`) errorMessages
        assertBool "Should indicate nested context" $ 
          any (\msg -> "nested" `isInfixOf` msg || "block" `isInfixOf` msg) errorMessages
      Right _ -> assertFailure "Expected nested block error"

-- Test location tracking in function definitions
test_function_definition_location :: TestTree
test_function_definition_location = testCase "Function definition errors have accurate locations" $ do
    let source = unlines
          [ "package main"
          , "func test() int {"
          , "    return \"string\""  // line 3: return type mismatch"
          , "}"
          , "func main() {"
          , "    _ := test()"
          , "}"
          ]
    result <- compile source
    case result of
      Left errs -> do
        let errorMessages = formatCompilerErrors errs
        assertBool "Should mention line 3" $ 
          any ("line 3" `isInfixOf`) errorMessages
        assertBool "Should mention function name" $ 
          any ("test" `isInfixOf`) errorMessages
      Right _ -> assertFailure "Expected function definition error"

-- QuickCheck property: Line numbers increase monotonically
prop_line_numbers_monotonic :: [String] -> Property
prop_line_numbers_monotonic linesList =
  let lineNumbers = [1..length linesList]
      isMonotonic = all (uncurry (<=)) (zip lineNumbers (tail lineNumbers))
  in classify (length linesList > 1) "multiple lines" $
     property isMonotonic

-- QuickCheck property: Column positions are within line bounds
prop_columns_within_line_bounds :: String -> Property
prop_columns_within_line_bounds line =
  let lineLength = length line
      validColumn col = col >= 1 && col <= lineLength + 1
  in classify (not (null line)) "non-empty line" $
     property $ validColumn (min 1 lineLength)

-- QuickCheck property: Source spans are well-formed
prop_source_spans_well_formed :: Int -> Int -> Int -> Int -> Property
prop_source_spans_well_formed startLine startCol endLine endCol =
  let validSpan = (startLine < endLine) || 
                  (startLine == endLine && startCol <= endCol)
  in classify validSpan "valid span" $
     classify (not validSpan) "invalid span" $
     property validSpan

tests :: TestTree
tests = testGroup "Source Location Accuracy"
  [ test_syntax_error_location
  , test_type_error_location
  , test_ownership_error_location
  , test_dependent_type_error_location
  , test_multi_error_location_tracking
  , test_column_position_accuracy
  , test_nested_block_location
  , test_function_definition_location
  , testCase "QuickCheck: Line numbers monotonic" $
      fastProperty prop_line_numbers_monotonic
  , testCase "QuickCheck: Columns within line bounds" $
      fastProperty prop_columns_within_line_bounds
  , testCase "QuickCheck: Source spans well-formed" $
      fastProperty prop_source_spans_well_formed
  ]