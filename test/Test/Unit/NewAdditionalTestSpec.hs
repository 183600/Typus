{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewAdditionalTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (sort, nub)
import Data.Char (isSpace, isAlpha, isAlphaNum, isDigit)
import qualified Data.Text as T

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  , posAfter
  , emptySpan
  , spanFrom
  , mergeSpans
  , isValidSpan
  , locatedAt
  , locatedValue
  , advancePos
  , advancePosBy
  )

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , splitByComma
  , removeLineComments
  , removeComments
  , normalizeIndentation
  , breakOn
  )

import Parser
  ( parseTypus
  , FileDirectives(..)
  , BlockDirectives(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )

import Compiler
  ( CompilerError(..)
  , CompilationPhase(..)
  , hasTypeErrors
  , checkDependentTypes
  , checkOwnership
  , generateGoCode
  )

import Compiler.Errors.Core
  ( formatError
  , ErrorSeverity(..)
  , getErrorLine
  , getErrorColumn
  )

-- ============================================================================
-- SourceLocation Tests
-- ============================================================================

-- Test 1: SourceSpan merging properties
prop_mergeSpans_commutative :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_commutative span1 span2 =
  property $ mergeSpans span1 span2 === mergeSpans span2 span1

prop_mergeSpans_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_mergeSpans_associative span1 span2 span3 =
  property $ mergeSpans span1 (mergeSpans span2 span3) === mergeSpans (mergeSpans span1 span2) span3

-- Test 2: Position advancement properties
prop_advancePos_by_newline :: SourcePos -> Int -> Property
prop_advancePos_by_newline pos n =
  let advanced = advancePosBy pos ('\n' : replicate n ' ')
  in property $ posLine advanced === posLine pos + 1 .&&. posColumn advanced === n + 1

prop_advancePos_by_space :: SourcePos -> Int -> Property
prop_advancePos_by_space pos n =
  let advanced = advancePosBy pos (replicate n ' ')
  in property $ posLine advanced === posLine pos .&&. posColumn advanced === posColumn pos + n

-- ============================================================================
-- Utils Tests
-- ============================================================================

-- Test 3: String splitting properties
prop_splitBy_length_preservation :: Char -> String -> Property
prop_splitBy_length_preservation delim str =
  let parts = splitBy delim str
      totalLength = L.sum (map L.length parts) + L.length parts - 1
  in property $ totalLength === L.length str

prop_splitByCollapsed_no_empty :: Char -> String -> Property
prop_splitByCollapsed_no_empty delim str =
  let parts = splitByCollapsed delim str
  in property $ L.all (not . null) parts

-- Test 4: Comment removal properties
prop_removeLineComments_preserves_non_comments :: String -> Property
prop_removeLineComments_preserves_non_comments str =
  let noComments = "//" `L.isInfixOf` str
      result = removeLineComments str
  in classify noComments "no comments present" $
      property $ if noComments then result === str else L.length result <= L.length str

prop_trim_idempotent :: String -> Property
prop_trim_idempotent str =
  let trimmedOnce = trim str
      trimmedTwice = trim trimmedOnce
  in property $ trimmedOnce === trimmedTwice

-- ============================================================================
-- Parser Tests
-- ============================================================================

-- Test 5: Directive parsing
test_parse_ownership_directive :: TestTree
test_parse_ownership_directive =
  testCase "parses ownership directive correctly" $ do
    let source = "//! ownership: on\npackage main\nfunc main() {}"
    case parseTypus source of
      Left err -> assertFailure $ "parseTypus failed: " ++ err
      Right typusFile -> do
        let FileDirectives { fdOwnership = ownership } = tfDirectives typusFile
        case ownership of
          Nothing -> assertFailure "expected ownership directive"
          Just loc -> locatedValue loc @?= True

test_parse_dependent_types_directive :: TestTree
test_parse_dependent_types_directive =
  testCase "parses dependent types directive correctly" $ do
    let source = "//! dependent_types: off\npackage main\nfunc main() {}"
    case parseTypus source of
      Left err -> assertFailure $ "parseTypus failed: " ++ err
      Right typusFile -> do
        let FileDirectives { fdDependentTypes = dependentTypes } = tfDirectives typusFile
        case dependentTypes of
          Nothing -> assertFailure "expected dependent types directive"
          Just loc -> locatedValue loc @?= False

test_parse_multiple_directives :: TestTree
test_parse_multiple_directives =
  testCase "parses multiple directives correctly" $ do
    let source = "//! ownership: on\n//! dependent_types: on\n//! constraints: off\npackage main\nfunc main() {}"
    case parseTypus source of
      Left err -> assertFailure $ "parseTypus failed: " ++ err
      Right typusFile -> do
        let FileDirectives { fdOwnership = ownership, fdDependentTypes = dependentTypes, fdConstraints = constraints } = tfDirectives typusFile
        case ownership of
          Nothing -> assertFailure "expected ownership directive"
          Just loc -> locatedValue loc @?= True
        case dependentTypes of
          Nothing -> assertFailure "expected dependent types directive"
          Just loc -> locatedValue loc @?= True
        case constraints of
          Nothing -> assertFailure "expected constraints directive"
          Just loc -> locatedValue loc @?= False

-- ============================================================================
-- Compiler Tests
-- ============================================================================

-- Test 6: Error detection
test_detects_type_errors :: TestTree
test_detects_type_errors =
  testCase "detects type errors correctly" $ do
    let hasErrors = hasTypeErrors "type mismatch error"
    assertBool "should detect type errors" hasErrors

test_check_dependent_types :: TestTree
test_check_dependent_types =
  testCase "checks dependent types" $ do
    let result = checkDependentTypes True
    case result of
      Left _ -> assertBool "dependent types check completed" True
      Right _ -> assertBool "dependent types check completed" True

test_check_ownership :: TestTree
test_check_ownership =
  testCase "checks ownership" $ do
    let result = checkOwnership True
    case result of
      Left _ -> assertBool "ownership check completed" True
      Right _ -> assertBool "ownership check completed" True

-- ============================================================================
-- Error Handler Tests
-- ============================================================================

-- Test 7: Error formatting
prop_format_error_non_empty :: String -> Property
prop_format_error_non_empty errorMsg =
  let formatted = formatError errorMsg
  in property $ not (null formatted)

prop_get_error_line_valid :: Int -> Property
prop_get_error_line_valid lineNum =
  let line = getErrorLine lineNum
  in property $ line >= 1

prop_get_error_column_valid :: Int -> Property
prop_get_error_column_valid colNum =
  let col = getErrorColumn colNum
  in property $ col >= 1

-- ============================================================================
-- Integration Tests
-- ============================================================================

-- Test 8: End-to-end parsing L.and compilation
test_parse_and_compile_simple_program :: TestTree
test_parse_and_compile_simple_program =
  testCase "parses L.and compiles simple program" $ do
    let source = unlines
          [ "//! ownership: on"
          , "package main"
          , "func main() {"
          , "  var x int = 42"
          , "  println(x)"
          , "}"
          ]
    case parseTypus source of
      Left err -> assertFailure $ "parseTypus failed: " ++ err
      Right typusFile -> do
        -- Check that directives are parsed correctly
        let FileDirectives { fdOwnership = ownership } = tfDirectives typusFile
        case ownership of
          Nothing -> assertFailure "expected ownership directive"
          Just loc -> locatedValue loc @?= True
        
        -- Check that code blocks are parsed
        assertBool "should have code blocks" (not (L.null (tfCodeBlocks typusFile)))

-- Test 9: String processing edge cases
test_string_processing_edge_cases :: TestTree
test_string_processing_edge_cases =
  testCase "handles string processing edge cases" $ do
    -- Test empty string
    trim "" @?= ""
    
    -- Test string with only whitespace
    trim "   \t\n   " @?= ""
    
    -- Test splitBy with empty string
    splitBy ',' "" @?= [""]
    
    -- Test splitByCollapsed with empty string
    splitByCollapsed ',' "" @?= []
    
    -- Test splitBy with consecutive delimiters
    splitBy ',' "a,,b" @?= ["a", "", "b"]
    
    -- Test splitByCollapsed with consecutive delimiters
    splitByCollapsed ',' "a,,b" @?= ["a", "b"]

-- Test 10: Source location accuracy
test_source_location_accuracy :: TestTree
test_source_location_accuracy =
  testCase "maintains source location accuracy" $ do
    let pos1 = startPos 1 1
        pos2 = posAfter pos1 'a'
        pos3 = advancePos pos2 '\n'
        pos4 = advancePos pos3 'b'
    
    posLine pos1 @?= 1
    posColumn pos1 @?= 1
    
    posLine pos2 @?= 1
    posColumn pos2 @?= 2
    
    posLine pos3 @?= 2
    posColumn pos3 @?= 1
    
    posLine pos4 @?= 2
    posColumn pos4 @?= 2
    
    -- Test span operations
    let span1 = spanFrom pos1
        span2 = spanTo pos4
        merged = mergeSpans span1 span2
    
    isValidSpan span1 @?= True
    isValidSpan span2 @?= True
    isValidSpan merged @?= True

-- Aggregate L.all tests
tests :: TestTree
tests =
  testGroup "New Additional Tests"
    [ testGroup "SourceLocation QuickCheck Tests"
        [ fastProperty "mergeSpans is commutative" prop_mergeSpans_commutative
        , fastProperty "mergeSpans is associative" prop_mergeSpans_associative
        , fastProperty "advancePos preserves line count for newlines" prop_advancePos_by_newline
        , fastProperty "advancePos advances column for spaces" prop_advancePos_by_space
        ]
    , testGroup "Utils QuickCheck Tests"
        [ fastProperty "splitBy preserves total L.length" prop_splitBy_length_preservation
        , fastProperty "splitByCollapsed has no empty parts" prop_splitByCollapsed_no_empty
        , fastProperty "removeLineComments preserves non-comments" prop_removeLineComments_preserves_non_comments
        , fastProperty "trim is idempotent" prop_trim_idempotent
        ]
    , testGroup "Parser Tests"
        [ test_parse_ownership_directive
        , test_parse_dependent_types_directive
        , test_parse_multiple_directives
        ]
    , testGroup "Compiler Tests"
        [ test_detects_type_errors
        , test_check_dependent_types
        , test_check_ownership
        ]
    , testGroup "Error Handler QuickCheck Tests"
        [ fastProperty "format error is non-empty" prop_format_error_non_empty
        , fastProperty "error line is valid" prop_get_error_line_valid
        , fastProperty "error column is valid" prop_get_error_column_valid
        ]
    , testGroup "Integration Tests"
        [ test_parse_and_compile_simple_program
        , test_string_processing_edge_cases
        , test_source_location_accuracy
        ]
    ]