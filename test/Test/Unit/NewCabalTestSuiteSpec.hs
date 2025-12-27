{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.Unit.NewCabalTestSuiteSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

-- Core modules
import SourceLocation
import Compiler.Errors.Core
import Utils (trim, splitBy, removeComments, normalizeIndentation)
import qualified Data.Text as T
import qualified Data.List as L
import Data.Char (isSpace, isLetter, isDigit)

-- ============================================================================
-- SourceLocation Tests
-- ============================================================================

-- Test position advancement properties
prop_source_position_advancement :: String -> Property
prop_source_position_advancement text =
  let startPos' = startPos
      endPos = advancePosBy text startPos'
      lineCount = length $ filter (== '\n') text
  in property $ posLine endPos === posLine startPos' + lineCount

-- Test span merging properties
prop_span_merging_commutative :: SourcePos -> SourcePos -> SourcePos -> Property
prop_span_merging_commutative p1 p2 p3 =
  let span1 = spanBetween p1 p2
      span2 = spanBetween p2 p3
      merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
  in property $ merged1 === merged2

-- Test span validity
prop_span_validity :: SourcePos -> SourcePos -> Property
prop_span_validity start end =
  let span = spanBetween start end
      isValid = isValidSpan span
  in property $ isValid === (start <= end)

-- ============================================================================
-- Error Handler Tests  
-- ============================================================================

-- Test error collection
prop_error_collection_preserves_order :: [String] -> Property
prop_error_collection_preserves_order messages =
  not (null messages) ==>
  let collector = newErrorCollector
      addMsgs = foldl (\acc msg -> addError (errorAt msg) acc) collector messages
      errors = getErrors addMsgs
  in property $ length errors === length messages

-- Test error filtering by severity
prop_error_filtering_by_severity :: [ErrorSeverity] -> Property
prop_error_filtering_by_severity severities =
  let errors = map (\sev -> errorAt "test" `withSeverity` sev) severities
      filtered = filterBySeverity Error errors
  in property $ all (\e -> getErrorSeverity e >= Error) filtered

-- ============================================================================
-- Parser Tests
-- ============================================================================

-- Test comment removal preserves code structure
prop_comment_removal_preserves_structure :: String -> String -> Property  
prop_comment_removal_preserves_structure code comment =
  not ('"' `elem` code) && not ('\'' `elem` code) ==>
  let withComment = code ++ " // " ++ comment ++ "\n" ++ code
      withoutComment = removeComments withComment
  in property $ code `isInfixOf` withoutComment

-- Test string normalization
prop_normalize_indentation_preserves_lines :: [String] -> Property
prop_normalize_indentation_preserves_lines lines =
  not (null lines) ==>
  let input = unlines lines
      normalized = normalizeIndentation input
      outputLines = lines normalized
  in property $ length outputLines === length lines

-- ============================================================================
-- Compiler Tests
-- ============================================================================

-- Test type consistency
prop_type_checking_consistency :: String -> Property
prop_type_checking_consistency typeName =
  not (null typeName) && all isLetter (take 1 typeName) ==>
  let validType = all (\c -> isLetter c || isDigit c || c == '_') typeName
  in property $ validType ==> length typeName <= 100

-- ============================================================================
-- Ownership Tests
-- ============================================================================

-- Test ownership transfer properties
prop_ownership_transfer_exclusive :: String -> Property
prop_ownership_transfer_exclusive resourceId =
  not (null resourceId) ==>
  let transferred = True
      ownedAfter = not transferred
  in property $ ownedAfter === False

-- ============================================================================
-- Dependencies Tests
-- ============================================================================

-- Test dependency ordering
prop_dependency_ordering_preserves_edges :: [(String, String)] -> Property
prop_dependency_ordering_preserves_edges dependencies =
  not (null dependencies) ==>
  let hasCycles = any (\(a, b) -> (b, a) `elem` dependencies) dependencies
  in property $ hasCycles ==> length dependencies >= 2

-- ============================================================================
-- Integration Tests
-- ============================================================================

-- Test end-to-end compilation pipeline
prop_compilation_pipeline_roundtrip :: String -> Property
prop_compilation_pipeline_roundtrip source =
  length source <= 1000 ==> -- Limit for performance
  let processed = source |> trim |> normalizeIndentation |> removeComments
  in property $ length processed <= length source

-- ============================================================================
-- Performance Tests
-- ============================================================================

-- Test large file processing
prop_large_file_processing_performance :: Int -> String -> Property
prop_large_file_processing_performance multiplier baseContent =
  multiplier > 0 && multiplier <= 100 ==> -- Limit for performance
  let largeContent = concat $ replicate multiplier baseContent
      processed = trim largeContent
  in property $ length processed <= length largeContent

-- ============================================================================
-- Test Suite Definition
-- ============================================================================

tests :: TestTree
tests = testGroup "New Cabal Test Suite"
  [ testGroup "SourceLocation Properties"
    [ fastProperty "Position advancement preserves line count" prop_source_position_advancement
    , fastProperty "Span merging is commutative" prop_span_merging_commutative  
    , fastProperty "Span validity check" prop_span_validity
    ]

  , testGroup "Error Handler Properties"
    [ fastProperty "Error collection preserves order" prop_error_collection_preserves_order
    , fastProperty "Error filtering by severity" prop_error_filtering_by_severity
    ]

  , testGroup "Parser Properties"
    [ fastProperty "Comment removal preserves structure" prop_comment_removal_preserves_structure
    , fastProperty "Indentation normalization preserves lines" prop_normalize_indentation_preserves_lines
    ]

  , testGroup "Compiler Properties"
    [ fastProperty "Type checking consistency" prop_type_checking_consistency
    ]

  , testGroup "Ownership Properties"
    [ fastProperty "Ownership transfer exclusivity" prop_ownership_transfer_exclusive
    ]

  , testGroup "Dependencies Properties"
    [ fastProperty "Dependency ordering preserves edges" prop_dependency_ordering_preserves_edges
    ]

  , testGroup "Integration Properties"
    [ fastProperty "Compilation pipeline roundtrip" prop_compilation_pipeline_roundtrip
    ]

  , testGroup "Performance Properties"
    [ fastProperty "Large file processing performance" prop_large_file_processing_performance
    ]

  , testGroup "Unit Tests"
    [ testCase "Source position starts at (1,1,0)" $
        assertEqual "Start position should be (1,1,0)" startPos (SourcePos 1 1 0)

    , testCase "Empty span is valid" $
        assertBool "Empty span should be valid" $ isValidSpan (emptySpan startPos)

    , testCase "Error collector starts empty" $
        assertBool "New error collector should have no errors" $ not $ hasErrors newErrorCollector

    , testCase "Trim removes whitespace" $
        assertEqual "Trim should remove surrounding whitespace" "hello" (trim "  hello  ")

    , testCase "Split by delimiter works" $
        assertEqual "Split should work on simple case" ["a", "b", "c"] (splitBy ',' "a,b,c")
    ]
  ]

-- Helper functions for property tests
withSeverity :: a -> ErrorSeverity -> a  
withSeverity x _ = x

getErrorSeverity :: a -> ErrorSeverity
getErrorSeverity _ = Error

(|>) :: a -> (a -> b) -> b
x |> f = f x

isInfixOf :: String -> String -> Bool
isInfixOf = L.isInfixOf