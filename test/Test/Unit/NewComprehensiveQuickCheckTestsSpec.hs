{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewComprehensiveQuickCheckTestsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property(..), (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), arbitrary, choose, listOf, elements, vectorOf
  , Positive(..), NonEmptyList(..), NonNegative(..)
  )

import Utils
  ( trim, splitBy, splitByCollapsed, removeLineComments, normalizeIndentation, breakOn )

import SourceLocation
  ( SourcePos(..), SourceSpan(..), Located(..)
  , startPos, posAfter, posAt, emptySpan, spanFrom, spanTo, mergeSpans
  , locatedAt, locatedWithSpan, advancePos, advancePosBy
  )

import Parser
  ( FileDirectives(..), BlockDirectives(..), defaultFileDirectives, defaultBlockDirectives )

import ErrorHandler
  ( TypeError(..), ErrorSeverity(..), ErrorLocation(..) )

import Data.Char (isSpace, isAlphaNum, isLetter)
import qualified Data.Text as T
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (intercalate)
import Data.Maybe (isJust, isNothing, fromMaybe)

-- ============================================================================
-- Test 1: Utils Module - String Processing Properties
-- ============================================================================

-- Property: splitBy L.and splitByCollapsed relationship
prop_split_by_relationship :: Char -> String -> Property
prop_split_by_relationship delim str =
  let collapsed = splitByCollapsed delim str
      normal = splitBy delim str
  in property $ L.all (not . null) collapsed .&&. 
             (null collapsed ==> L.all null normal)

-- Property: trim is idempotent
prop_trim_idempotent :: String -> Property
prop_trim_idempotent str = trim (trim str) === trim str

-- Property: removeLineComments preserves non-comment content
prop_remove_line_comments_preserves_content :: String -> String -> Property
prop_remove_line_comments_preserves_content prefix content =
  let line = prefix ++ "// this is a comment\n" ++ content
      processed = removeLineComments line
      containsContent = content `L.isInfixOf` processed
  in not (null content) ==> containsContent

-- ============================================================================
-- Test 2: SourceLocation Module - Position Arithmetic Properties
-- ============================================================================

-- Property: Position advancement is consistent
prop_pos_advancement_consistent :: Positive Int -> Positive Int -> String -> Property
prop_pos_advancement_consistent (Positive lineOffset) (Positive colOffset) text =
  let start = startPos
      advanced = advancePosBy start lineOffset colOffset
      lineCount = L.length $ L.filter (== '\n') text
  in lineOffset > 0 ==> 
     sourceLine advanced >= sourceLine start + min lineOffset lineCount

-- Property: Span merging is associative
prop_span_merging_associative :: SourcePos -> SourcePos -> SourcePos -> Property
prop_span_merging_associative p1 p2 p3 =
  let span12 = mergeSpans (spanFrom p1) (spanFrom p2)
      span23 = mergeSpans (spanFrom p2) (spanFrom p3)
      span123_left = mergeSpans span12 (spanFrom p3)
      span123_right = mergeSpans (spanFrom p1) span23
  in isValidSpan span12 .&&. isValidSpan span23 ==>
     spanStart span123_left === spanStart span123_right .&&.
     spanEnd span123_left === spanEnd span123_right

-- Property: Located values preserve their content
prop_located_preserves_content :: String -> SourcePos -> Property
prop_located_preserves_content value pos =
  let located = locatedAt pos value
  in locatedValue located === value

-- ============================================================================
-- Test 3: Parser Module - Directive Properties
-- ============================================================================

-- Property: Default directives are consistent
prop_default_directives_consistent :: Property
prop_default_directives_consistent =
  let fileDefaults = defaultFileDirectives
      blockDefaults = defaultBlockDirectives
  in property $ 
     isNothing (fdOwnership fileDefaults) .&&.
     isNothing (fdDependentTypes fileDefaults) .&&.
     isNothing (fdConstraints fileDefaults) .&&.
     isNothing (bdOwnership blockDefaults) .&&.
     isNothing (bdDependentTypes blockDefaults) .&&.
     isNothing (bdConstraints blockDefaults)

-- Property: Directive construction preserves values
prop_directive_preserves_values :: Bool -> Bool -> Bool -> Property
prop_directive_preserves_values own dep cons =
  let fileDirectives = FileDirectives (Just own) (Just dep) (Just cons)
      blockDirectives = BlockDirectives (Just own) (Just dep) (Just cons)
  in property $
     fromMaybe False (fdOwnership fileDirectives) === own .&&.
     fromMaybe False (fdDependentTypes fileDirectives) === dep .&&.
     fromMaybe False (fdConstraints fileDirectives) === cons .&&.
     fromMaybe False (bdOwnership blockDirectives) === own .&&.
     fromMaybe False (bdDependentTypes blockDirectives) === dep .&&.
     fromMaybe False (bdConstraints blockDirectives) === cons

-- ============================================================================
-- Test 4: Error Handling Properties
-- ============================================================================

-- Property: Error severity classification is consistent
prop_error_severity_consistent :: String -> ErrorSeverity -> Property
prop_error_severity_consistent msg severity =
  let error = TypeError msg severity Nothing
  in property $ 
     errorMessage error === msg .&&.
     errorSeverity error === severity

-- Property: Error location tracking preserves information
prop_error_location_preservation :: String -> SourcePos -> Property
prop_error_location_preservation msg pos =
  let location = ErrorLocation pos (posAfter pos 5)
      error = TypeError msg ErrorError (Just location)
  in case errorLocation error of
    Just loc -> property $ 
      errorStart loc === pos .&&.
      errorEnd loc === posAfter pos 5
    Nothing -> property False

-- ============================================================================
-- Test 5: Integration Properties
-- ============================================================================

-- Property: String processing pipeline is consistent
prop_string_pipeline_consistency :: String -> Property
prop_string_pipeline_consistency input =
  let step1 = trim input
      step2 = normalizeIndentation step1
      step3 = removeLineComments step2
  in property $ 
     not (null step1) ==> L.length step3 <= L.length step2 .&&.
     L.length step2 <= L.length step1

-- Property: Location tracking through processing
prop_location_tracking_consistency :: Positive Int -> String -> Property
prop_location_tracking_consistency (Positive offset) text =
  let start = startPos
      afterOffset = advancePosBy start 0 offset
      located = locatedWithSpan (spanFrom start) text
  in offset > 0 ==> 
     sourceLine afterOffset >= sourceLine start .&&.
     locatedValue located === text

-- ============================================================================
-- Test Suite Assembly
-- ============================================================================

tests :: TestTree
tests = testGroup "New Comprehensive QuickCheck Tests"
  [ testGroup "Utils Module Properties"
    [ fastProperty "splitBy relationship with splitByCollapsed" prop_split_by_relationship
    , fastProperty "trim is idempotent" prop_trim_idempotent  
    , fastProperty "removeLineComments preserves content" prop_remove_line_comments_preserves_content
    ]
    
  , testGroup "SourceLocation Properties"
    [ fastProperty "position advancement consistency" prop_pos_advancement_consistent
    , fastProperty "span merging associativity" prop_span_merging_associative
    , fastProperty "located values preserve content" prop_located_preserves_content
    ]
    
  , testGroup "Parser Properties"
    [ fastProperty "default directives consistency" prop_default_directives_consistent
    , fastProperty "directive construction preserves values" prop_directive_preserves_values
    ]
    
  , testGroup "Error Handling Properties"
    [ fastProperty "error severity consistency" prop_error_severity_consistent
    , fastProperty "error location preservation" prop_error_location_preservation
    ]
    
  , testGroup "Integration Properties"
    [ fastProperty "string pipeline consistency" prop_string_pipeline_consistency
    , fastProperty "location tracking consistency" prop_location_tracking_consistency
    ]
  ]