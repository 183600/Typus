{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewAdditionalQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), Positive(..), NonEmptyList(..))

import Utils (trim, splitBy, splitByCollapsed)
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, emptySpan, mergeSpans, isValidSpan, advancePos, posLine, posColumn)
import Parser (FileDirectives(..), BlockDirectives(..), defaultFileDirectives, defaultBlockDirectives)
import ErrorHandler (ErrorSeverity(..))

import Data.Char (isSpace)
import Data.List (null, head, last, all, length, concat)

-- ============================================================================
-- Utils Module Tests
-- ============================================================================

-- Property: trim idempotency
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s =
  let trimmed = trim s
      trimmedTwice = trim trimmed
  in property $ trimmed === trimmedTwice

-- Property: splitBy vs splitByCollapsed relationship
prop_splitBy_vs_collapsed :: Char -> String -> Property
prop_splitBy_vs_collapsed delim input =
  let splitResult = splitBy delim input
      collapsedResult = splitByCollapsed delim input
      hasEmptyStrings = any null splitResult
  in property $ if hasEmptyStrings 
                then length collapsedResult < length splitResult
                else collapsedResult === splitResult

-- ============================================================================
-- SourceLocation Module Tests
-- ============================================================================

-- Property: advancePos handles newline correctly
prop_advance_pos_newline :: SourcePos -> Property
prop_advance_pos_newline pos =
  let newPos = advancePos '\n' pos
  in property $ posLine newPos === posLine pos + 1 .&&.
                posColumn newPos === 1

-- Property: advancePos handles tab correctly
prop_advance_pos_tab :: SourcePos -> Property
prop_advance_pos_tab pos =
  let newPos = advancePos '\t' pos
      expectedCol = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in property $ posLine newPos === posLine pos .&&.
                posColumn newPos === expectedCol

-- Property: mergeSpans creates valid span
prop_merge_spans_valid :: SourceSpan -> SourceSpan -> Property
prop_merge_spans_valid span1 span2 =
  let merged = mergeSpans span1 span2
  in property $ isValidSpan merged

-- ============================================================================
-- Parser Module Tests
-- ============================================================================

-- Property: Default directives are all Nothing
prop_default_directives_nothing :: Property
prop_default_directives_nothing =
  let fileDirectives = defaultFileDirectives
      blockDirectives = defaultBlockDirectives
  in property $ (Nothing == fdOwnership fileDirectives) .&&.
                (Nothing == fdDependentTypes fileDirectives) .&&.
                (Nothing == fdConstraints fileDirectives) .&&.
                (Nothing == bdOwnership blockDirectives) .&&.
                (Nothing == bdDependentTypes blockDirectives) .&&.
                (Nothing == bdConstraints blockDirectives)

-- ============================================================================
-- Error Handling Tests
-- ============================================================================

-- Property: Error severity comparison works
prop_error_severity_comparison :: ErrorSeverity -> ErrorSeverity -> Property
prop_error_severity_comparison sev1 sev2 =
  let severityOrder = [Info, Warning, Error, Fatal]
      sev1Index = length $ takeWhile (/= sev1) severityOrder
      sev2Index = length $ takeWhile (/= sev2) severityOrder
  in property $ (sev1 == sev2) === (sev1Index == sev2Index)

-- Test collection
tests :: TestTree
tests = testGroup "New Additional QuickCheck Tests"
  [ testGroup "Utils Module"
    [ fastProperty "trim idempotent" prop_trim_idempotent
    , fastProperty "splitBy vs splitByCollapsed" prop_splitBy_vs_collapsed
    ]
  , testGroup "SourceLocation Module"
    [ fastProperty "advancePos handles newline" prop_advance_pos_newline
    , fastProperty "advancePos handles tab" prop_advance_pos_tab
    , fastProperty "mergeSpans creates valid span" prop_merge_spans_valid
    ]
  , testGroup "Parser Module"
    [ fastProperty "default directives are nothing" prop_default_directives_nothing
    ]
  , testGroup "Error Handling"
    [ fastProperty "error severity comparison" prop_error_severity_comparison
    ]
  ]