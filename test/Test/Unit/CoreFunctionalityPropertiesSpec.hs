{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.CoreFunctionalityPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, ioProperty, (===), (.&&.), counterexample)
import Test.Tasty.HUnit (testCase, assertBool)

import qualified Data.Text as T
import Data.Char (isSpace)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)

import Utils (trim, splitBy, splitByCollapsed, removeLineComments, removeComments, normalizeIndentation, breakOn)
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, posAfter, spanBetween, mergeSpans, isValidSpan, advancePosByText)
import Compiler.Errors.Core (TypeError(..), ErrorSeverity(..), ErrorCategory(..), ErrorLocation(..), emptyContext, errorAt, withLocation, withContext, formatError, filterBySeverity, filterByCategory)

-- ============================================================================
-- Utils Module Properties
-- ============================================================================

-- Property: trim should not change already trimmed strings
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s = trim (trim s) === trim s

-- Property: splitBy should reconstruct original string with delimiter
prop_splitBy_reconstruct :: Char -> String -> Property
prop_splitBy_reconstruct delim s = 
    let parts = splitBy delim s
        reconstructed = intercalate [delim] parts
    in reconstructed === s
  where
    intercalate :: String -> [String] -> String
    intercalate _ [] = ""
    intercalate _ [x] = x
    intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

-- Property: splitByCollapsed should not contain empty strings
prop_splitByCollapsed_no_empty :: Char -> String -> Property
prop_splitByCollapsed_no_empty delim s = 
    let parts = splitByCollapsed delim s
    in not (any null parts)

-- Property: removeLineComments should remove all line comments
prop_removeLineComments_basic :: String -> Property
prop_removeLineComments_basic s = 
    let withComments = s ++ "\n// This is a comment\n// Another comment"
        withoutComments = removeLineComments withComments
    in not ("// This is a comment" `isInfixOf` withoutComments) .&&.
       not ("// Another comment" `isInfixOf` withoutComments)

-- Property: removeComments should preserve string literals containing comment markers
prop_removeComments_preserve_strings :: String -> Property
prop_removeComments_preserve_strings s = 
    let stringWithCommentInString = "print(\"// not a comment\"); // real comment"
        processed = removeComments stringWithCommentInString
    in "// not a comment" `isInfixOf` processed

-- Property: normalizeIndentation should preserve relative indentation
prop_normalizeIndentation_preserve_relative :: String -> Property
prop_normalizeIndentation_preserve_relative s = 
    let indented = "  " ++ s ++ "\n    " ++ s ++ "  \n  " ++ s
        normalized = normalizeIndentation indented
        lines' = lines normalized
    in length lines' === 3 .&&. 
       all (`notElem` "\t") (concat lines')

-- Property: breakOn should correctly split strings
prop_breakOn_correct :: String -> String -> Property
prop_breakOn_correct pat s = 
    let (before, after) = breakOn pat s
        reconstructed = before ++ pat ++ after
    in if pat `isInfixOf` s
       then reconstructed === s
       else before === s .&&. after === ""

-- ============================================================================
-- SourceLocation Module Properties
-- ============================================================================

-- Property: posAfter should advance line number for newline characters
prop_posAfter_newline :: SourcePos -> Property
prop_posAfter_newline pos = 
    let newPos = posAfter '\n' pos
    in posLine newPos === posLine pos + 1 .&&.
       posColumn newPos === 1 .&&.
       posOffset newPos === posOffset pos + 1

-- Property: spanBetween should create valid spans
prop_spanBetween_valid :: SourcePos -> SourcePos -> Property
prop_spanBetween_valid pos1 pos2 = 
    let span = spanBetween pos1 pos2
    in isValidSpan span === (pos1 <= pos2)

-- Property: mergeSpans should be commutative for overlapping spans
prop_mergeSpans_commutative :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_commutative span1 span2 = 
    let merged1 = mergeSpans span1 span2
        merged2 = mergeSpans span2 span1
    in merged1 === merged2

-- Property: advancePosByText should be consistent with individual character advances
prop_advancePosByText_consistent :: String -> Property
prop_advancePosByText_consistent s = 
    let text = T.pack s
        advanceByChars = foldl (flip posAfter) startPos s
        advanceByText = advancePosByText text startPos
    in advanceByChars === advanceByText

-- ============================================================================
-- Error Handling Properties
-- ============================================================================

-- Property: error formatting should include severity information
prop_error_format_includes_severity :: String -> T.Text -> Property
prop_error_format_includes_severity errId msg = 
    let err = errorAt errId msg (ErrorLocation Nothing 1 1 Nothing Nothing)
        formatted = formatError err
    in "ERROR" `isInfixOf` formatted

-- Property: filtering by severity should be consistent
prop_filter_severity_consistent :: [TypeError] -> ErrorSeverity -> Property
prop_filter_severity_consistent errors sev = 
    let filtered = filterBySeverity sev errors
        allMatch = all (\e -> severity e == sev) filtered
    in allMatch === True

-- Property: filtering by category should be consistent
prop_filter_category_consistent :: [TypeError] -> ErrorCategory -> Property
prop_filter_category_consistent errors cat = 
    let filtered = filterByCategory cat errors
        allMatch = all (\e -> category e == cat) filtered
    in allMatch === True

-- Property: withLocation should update error location
prop_withLocation_updates_location :: String -> T.Text -> ErrorLocation -> ErrorLocation -> Property
prop_withLocation_updates_location errId msg loc1 loc2 = 
    let err = errorAt errId msg loc1
        updatedErr = withLocation err loc2
    in location updatedErr === loc2

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "Core Functionality Properties"
    [ testGroup "Utils Module Properties"
        [ testProperty "trim idempotent" prop_trim_idempotent
        , testProperty "splitBy reconstruct" prop_splitBy_reconstruct
        , testProperty "splitByCollapsed no empty" prop_splitByCollapsed_no_empty
        , testProperty "removeLineComments basic" prop_removeLineComments_basic
        , testProperty "removeComments preserve strings" prop_removeComments_preserve_strings
        , testProperty "normalizeIndentation preserve relative" prop_normalizeIndentation_preserve_relative
        , testProperty "breakOn correct" prop_breakOn_correct
        ]
    , testGroup "SourceLocation Module Properties"
        [ testProperty "posAfter newline" prop_posAfter_newline
        , testProperty "spanBetween valid" prop_spanBetween_valid
        , testProperty "mergeSpans commutative" prop_mergeSpans_commutative
        , testProperty "advancePosByText consistent" prop_advancePosByText_consistent
        ]
    , testGroup "Error Handling Properties"
        [ testProperty "error format includes severity" prop_error_format_includes_severity
        , testProperty "filter severity consistent" prop_filter_severity_consistent
        , testProperty "filter category consistent" prop_filter_category_consistent
        , testProperty "withLocation updates location" prop_withLocation_updates_location
        ]
    ]