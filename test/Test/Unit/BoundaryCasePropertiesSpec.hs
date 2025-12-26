{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.BoundaryCasePropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, assertBool, (@?=))
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary ()
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose, sized)
import qualified Test.QuickCheck as QC

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , removeComments
  , removeLineComments
  , breakOn
  , normalizeIndentation
  )

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  , posAfter
  , posAt
  , advancePos
  , advancePosBy
  , emptySpan
  , spanFrom
  , mergeSpans
  , isValidSpan
  , locatedAt
  , locatedWithSpan
  )

import Data.Char (isSpace, isAlphaNum, isLetter, isDigit, isControl)
import Data.List (sort, nub, intersperse, isInfixOf, isPrefixOf)
import qualified Data.Text as T

-- ============================================================================
-- Custom Generators for Edge Cases
-- ============================================================================

-- Generate strings with lots of whitespace
genWhitespaceString :: Gen String
genWhitespaceString = listOf $ elements " \t\n\r\v\f"

-- Generate strings with special characters
genSpecialCharString :: Gen String
genSpecialCharString = listOf $ elements "!@#$%^&*()_+-=[]{}|;':\",./<>?`~"

-- Generate strings with Unicode characters
genUnicodeString :: Gen String
genUnicodeString = listOf $ choose ('\x80', '\xFFFF')

-- Generate very long strings
genLongString :: Gen String
genLongString = sized $ \n -> listOf (elements "abc") >>= return . concat . replicate (max 1 (n `div` 10))

-- ============================================================================
-- Utils Boundary Cases
-- ============================================================================

-- Property: trim handles empty strings correctly
prop_trim_empty :: Property
prop_trim_empty =
  trim "" === ""

-- Property: trim handles all-whitespace strings correctly
prop_trim_all_whitespace :: Property
prop_trim_all_whitespace =
  forAll genWhitespaceString $ \ws ->
    trim ws === ""

-- Property: trim preserves non-whitespace content
prop_trim_preserves_content :: Property
prop_trim_preserves_content =
  forAll genSpecialCharString $ \content ->
    let withWhitespace = "  \t\n" ++ content ++ "\n\t  "
    in trim withWhitespace === content

-- Property: splitBy handles empty strings correctly
prop_splitBy_empty :: Char -> Property
prop_splitBy_empty delim =
  splitBy delim "" === [""]

-- Property: splitBy on single character works correctly
prop_splitBy_single_char :: Char -> Char -> Property
prop_splitBy_single_char delim c =
  c /= delim ==>
  splitBy delim [c] === [[c]]

-- Property: splitByCollapsed handles all delimiters correctly
prop_splitByCollapsed_all_delims :: Char -> Property
prop_splitByCollapsed_all_delims delim =
  let allDelims = replicate 5 delim
  in splitByCollapsed delim allDelims === []

-- Property: removeComments handles empty input
prop_removeComments_empty :: Property
prop_removeComments_empty =
  removeComments "" === ""

-- Property: removeComments handles only comments
prop_removeComments_only_comments :: Property
prop_removeComments_only_comments =
  let commentOnly = "// This is a comment\n/* This is a block comment */"
  in removeComments commentOnly === "\n\n"

-- Property: removeComments handles nested quotes in comments
prop_removeComments_nested_quotes :: Property
prop_removeComments_nested_quotes =
  let withQuotes = "// Comment with \"quotes\" and 'apostrophes'\n"
  in removeComments withQuotes === "\n"

-- Property: removeLineComments preserves content before comment
prop_removeLineComments_preserves_before :: String -> String -> Property
prop_removeLineComments_preserves_before before comment =
  let line = before ++ "// " ++ comment
      expected = if null before then "" else before
  in removeLineComments line === expected

-- Property: breakOn handles empty pattern
prop_breakOn_empty_pattern :: String -> Property
prop_breakOn_empty_pattern str =
  breakOn "" str === ("", str)

-- Property: breakOn handles pattern not found
prop_breakOn_not_found :: String -> String -> Property
prop_breakOn_not_found pat str =
  not (pat `isInfixOf` str) && not (null pat) ==>
  breakOn pat str === (str, "")

-- Property: normalizeIndentation handles empty input
prop_normalizeIndentation_empty :: Property
prop_normalizeIndentation_empty =
  normalizeIndentation "" === ""

-- Property: normalizeIndentation handles all-whitespace lines
prop_normalizeIndentation_all_whitespace :: Property
prop_normalizeIndentation_all_whitespace =
  forAll genWhitespaceString $ \ws ->
    let linesWithWs = lines ws
        normalized = lines (normalizeIndentation ws)
    in all null normalized

-- ============================================================================
-- SourceLocation Boundary Cases
-- ============================================================================

-- Property: posAfter handles all control characters
prop_posAfter_control_chars :: Property
prop_posAfter_control_chars =
  forAll (elements ['\0'..'\31']) $ \c ->
    let pos = startPos
        newPos = posAfter c pos
    in posOffset newPos === 1

-- Property: posAfter handles very high column positions
prop_posAfter_high_column :: Property
prop_posAfter_high_column =
  let highColPos = startPos { posColumn = 1000 }
      newPos = posAfter 'a' highColPos
  in posColumn newPos === 1001 .&&. posOffset newPos === 1

-- Property: advancePosBy handles empty strings
prop_advancePosBy_empty :: Property
prop_advancePosBy_empty =
  advancePosBy "" startPos === startPos

-- Property: advancePosBy handles very long strings
prop_advancePosBy_long :: Property
prop_advancePosBy_long =
  forAll genLongString $ \longStr ->
    let finalPos = advancePosBy longStr startPos
    in posOffset finalPos === length longStr

-- Property: mergeSpans handles zero-length spans
prop_mergeSpans_zero_length :: SourcePos -> Property
prop_mergeSpans_zero_length pos =
  let span1 = emptySpan pos
      span2 = emptySpan pos
      merged = mergeSpans span1 span2
  in merged === span1

-- Property: mergeSpans handles spans with large gaps
prop_mergeSpans_large_gap :: SourcePos -> Property
prop_mergeSpans_large_gap pos =
  let farPos = pos { posOffset = posOffset pos + 10000 }
      span1 = emptySpan pos
      span2 = emptySpan farPos
      merged = mergeSpans span1 span2
  in spanStart merged === pos .&&. spanEnd merged === farPos

-- Property: isValidSpan handles equal positions
prop_isValidSpan_equal_positions :: SourcePos -> Property
prop_isValidSpan_equal_positions pos =
  let span = SourceSpan pos pos
  in property $ isValidSpan span

-- ============================================================================
-- Combined Boundary Cases
-- ============================================================================

-- Property: String processing pipeline handles complex inputs
prop_processing_pipeline_complex :: Property
prop_processing_pipeline_complex =
  forAll genUnicodeString $ \unicodeStr ->
  forAll genSpecialCharString $ \specialStr ->
  forAll genWhitespaceString $ \ws ->
    let combined = ws ++ "\n// Comment\n" ++ unicodeStr ++ " /* Block */ " ++ specialStr
        processed1 = trim combined
        processed2 = removeComments processed1
        processed3 = normalizeIndentation processed2
    in not (null processed3)

-- Property: Position tracking through complex text
prop_position_tracking_complex :: Property
prop_position_tracking_complex =
  let complexText = "Line 1\n\tTabbed line\n  \"String with \\\"quotes\\\"\"\n// Comment\n/* Block\ncomment */\n"
      finalPos = foldl (flip posAfter) startPos complexText
  in posLine finalPos === 6 .&&. posOffset finalPos === length complexText

-- Property: Error recovery in malformed inputs
prop_error_recovery_malformed :: Property
prop_error_recovery_malformed =
  let malformed = "/* Unclosed comment\n\"Unclosed string\n'Unclosed char\n// Normal comment"
      processed = removeComments malformed
  in property $ not (null processed)

-- ============================================================================
-- Unit Tests for Specific Edge Cases
-- ============================================================================

test_trim_specific_cases :: TestTree
test_trim_specific_cases = testCase "trim specific cases" $ do
  trim "\t\n\r\v\f" @?= ""
  trim "  a  " @?= "a"
  trim "\t\n  hello world  \n\t" @?= "hello world"
  trim "no-whitespace" @?= "no-whitespace"

test_splitBy_specific_cases :: TestTree
test_splitBy_specific_cases = testCase "splitBy specific cases" $ do
  splitBy ',' "" @?= [""]
  splitBy ',' "a" @?= ["a"]
  splitBy ',' "a,b,c" @?= ["a", "b", "c"]
  splitBy ',' ",a," @?= ["", "a", ""]
  splitBy ',' "a,,b" @?= ["a", "", "b"]

test_removeComments_specific_cases :: TestTree
test_removeComments_specific_cases = testCase "removeComments specific cases" $ do
  removeComments "" @?= ""
  removeComments "// comment" @?= ""
  removeComments "/* comment */" @?= ""
  removeComments "code // comment\nmore code" @?= "code \nmore code"
  removeComments "\"string // not comment\"" @?= "\"string // not comment\""

test_source_location_specific_cases :: TestTree
test_source_location_specific_cases = testCase "source location specific cases" $ do
  let pos1 = posAfter '\n' startPos
  posLine pos1 @?= 2
  posColumn pos1 @?= 1
  
  let pos2 = posAfter '\t' startPos
  posColumn pos2 @?= 9
  
  let span1 = emptySpan startPos
  isValidSpan span1 @?= True

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "Boundary Case Properties"
  [ testGroup "Utils Boundary Cases"
    [ fastProperty "trim empty" prop_trim_empty
    , fastProperty "trim all whitespace" prop_trim_all_whitespace
    , fastProperty "trim preserves content" prop_trim_preserves_content
    , fastProperty "splitBy empty" prop_splitBy_empty
    , fastProperty "splitBy single char" prop_splitBy_single_char
    , fastProperty "splitByCollapsed all delims" prop_splitByCollapsed_all_delims
    , fastProperty "removeComments empty" prop_removeComments_empty
    , fastProperty "removeComments only comments" prop_removeComments_only_comments
    , fastProperty "removeComments nested quotes" prop_removeComments_nested_quotes
    , fastProperty "removeLineComments preserves before" prop_removeLineComments_preserves_before
    , fastProperty "breakOn empty pattern" prop_breakOn_empty_pattern
    , fastProperty "breakOn not found" prop_breakOn_not_found
    , fastProperty "normalizeIndentation empty" prop_normalizeIndentation_empty
    , fastProperty "normalizeIndentation all whitespace" prop_normalizeIndentation_all_whitespace
    ]
  , testGroup "SourceLocation Boundary Cases"
    [ fastProperty "posAfter control chars" prop_posAfter_control_chars
    , fastProperty "posAfter high column" prop_posAfter_high_column
    , fastProperty "advancePosBy empty" prop_advancePosBy_empty
    , fastProperty "advancePosBy long" prop_advancePosBy_long
    , fastProperty "mergeSpans zero length" prop_mergeSpans_zero_length
    , fastProperty "mergeSpans large gap" prop_mergeSpans_large_gap
    , fastProperty "isValidSpan equal positions" prop_isValidSpan_equal_positions
    ]
  , testGroup "Combined Boundary Cases"
    [ fastProperty "processing pipeline complex" prop_processing_pipeline_complex
    , fastProperty "position tracking complex" prop_position_tracking_complex
    , fastProperty "error recovery malformed" prop_error_recovery_malformed
    ]
  , testGroup "Specific Edge Cases"
    [ test_trim_specific_cases
    , test_splitBy_specific_cases
    , test_removeComments_specific_cases
    , test_source_location_specific_cases
    ]
  ]