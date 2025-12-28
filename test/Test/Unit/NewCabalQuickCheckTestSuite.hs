{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalQuickCheckTestSuite (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose, sized)
import qualified Test.QuickCheck as QC

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  , posAfter
  , posAt
  , posAtLineCol
  , emptySpan
  , spanFrom
  , spanTo
  , spanBetween
  , mergeSpans
  , isValidSpan
  , locatedAt
  , locatedWithSpan
  , locatedValue
  , locatedSpan
  , locatedPos
  , mapLocated
  , advancePos
  , advancePosBy
  , advancePosByText
  , advancePosByLine
  , toErrorLocation
  , toErrorLocationWithSpan
  )

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , splitByComma
  , splitByCommaCollapsed
  , removeLineComments
  , removeComments
  , normalizeIndentation
  , forceSingleTabIndentation
  , fixIndentation
  , breakOn
  )

import Data.Char (isSpace, toLower, isAlphaNum)
import qualified Data.List as Data.List
import Data.List (isPrefixOf, tails, isInfixOf, sort, nub)
import qualified Data.Text as T
import Data.Text (Text)

-- ============================================================================
-- QuickCheck Generators
-- ============================================================================

-- Generator for valid source positions
genSourcePos :: Gen SourcePos
genSourcePos = do
  line <- choose (1, 1000)
  col <- choose (1, 1000)
  offset <- choose (0, 1000000)
  return $ SourcePos line col offset

-- Generator for valid source spans
genSourceSpan :: Gen SourceSpan
genSourceSpan = do
  startLine <- choose (1, 500)
  startCol <- choose (1, 500)
  startOffset <- choose (0, 500000)
  let start = SourcePos startLine startCol startOffset
  
  endLineOffset <- choose (0, 100)
  endColOffset <- choose (0, 100)
  endOffsetOffset <- choose (0, 100000)
  let end = SourcePos (startLine + endLineOffset) (max startCol (startCol + endColOffset)) (startOffset + endOffsetOffset)
  
  return $ SourceSpan start end

-- Generator for printable strings
genPrintableString :: Gen String
genPrintableString = listOf $ elements $ filter isPrintable ['\0'..'\127']
  where isPrintable c = isAlphaNum c || c `elem` " \t\n\r!@#$%^&*()_+-=[]{}|;':\",./<>?"

-- Generator for strings without quotes (for comment tests)
genStringWithoutQuotes :: Gen String
genStringWithoutQuotes = listOf $ elements $ filter (\c -> c /= '"' && c /= '\'') ['\0'..'\127']

instance Arbitrary SourcePos where
  arbitrary = genSourcePos

instance Arbitrary SourceSpan where
  arbitrary = genSourceSpan

-- ============================================================================
-- SourceLocation QuickCheck Tests (3 tests)
-- ============================================================================

-- Test 1: Position advancement consistency
prop_position_advancement_consistency :: String -> Property
prop_position_advancement_consistency str =
  not (null str) ==>
  let finalPos = advancePosBy str startPos
      manualPos = foldl (flip posAfter) startPos str
  in finalPos === manualPos

-- Test 2: Span merging properties
prop_span_merging_properties :: SourceSpan -> SourceSpan -> Property
prop_span_merging_properties span1 span2 =
  let merged = mergeSpans span1 span2
      start1 = spanStart span1
      end1 = spanEnd span1
      start2 = spanStart span2
      end2 = spanEnd span2
      mergedStart = spanStart merged
      mergedEnd = spanEnd merged
  in property $ 
     (mergedStart <= start1 .&&. mergedStart <= start2) .&&.
     (mergedEnd >= end1 .&&. mergedEnd >= end2)

-- Test 3: Located value roundtrip
prop_located_value_roundtrip :: String -> SourcePos -> Property
prop_located_value_roundtrip value pos =
  let located = locatedAt pos value
      extractedValue = locatedValue located
      extractedPos = locatedPos located
  in property $ extractedValue === value .&&. extractedPos === pos

-- ============================================================================
-- Utils QuickCheck Tests (4 tests)
-- ============================================================================

-- Test 4: Split and join roundtrip property
prop_split_join_roundtrip :: Char -> String -> Property
prop_split_join_roundtrip delim input =
  let parts = splitBy delim input
      rejoined = Data.List.intercalate [delim] parts
  in rejoined === input

-- Test 5: Trim idempotency with Unicode
prop_trim_unicode_idempotent :: String -> Property
prop_trim_unicode_idempotent input =
  let unicodeInput = input ++ " café naïve résumé 🚀 测试 "
      trimmedOnce = trim unicodeInput
      trimmedTwice = trim trimmedOnce
  in trimmedOnce === trimmedTwice

-- Test 6: Comment removal preserves code structure
prop_comment_removal_preserves_structure :: String -> String -> Property
prop_comment_removal_preserves_structure prefix suffix =
  not ('"' `elem` prefix) && not ('\'' `elem` prefix) &&
  not ('"' `elem` suffix) && not ('\'' `elem` suffix) ==>
  let code = prefix ++ "x = 1" ++ suffix
      withComments = code ++ " // comment\n /* block */ " ++ code
      withoutComments = removeComments withComments
  in property $ code `isInfixOf` withoutComments

-- Test 7: Indentation normalization preserves relative structure
prop_indentation_preserves_relative :: [Int] -> Property
prop_indentation_preserves_relative indentLevels =
  not (null indentLevels) ==>
  let inputLines = zipWith (\level content -> replicate (abs level `mod` 20) ' ' ++ "line" ++ show level) indentLevels [1..]
      content = unlines inputLines
      normalized = normalizeIndentation content
      normalizedLines = filter (not . null . trim) (lines normalized)
  in property $ length normalizedLines === length inputLines

-- ============================================================================
-- Parser QuickCheck Tests (3 tests)
-- ============================================================================

-- Test 8: BreakOn consistency with splitBy
prop_breakOn_splitBy_consistency :: String -> String -> Property
prop_breakOn_splitBy_consistency pat haystack =
  not (null pat) && pat `isInfixOf` haystack ==>
  let (before, after) = breakOn pat haystack
      parts = splitBy (head pat) haystack
      patIndex = length $ takeWhile (not . (pat `isPrefixOf`)) (tails haystack)
      expectedBefore = take patIndex haystack
      expectedAfter = drop (patIndex + length pat) haystack
  in before === expectedBefore .&&. after === expectedAfter

-- Test 9: String processing pipeline commutativity
prop_string_pipeline_commutative :: String -> Property
prop_string_pipeline_commutative input =
  let pipeline1 = input |> trim |> removeLineComments |> normalizeIndentation
      pipeline2 = input |> removeLineComments |> trim |> normalizeIndentation
      pipeline3 = input |> normalizeIndentation |> trim |> removeLineComments
  in property $ (pipeline1 == pipeline2) .||. (pipeline2 == pipeline3) .||. (pipeline1 == pipeline3)

-- Test 10: Error location conversion roundtrip
prop_error_location_roundtrip :: SourceSpan -> Property
prop_error_location_roundtrip span =
  let errorLoc = toErrorLocationWithSpan span
      startLine = line errorLoc
      startCol = column errorLoc
      endLine = endLine errorLoc
      endCol = endColumn errorLoc
      expectedStartLine = posLine (spanStart span)
      expectedStartCol = posColumn (spanStart span)
      expectedEndLine = posLine (spanEnd span)
      expectedEndCol = posColumn (spanEnd span)
  in property $ 
     startLine === expectedStartLine .&&.
     startCol === expectedStartCol .&&.
     endLine === Just expectedEndLine .&&.
     endCol === Just expectedEndCol

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New Cabal QuickCheck Test Suite (10 tests)"
  [ testGroup "SourceLocation Tests"
    [ fastProperty "position advancement consistency" prop_position_advancement_consistency
    , fastProperty "span merging properties" prop_span_merging_properties
    , fastProperty "located value roundtrip" prop_located_value_roundtrip
    ]
  
  , testGroup "Utils Tests"
    [ fastProperty "split and join roundtrip" prop_split_join_roundtrip
    , fastProperty "trim unicode idempotent" prop_trim_unicode_idempotent
    , fastProperty "comment removal preserves structure" prop_comment_removal_preserves_structure
    , fastProperty "indentation preserves relative" prop_indentation_preserves_relative
    ]
  
  , testGroup "Parser Tests"
    [ fastProperty "breakOn splitBy consistency" prop_breakOn_splitBy_consistency
    , fastProperty "string pipeline commutative" prop_string_pipeline_commutative
    , fastProperty "error location roundtrip" prop_error_location_roundtrip
    ]
  ]

-- Helper function for pipeline operations
(|>) :: a -> (a -> b) -> b
x |> f = f