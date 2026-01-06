{-# LANGUAGE CPP #-}

module Test.Unit.NewEnhancedTestSuiteSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)

import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (==>))

import Utils (trim, splitBy, splitByCollapsed, splitByComma, removeLineComments, removeComments, normalizeIndentation)
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, posAfter, spanFrom, spanTo, mergeSpans, isValidSpan)
import qualified Data.Text as T
import Data.Char (isSpace)

-- ============================================================================
-- Utils Module Tests
-- ============================================================================

-- Test trim function properties
prop_trim_idempotent :: String -> Bool
prop_trim_idempotent s = trim (trim s) == trim s

prop_trim_no_leading_trailing_spaces :: String -> Bool
prop_trim_no_leading_trailing_spaces s = 
    let trimmed = trim s
    in null trimmed || (not (isSpace (L.head trimmed)) && not (isSpace (last trimmed)))

prop_trim_preserves_internal_spaces :: String -> Bool  
prop_trim_preserves_internal_spaces s =
    let trimmed = trim s
        spacesInside = dropWhile isSpace . dropWhileEnd isSpace $ s
        dropWhileEnd p = L.reverse . dropWhile p . L.reverse
    in L.length (filter isSpace spacesInside) == L.length (filter isSpace trimmed)

-- Test splitBy function properties
prop_splitBy_concatenation :: Char -> String -> Bool
prop_splitBy_concatenation delim s = L.concat (splitBy delim s) == s

prop_splitBy_empty_segments :: Char -> Bool
prop_splitBy_empty_segments delim = splitBy delim [delim, delim] == ["", "", ""]

prop_splitBy_preserves_order :: Char -> String -> Bool
prop_splitBy_preserves_order delim s = 
    let parts = splitBy delim s
        reconstructed = intercalate [delim] parts
    in reconstructed == s
  where
    intercalate _ [] = []
    intercalate _ [x] = x
    intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

-- Test splitByCollapsed function properties
prop_splitByCollapsed_no_empty_segments :: Char -> String -> Bool
prop_splitByCollapsed_no_empty_segments delim s = L.all (not . null) (splitByCollapsed delim s)

prop_splitByCollapsed_concatenation_with_delim :: Char -> String -> Bool
prop_splitByCollapsed_concatenation_with_delim delim s =
    let parts = splitByCollapsed delim s
    in if null parts then True else L.concat (intersperse [delim] parts) == L.filter (/= delim) s
  where
    intersperse _ [] = []
    intersperse _ [x] = [x]
    intersperse sep (x:xs) = x ++ sep ++ intersperse sep xs

-- Test removeLineComments function properties
prop_removeLineComments_no_comments :: String -> Property
prop_removeLineComments_no_comments s = not ("//" `L.isInfixOf` s) ==> removeLineComments s == s

prop_removeLineComments_preserves_non_comment_content :: String -> String -> Property
prop_removeLineComments_preserves_non_comment_content s content =
    let testInput = content ++ "// comment\n" ++ content
        result = removeLineComments testInput
    in content `L.isInfixOf` result

-- ============================================================================
-- SourceLocation Module Tests  
-- ============================================================================

-- Test SourcePos properties
prop_posAfter_advances_column :: Int -> Int -> Property
prop_posAfter_advances_column line col =
    col >= 0 && line >= 0 ==>
    let pos = SourcePos line col
        pos' = posAfter pos
    in posLine pos' == line && posColumn pos' == col + 1

prop_spanFrom_creates_valid_span :: Int -> Int -> Property
prop_spanFrom_creates_valid_span line col =
    line >= 0 && col >= 0 ==>
    let pos = SourcePos line col 0
        span = spanFrom pos
    in isValidSpan span && spanStart span == pos && spanEnd span == pos

prop_mergeSpans_properties :: Int -> Int -> Int -> Int -> Property
prop_mergeSpans_properties line1 col1 line2 col2 =
    L.all (>= 0) [line1, col1, line2, col2] ==>
    let pos1 = SourcePos line1 col1 0
        pos2 = SourcePos line2 col2 0
        span1 = spanFrom pos1
        span2 = spanFrom pos2
        merged = mergeSpans span1 span2
    in isValidSpan merged && 
       spanStart merged == min pos1 pos2 &&
       spanEnd merged == max (spanEnd span1) (spanEnd span2)

-- ============================================================================
-- Parser Integration Tests
-- ============================================================================

-- Test indentation normalization
test_normalizeIndentation_basic :: IO ()
test_normalizeIndentation_basic = do
    let input = "    line1\n        line2\n    line3"
        expected = "line1\n    line2\nline3"
        result = normalizeIndentation input
    assertEqual "Basic indentation normalization" expected result

test_normalizeIndentation_empty :: IO ()
test_normalizeIndentation_empty = do
    let input = ""
        expected = ""
        result = normalizeIndentation input
    assertEqual "Empty string normalization" expected result

test_normalizeIndentation_mixed :: IO ()
test_normalizeIndentation_mixed = do
    let input = "\t    line1\n        \tline2\n    line3"
        result = normalizeIndentation input
    assertBool "Mixed indentation should be normalized" (L.length result > 0)

-- ============================================================================
-- QuickCheck Utilities
-- ============================================================================

isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf needle haystack = L.any (isPrefixOf needle) (tails haystack)
  where
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
    tails [] = [[]]
    tails xs@(_:ys) = xs : tails ys

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "New Enhanced Test Suite"
  [ testGroup "Utils Module Tests"
      [ fastProperty "trim idempotent" prop_trim_idempotent
      , fastProperty "trim removes leading/trailing spaces" prop_trim_no_leading_trailing_spaces
      , fastProperty "trim preserves internal spaces" prop_trim_preserves_internal_spaces
      , fastProperty "splitBy concatenation property" prop_splitBy_concatenation
      , fastProperty "splitBy creates empty segments" prop_splitBy_empty_segments
      , fastProperty "splitBy preserves order" prop_splitBy_preserves_order
      , fastProperty "splitByCollapsed has no empty segments" prop_splitByCollapsed_no_empty_segments
      , fastProperty "splitByCollapsed concatenation" prop_splitByCollapsed_concatenation_with_delim
      , fastProperty "removeLineComments preserves non-comment content" prop_removeLineComments_preserves_non_comment_content
      ]
  , testGroup "SourceLocation Module Tests"
      [ fastProperty "posAfter advances column" prop_posAfter_advances_column
      , fastProperty "spanFrom creates valid span" prop_spanFrom_creates_valid_span
      , fastProperty "mergeSpans properties" prop_mergeSpans_properties
      ]
  , testGroup "Parser Integration Tests"
      [ testCase "normalizeIndentation basic" test_normalizeIndentation_basic
      , testCase "normalizeIndentation empty" test_normalizeIndentation_empty
      , testCase "normalizeIndentation mixed" test_normalizeIndentation_mixed
      ]
  ]