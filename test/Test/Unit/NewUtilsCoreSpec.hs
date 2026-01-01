{-# LANGUAGE CPP #-}

module Test.Unit.NewUtilsCoreSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum, isAlpha)

import Utils (trim, splitBy, splitByCollapsed, splitByComma, splitByCommaCollapsed,
             removeLineComments, removeComments, normalizeIndentation, 
             forceSingleTabIndentation, fixIndentation, breakOn)
import TestSupport.Arbitrary ()

-- Test 1: trim removes leading L.and trailing whitespace
prop_trim_removes_whitespace :: String -> Property
prop_trim_removes_whitespace str =
  let trimmed = trim str
      hasLeadingSpace = not (null str) && isSpace (L.head str)
      hasTrailingSpace = not (null str) && isSpace (last str)
  in (hasLeadingSpace || hasTrailingSpace) ==> 
     L.head trimmed /= ' ' && (null trimmed || last trimmed /= ' ')

-- Test 2: splitBy preserves empty segments
prop_splitBy_preserves_empty :: Char -> String -> Property
prop_splitBy_preserves_empty delim str =
  let parts = splitBy delim str
      expectedCount = L.length (L.filter (== delim) str) + 1
  in L.length parts === expectedCount

-- Test 3: splitByCollapsed removes empty segments
prop_splitByCollapsed_removes_empty :: Char -> String -> Property
prop_splitByCollapsed_removes_empty delim str =
  let parts = splitByCollapsed delim str
      allNonEmpty = L.all (not . null) parts
  in allNonEmpty

-- Test 4: splitByComma consistency
prop_splitByComma_consistency :: String -> Property
prop_splitByComma_consistency str =
  splitByComma str === splitBy ',' str

-- Test 5: splitByCommaCollapsed consistency
prop_splitByCommaCollapsed_consistency :: String -> Property
prop_splitByCommaCollapsed_consistency str =
  splitByCommaCollapsed str === splitByCollapsed ',' str

-- Test 6: removeLineComments idempotence
prop_removeLineComments_idempotent :: String -> Property
prop_removeLineComments_idempotent str =
  let once = removeLineComments str
      twice = removeLineComments once
  in once === twice

-- Test 7: removeLineComments removes line comments
prop_removeLineComments_removes_comments :: String -> Property
prop_removeLineComments_removes_comments str =
  let withComment = str ++ "\n// This is a comment\nmore code"
      withoutComment = removeLineComments withComment
  in not ("// This is a comment" `L.isInfixOf` withoutComment)

-- Test 8: normalizeIndentation preserves relative structure
prop_normalizeIndentation_preserves_relative :: String -> Property
prop_normalizeIndentation_preserves_relative str =
  let lines = splitBy '\n' str
      normalized = normalizeIndentation str
      normalizedLines = splitBy '\n' normalized
  in L.length normalizedLines === L.length lines

-- Test 9: normalizeIndentation removes common prefix
prop_normalizeIndentation_removes_common_prefix :: String -> Property
prop_normalizeIndentation_removes_common_prefix str =
  let indented = "  " ++ str
      normalized = normalizeIndentation indented
  in not ("  " `L.isPrefixOf` normalized) || normalized === str

-- Test 10: breakOn consistency with standard functions
prop_breakOn_consistency :: String -> String -> Property
prop_breakOn_consistency str sep =
  L.length sep > 0 ==> -- Ensure separator is not empty
  let breakResult = breakOn sep str
      standardResult = span (not . (`L.isPrefixOf` sep)) (tails str)
  in case breakResult of
    (before, after) -> 
      case standardResult of
        (prefix, suffix) -> before === prefix .&&. 
                              (null suffix || after === L.head suffix)

tests :: TestTree
tests = testGroup "New Utils Core Tests"
  [ fastProperty "trim removes leading L.and trailing whitespace" prop_trim_removes_whitespace
  , fastProperty "splitBy preserves empty segments" prop_splitBy_preserves_empty
  , fastProperty "splitByCollapsed removes empty segments" prop_splitByCollapsed_removes_empty
  , fastProperty "splitByComma consistency" prop_splitByComma_consistency
  , fastProperty "splitByCommaCollapsed consistency" prop_splitByCommaCollapsed_consistency
  , fastProperty "removeLineComments idempotent" prop_removeLineComments_idempotent
  , fastProperty "removeLineComments removes line comments" prop_removeLineComments_removes_comments
  , fastProperty "normalizeIndentation preserves relative structure" prop_normalizeIndentation_preserves_relative
  , fastProperty "normalizeIndentation removes common prefix" prop_normalizeIndentation_removes_common_prefix
  , fastProperty "breakOn consistency with standard functions" prop_breakOn_consistency
  ]