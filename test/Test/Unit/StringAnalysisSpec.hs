{-# LANGUAGE CPP #-}

module Test.Unit.StringAnalysisSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Utils (trim, splitBy, splitByCollapsed, removeLineComments, removeComments, normalizeIndentation, breakOn)
import Data.Char (isSpace)
import qualified Data.List as L

-- ============================================================================
-- String Processing Tests
-- ============================================================================

-- Property: trim removes leading L.and trailing whitespace
prop_trim_removes_whitespace :: String -> String -> Property
prop_trim_removes_whitespace prefix suffix =
  let content = prefix ++ "content" ++ suffix
      trimmed = trim content
      hasLeading = L.any isSpace prefix
      hasTrailing = L.any isSpace suffix
      noLeadingSpace = null trimmed || not (isSpace (L.head trimmed))
      noTrailingSpace = null trimmed || not (isSpace (last trimmed))
  in classify hasLeading "has leading whitespace" $
     classify hasTrailing "has trailing whitespace" $
     property $ noLeadingSpace .&&. noTrailingSpace

-- Property: splitBy preserves empty segments
prop_splitBy_preserves_empty :: Char -> String -> Property
prop_splitBy_preserves_empty delim input =
  let result = splitBy delim input
      expectedCount = L.length (L.filter (== delim) input) + 1
  in property $ L.length result === expectedCount

-- Property: splitByCollapsed removes empty segments
prop_splitByCollapsed_removes_empty :: Char -> String -> Property
prop_splitByCollapsed_removes_empty delim input =
  let result = splitByCollapsed delim input
  in property $ L.all (not . null) result

-- Property: removeLineComments removes line comments
prop_removeLineComments_basic :: String -> String -> Property
prop_removeLineComments_basic code comment =
  not ('"' `elem` code) && not ('\'' `elem` code) ==>
  let lineWithComment = code ++ " // " ++ comment
      cleaned = removeLineComments lineWithComment
  in property $ cleaned === (code ++ " ")

-- Property: normalizeIndentation removes common leading whitespace
prop_normalizeIndentation_removes_common :: String -> Property
prop_normalizeIndentation_removes_common content =
  not (null content) && not (L.any (`elem` "\n\r") content) ==>
  let indented = "    " ++ content ++ "\n    " ++ content ++ "\n"
      normalized = normalizeIndentation indented
      lines' = lines normalized
  in property $ L.all (not . L.isPrefixOf "    ") (L.filter (not . null) lines')

-- Property: breakOn finds first occurrence
prop_breakOn_first :: String -> String -> String -> Property
prop_breakOn_first prefix delimiter suffix =
  not (null delimiter) ==>
  let full = prefix ++ delimiter ++ suffix ++ delimiter ++ "extra"
      (before, after) = breakOn delimiter full
  in property $ before === prefix ++ delimiter ++ suffix .&&. after === "extra"

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "String Analysis Tests"
  [ testGroup "Unit Tests"
    [ testCase "trim removes leading L.and trailing whitespace" $ do
        trim "\t  hello  world \n" @?= "hello  world"

    , testCase "splitBy preserves empty segments" $ do
        splitBy ':' "a::b:" @?= ["a", "", "b", ""]

    , testCase "splitByCollapsed removes empty segments" $ do
        splitByCollapsed ':' "::alpha::beta::" @?= ["alpha", "beta"]

    , testCase "breakOn returns prefix L.and suffix when the pattern exists" $ do
        breakOn "ll" "hello" @?= ("he", "o")

    , testCase "breakOn falls back to the original string when the pattern is missing" $ do
        breakOn "xyz" "hello" @?= ("hello", "")
    ]
  , testGroup "Property Tests"
    [ fastProperty "trim removes leading L.and trailing whitespace" prop_trim_removes_whitespace
    , fastProperty "splitBy preserves empty segments" prop_splitBy_preserves_empty
    , fastProperty "splitByCollapsed removes empty segments" prop_splitByCollapsed_removes_empty
    , fastProperty "removeLineComments removes line comments" prop_removeLineComments_basic
    , fastProperty "normalizeIndentation removes common leading whitespace" prop_normalizeIndentation_removes_common
    , fastProperty "breakOn finds first occurrence" prop_breakOn_first
    ]
  ]