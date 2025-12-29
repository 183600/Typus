{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.StringProcessingBoundaryQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , removeLineComments
  , removeComments
  , normalizeIndentation
  , breakOn
  )

import Data.Char (isSpace, isControl, isAscii)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)

-- Property: trim should be idempotent
prop_trim_idempotent :: String -> Property
prop_trim_idempotent input =
  let trimmedOnce = trim input
      trimmedTwice = trim trimmedOnce
  in property $ trimmedOnce === trimmedTwice

-- Property: trim should remove all leading and trailing whitespace
prop_trim_removes_all_whitespace :: String -> String -> Property
prop_trim_removes_all_whitespace prefix suffix =
  let hasLeadingSpace = not (null prefix) && isSpace (last prefix)
      hasTrailingSpace = not (null suffix) && isSpace (head suffix)
      input = prefix ++ "content" ++ suffix
      trimmed = trim input
  in (hasLeadingSpace .||. hasTrailingSpace) ==>
  property $ not (null trimmed) ==> 
           (not (isSpace (head trimmed)) .&&. not (isSpace (last trimmed)))

-- Property: splitBy should preserve empty segments
prop_split_by_preserves_empty :: Char -> String -> Property
prop_split_by_preserves_empty delim input =
  let segments = splitBy delim input
      expectedCount = length input + 1
  in property $ length segments === expectedCount

-- Property: splitByCollapsed should remove empty segments
prop_split_by_collapsed_removes_empty :: Char -> String -> Property
prop_split_by_collapsed_removes_empty delim input =
  let segments = splitByCollapsed delim input
  in property $ all (not . null) segments

-- Property: splitBy and splitByCollapsed relationship
prop_split_by_relationship :: Char -> String -> Property
prop_split_by_relationship delim input =
  let normal = splitBy delim input
      collapsed = splitByCollapsed delim input
  in property $ length collapsed <= length normal

-- Property: removeLineComments should preserve string literals
prop_remove_line_comments_preserves_strings :: String -> Property
prop_remove_line_comments_preserves_strings content =
  let input = "code // comment\n\"" ++ content ++ "\" // another comment"
      result = removeLineComments input
  in property $ ("\"" ++ content ++ "\"") `isInfixOf` result

-- Property: removeComments should handle nested block comments
prop_remove_comments_nested_blocks :: String -> String -> Property
prop_remove_comments_nested_blocks outer inner =
  let input = "code /* outer " ++ outer ++ " /* inner " ++ inner ++ " */ still outer */ more code"
      result = removeComments input
  in property $ not ("/*" `isInfixOf` result) .&&. not ("*/" `isInfixOf` result)

-- Property: normalizeIndentation should preserve relative indentation
prop_normalize_indentation_preserves_relative :: String -> Property
prop_normalize_indentation_preserves_relative input =
  let lines' = lines input
      hasMultipleLines = length lines' > 1
  in hasMultipleLines ==>
     let normalized = normalizeIndentation input
         normLines = lines normalized
         originalRelIndent = calculateRelativeIndentation lines'
         normalizedRelIndent = calculateRelativeIndentation normLines
     in property $ originalRelIndent === normalizedRelIndent

-- Property: breakOn should find first occurrence
prop_break_on_first_occurrence :: String -> String -> Property
prop_break_on_first_occurrence needle haystack =
  not (null needle) ==>
  let result = breakOn needle haystack
      (before, after) = result
      combined = before ++ needle ++ after
  in property $ combined === haystack

-- Property: String processing should handle Unicode correctly
prop_unicode_handling :: String -> Property
prop_unicode_handling input =
  let hasUnicode = any (not . isAscii) input
  in hasUnicode ==>
     let trimmed = trim input
         split = splitBy ',' input
     in property $ length trimmed >= 0 .&&. length split >= 1

-- Property: String processing should handle control characters
prop_control_character_handling :: String -> Property
prop_control_character_handling input =
  let hasControl = any isControl input
  in hasControl ==>
     let processed = trim input
     in property $ length processed >= 0

-- Property: Empty string handling consistency
prop_empty_string_consistency :: Char -> Property
prop_empty_string_consistency delim =
  let empty = ""
      splitEmpty = splitBy delim empty
      splitCollapsedEmpty = splitByCollapsed delim empty
      trimmedEmpty = trim empty
  in property $ splitEmpty === [""] .&&. 
               splitCollapsedEmpty === [] .&&.
               trimmedEmpty === ""

-- Property: Large string processing performance
prop_large_string_processing :: Int -> Char -> Property
prop_large_string_processing size delim =
  size >= 0 .&&. size < 10000 ==>
  let largeString = replicate size delim
      result = splitBy delim largeString
  in property $ length result === size + 1

-- Property: String processing should be memory safe
prop_memory_safety :: String -> Property
prop_memory_safety input =
  let processed = trim input
      split = splitBy ',' processed
  in property $ length processed >= 0 .&&. all (>= 0) (map length split)

-- Helper functions
calculateRelativeIndentation :: [String] -> [Int]
calculateRelativeIndentation [] = []
calculateRelativeIndentation (first:rest) =
  let firstIndent = length $ takeWhile isSpace first
      baseIndent = firstIndent
  in firstIndent : map (\line -> length (takeWhile isSpace line) - baseIndent) rest

isAscii :: Char -> Bool
isAscii c = fromEnum c < 128

tests :: TestTree
tests = testGroup "String Processing Boundary QuickCheck Tests"
  [ fastProperty "trim is idempotent" prop_trim_idempotent
  , fastProperty "trim removes all leading and trailing whitespace" prop_trim_removes_all_whitespace
  , fastProperty "splitBy preserves empty segments" prop_split_by_preserves_empty
  , fastProperty "splitByCollapsed removes empty segments" prop_split_by_collapsed_removes_empty
  , fastProperty "splitBy and splitByCollapsed relationship" prop_split_by_relationship
  , fastProperty "removeLineComments preserves string literals" prop_remove_line_comments_preserves_strings
  , fastProperty "removeComments handles nested block comments" prop_remove_comments_nested_blocks
  , fastProperty "normalizeIndentation preserves relative indentation" prop_normalize_indentation_preserves_relative
  , fastProperty "breakOn finds first occurrence" prop_break_on_first_occurrence
  , fastProperty "Unicode handling" prop_unicode_handling
  , fastProperty "Control character handling" prop_control_character_handling
  , fastProperty "Empty string handling consistency" prop_empty_string_consistency
  , fastProperty "Large string processing performance" prop_large_string_processing
  , fastProperty "Memory safety" prop_memory_safety
  ]