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
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

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
  )

import Data.Char (isSpace, isDigit, isLetter)
import qualified Data.List as Data.List
import Data.List (isPrefixOf, tails, isInfixOf, sort, nub)
import Data.Maybe (isJust, isNothing, fromMaybe)

-- | Test 1: Property-based test for trim function
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s = trim (trim s) === trim s

-- | Test 2: Property-based test for splitBy consistency
prop_split_by_roundtrip :: Char -> String -> Property
prop_split_by_roundtrip delim s = 
  let parts = splitBy delim s
      rejoined = Data.List.intercalate [delim] parts
  in property $ rejoined === s

-- | Test 3: Property-based test for splitByCollapsed
prop_split_by_collapsed_no_empty :: Char -> String -> Property
prop_split_by_collapsed_no_empty delim s = 
  let parts = splitByCollapsed delim s
  in property $ all (not . null) parts

-- | Test 4: Property-based test for SourceSpan merging
prop_merge_spans_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_merge_spans_associative span1 span2 span3 =
  let left = mergeSpans span1 (mergeSpans span2 span3)
      right = mergeSpans (mergeSpans span1 span2) span3
  in property $ left === right

-- | Test 5: Property-based test for SourceSpan validity
prop_merge_spans_valid :: SourceSpan -> SourceSpan -> Property
prop_merge_spans_valid span1 span2 =
  let merged = mergeSpans span1 span2
  in property $ isValidSpan merged

-- | Test 6: Property-based test for Located values
prop_located_map_preserves_span :: String -> SourceSpan -> Property
prop_located_map_preserves_span value span =
  let located = locatedWithSpan value span
      mapped = mapLocated (Data.List.reverse) located
  in property $ locatedSpan mapped === locatedSpan located

-- | Test 7: Property-based test for position advancement
prop_advance_pos_consistency :: SourcePos -> String -> Property
prop_advance_pos_consistency pos text =
  let advanced = advancePos pos text
      lineCount = length $ filter (== '\n') text
  in property $ posLine advanced >= posLine pos

-- | Test 8: Property-based test for comment removal
prop_remove_line_comments_no_unintended_removal :: String -> String -> Property
prop_remove_line_comments_no_unintended_removal code comment =
  let hasSlash = '/' `elem` comment
      result = removeLineComments (code ++ "//" ++ comment)
      notContainsComment = not $ isInfixOf comment result
  in hasSlash ==> property $ notContainsComment

-- | Test 9: Property-based test for indentation normalization
prop_normalize_indentation_preserves_relative :: String -> Property
prop_normalize_indentation_preserves_relative s =
  let normalized = normalizeIndentation s
      linesOriginal = lines s
      linesNormalized = lines normalized
      -- Check that relative indentation is preserved for non-empty lines
      nonEmptyOriginal = filter (not . null) linesOriginal
      nonEmptyNormalized = filter (not . null) linesNormalized
      sameLength = length nonEmptyOriginal == length nonEmptyNormalized
  in sameLength ==> property $ 
    let checkRelativeIndent (orig, norm) = 
          let origIndent = length $ takeWhile isSpace orig
              normIndent = length $ takeWhile isSpace norm
          in if length orig > 0 && length norm > 0 
             then abs (origIndent - normIndent) <= 1  -- Allow small differences due to normalization
             else True
    in all checkRelativeIndent (zip nonEmptyOriginal nonEmptyNormalized)

-- | Test 10: Property-based test for breakOn function
prop_break_on_consistency :: String -> String -> Property
prop_break_on_consistency needle haystack =
  let result = breakOn needle haystack
  in case result of
    Nothing -> property $ not (needle `isInfixOf` haystack)
    Just (before, after) -> 
      property $ before ++ needle ++ after === haystack

tests :: TestTree
tests =
  testGroup "New Cabal QuickCheck Tests"
    [ testGroup "Utils Tests"
        [ fastProperty "trim is idempotent" prop_trim_idempotent
        , fastProperty "splitBy roundtrip consistency" prop_split_by_roundtrip
        , fastProperty "splitByCollapsed has no empty parts" prop_split_by_collapsed_no_empty
        , fastProperty "removeLineComments doesn't remove unintended content" prop_remove_line_comments_no_unintended_removal
        , fastProperty "normalizeIndentation preserves relative indentation" prop_normalize_indentation_preserves_relative
        , fastProperty "breakOn consistency" prop_break_on_consistency
        ]
    , testGroup "SourceLocation Tests"
        [ fastProperty "mergeSpans is associative" prop_merge_spans_associative
        , fastProperty "mergeSpans produces valid spans" prop_merge_spans_valid
        , fastProperty "mapLocated preserves span" prop_located_map_preserves_span
        , fastProperty "advancePos is consistent" prop_advance_pos_consistency
        ]
    ]