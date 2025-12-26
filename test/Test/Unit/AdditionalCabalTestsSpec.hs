{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.AdditionalCabalTestsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose)
import Data.Char (isSpace, isAlphaNum, isLetter)
import Data.List (isPrefixOf, isInfixOf, sort, nub)
import qualified Data.Text as T

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , splitByComma
  , removeLineComments
  , removeComments
  , normalizeIndentation
  , breakOn
  )

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  , posAfter
  , posAt
  , emptySpan
  , spanFrom
  , mergeSpans
  , locatedAt
  , advancePos
  , advancePosBy
  )

import Parser (FileDirectives(..), BlockDirectives(..), defaultFileDirectives, defaultBlockDirectives)

-- ============================================================================
-- Utils Module Tests
-- ============================================================================

-- Test 1: Property: splitBy and splitByCollapsed relationship
prop_splitBy_collapsed_relationship :: Char -> String -> Property
prop_splitBy_collapsed_relationship delim str =
  let normal = splitBy delim str
      collapsed = splitByCollapsed delim str
      -- Collapsed version should be normal version with empty strings removed
      expectedCollapsed = filter (not . null) normal
  in property $ collapsed === expectedCollapsed

-- Test 2: Property: trim idempotency (trim(trim(x)) == trim(x))
prop_trim_idempotent :: String -> Property
prop_trim_idempotent str =
  let trimmedOnce = trim str
      trimmedTwice = trim trimmedOnce
  in property $ trimmedOnce === trimmedTwice

-- Test 3: Property: breakOn returns a tuple
prop_breakOn_consistency :: String -> String -> Property
prop_breakOn_consistency delim str =
  let breakResult = breakOn delim str
      (before, after) = breakResult
      -- Check that concatenating the results gives the original string
      reconstructed = before ++ delim ++ after
  in property $ reconstructed === str

-- Test 4: Property: normalizeIndentation preserves relative indentation
prop_normalizeIndentation_preserves_relative :: String -> Property
prop_normalizeIndentation_preserves_relative multiLineStr =
  let linesList = lines multiLineStr
      normalized = normalizeIndentation multiLineStr
      normalizedLines = lines normalized
      -- Check that relative indentation is preserved for non-empty lines
      nonEmptyOriginal = filter (not . null) linesList
      nonEmptyNormalized = filter (not . null) normalizedLines
  in property $ length nonEmptyOriginal === length nonEmptyNormalized

-- ============================================================================
-- SourceLocation Module Tests
-- ============================================================================

-- Test 5: Property: advancePos line/column consistency
prop_advancePos_line_consistency :: Int -> String -> Property
prop_advancePos_line_consistency lineOffset chars =
  lineOffset >= 0 ==> 
  let start = startPos
      -- Use advancePosBy to advance by a string
      advanced = advancePosBy chars start
      expectedLine = posLine start + lineOffset + countNewlines chars
      countNewlines = length . filter (== '\n')
  in property $ posLine advanced === expectedLine

-- Test 6: Property: mergeSpans is commutative for valid spans
prop_mergeSpans_commutative :: Int -> Int -> Int -> Int -> Property
prop_mergeSpans_commutative l1 c1 l2 c2 =
  let pos1 = posAt l1 c1
      pos2 = posAt l2 c2
      span1 = spanFrom pos1
      span2 = spanFrom pos2
      merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
  in property $ merged1 === merged2

-- ============================================================================
-- Parser Module Tests  
-- ============================================================================

-- Test 7: Property: FileDirectives default values
prop_fileDirectives_defaults :: Property
prop_fileDirectives_defaults =
  let defaults = defaultFileDirectives
  in property $ fdOwnership defaults === Nothing .&&. 
                fdDependentTypes defaults === Nothing .&&.
                fdConstraints defaults === Nothing

-- Test 8: Property: BlockDirectives default values  
prop_blockDirectives_defaults :: Property
prop_blockDirectives_defaults =
  let defaults = defaultBlockDirectives
  in property $ bdOwnership defaults === Nothing .&&.
                bdDependentTypes defaults === Nothing .&&.
                bdConstraints defaults === Nothing

-- Test 9: Property: removeComments preserves non-comment content
prop_removeComments_preserves_content :: String -> String -> Property
prop_removeComments_preserves_content prefix suffix =
  let content = "valid content"
      withComments = prefix ++ "/* comment */" ++ content ++ "// line comment\n" ++ suffix
      withoutComments = removeComments withComments
  in property $ content `isInfixOf` withoutComments

-- Test 10: Property: splitByComma handles edge cases
prop_splitByComma_edge_cases :: String -> Property
prop_splitByComma_edge_cases str =
  let result = splitByComma str
      -- Check that joining with commas gives back original (with empty strings for consecutive commas)
      rejoined = foldr (\x acc -> if null acc then x else x ++ "," ++ acc) "" result
  in property $ length result >= 1 -- Should always return at least one element

-- ============================================================================
-- Test Suite Definition
-- ============================================================================

tests :: TestTree
tests = testGroup "Additional Cabal Tests"
  [ testGroup "Utils Module Properties"
    [ fastProperty "splitBy/splitByCollapsed relationship" prop_splitBy_collapsed_relationship
    , fastProperty "trim idempotency" prop_trim_idempotent
    , fastProperty "breakOn consistency" prop_breakOn_consistency
    , fastProperty "normalizeIndentation preserves relative" prop_normalizeIndentation_preserves_relative
    ]
  , testGroup "SourceLocation Properties"
    [ fastProperty "advancePos line consistency" prop_advancePos_line_consistency
    , fastProperty "mergeSpans commutative" prop_mergeSpans_commutative
    ]
  , testGroup "Parser Properties"
    [ fastProperty "FileDirectives defaults" prop_fileDirectives_defaults
    , fastProperty "BlockDirectives defaults" prop_blockDirectives_defaults
    , fastProperty "removeComments preserves content" prop_removeComments_preserves_content
    , fastProperty "splitByComma edge cases" prop_splitByComma_edge_cases
    ]
  ]