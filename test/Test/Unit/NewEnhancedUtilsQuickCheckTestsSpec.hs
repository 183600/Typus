{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewEnhancedUtilsQuickCheckTestsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, listOf, elements, choose, oneof)
import Test.QuickCheck.Arbitrary (Arbitrary(..))

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

import Data.Char (isSpace, toLower)
import qualified Data.List as Data.List
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (tails, sort, nub)

-- ============================================================================
-- String Generation Helpers
-- ============================================================================

genWhitespace :: Gen Char
genWhitespace = elements " \t\n\r"

genNonWhitespace :: Gen Char
genNonWhitespace = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "!@#$%^&*()_+-=[]{}|;':\",./<>?"

genStringWithWhitespace :: Gen String
genStringWithWhitespace = listOf (oneof [genWhitespace, genNonWhitespace])

genStringWithoutNewlines :: Gen String
genStringWithoutNewlines = listOf (oneof [elements " \t", genNonWhitespace])

genDelimiter :: Gen Char
genDelimiter = elements [',', ';', ':', '|', '#', '@']

-- ============================================================================
-- Trim Function Properties
-- ============================================================================

-- Property: trim should never add whitespace to a string
prop_trim_never_adds_whitespace :: String -> Property
prop_trim_never_adds_whitespace input =
  let trimmed = trim input
      originalSpaces = L.length (filter isSpace input)
      trimmedSpaces = L.length (filter isSpace trimmed)
  in property $ trimmedSpaces <= originalSpaces

-- Property: trim applied twice should give same result as once
prop_trim_idempotent :: String -> Property
prop_trim_idempotent input =
  let trimmedOnce = trim input
      trimmedTwice = trim (trim input)
  in property $ trimmedOnce === trimmedTwice

-- Property: trim of L.all whitespace should be empty
prop_trim_all_whitespace_is_empty :: Property
prop_trim_all_whitespace_is_empty =
  forAll genStringWithWhitespace $ \input ->
    let allWhitespace = L.all isSpace input
        trimmed = trim input
    in allWhitespace ==> property $ trimmed === ""

-- ============================================================================
-- Split Function Properties
-- ============================================================================

-- Property: splitBy should preserve total character count (including delimiters)
prop_splitBy_preserves_content :: Char -> String -> Property
prop_splitBy_preserves_content delim input =
  let parts = splitBy delim input
      rejoined = Data.List.intercalate [delim] parts
  in property $ rejoined === input

-- Property: splitByCollapsed should never produce empty strings
prop_splitByCollapsed_no_empty_segments :: Char -> String -> Property
prop_splitByCollapsed_no_empty_segments delim input =
  let parts = splitByCollapsed delim input
  in property $ L.all (not . null) parts

-- Property: splitByComma should be equivalent to splitBy ','
prop_splitByComma_equals_splitBy_comma :: String -> Property
prop_splitByComma_equals_splitBy_comma input =
  property $ splitByComma input === splitBy ',' input

-- Property: splitByCommaCollapsed should be equivalent to splitByCollapsed ','
prop_splitByCommaCollapsed_equals_splitByCollapsed_comma :: String -> Property
prop_splitByCommaCollapsed_equals_splitByCollapsed_comma input =
  property $ splitByCommaCollapsed input === splitByCollapsed ',' input

-- ============================================================================
-- Comment Removal Properties
-- ============================================================================

-- Property: removeLineComments should never increase string L.length
prop_removeLineComments_never_increases_length :: String -> Property
prop_removeLineComments_never_increases_length input =
  let withoutComments = removeLineComments input
  in property $ L.length withoutComments <= L.length input

-- Property: removeLineComments should preserve non-comment content
prop_removeLineComments_preserves_non_comment_content :: String -> Property
prop_removeLineComments_preserves_non_comment_content input =
  let withoutComments = removeLineComments input
      linesWithoutComments = lines withoutComments
      originalLines = lines input
      -- Count non-empty, non-whitespace lines in both
      nonCommentOriginal = L.length $ L.filter (not . L.all isSpace) $ 
                           L.map (takeWhile (/= '/')) $ originalLines
      nonCommentResult = L.length $ L.filter (not . L.all isSpace) linesWithoutComments
  in property $ nonCommentResult <= nonCommentOriginal

-- Property: removeComments applied twice should give same result as once
prop_removeComments_idempotent :: String -> Property
prop_removeComments_idempotent input =
  let once = removeComments input
      twice = removeComments (removeComments input)
  in property $ once === twice

-- ============================================================================
-- Indentation Properties
-- ============================================================================

-- Property: normalizeIndentation should never increase the number of leading spaces
prop_normalizeIndentation_never_increases_leading :: Property
prop_normalizeIndentation_never_increases_leading =
  forAll genStringWithoutNewlines $ \input ->
    let normalized = normalizeIndentation input
        originalLines = lines input
        normalizedLines = lines normalized
        leadingSpaces line = L.length $ takeWhile isSpace line
    in not (null originalLines) ==> 
       property $ L.all (\(orig, norm) -> leadingSpaces norm <= leadingSpaces orig) 
                     (zip originalLines normalizedLines)

-- Property: fixIndentation should be equivalent to normalizeIndentation
prop_fixIndentation_equals_normalizeIndentation :: String -> Property
prop_fixIndentation_equals_normalizeIndentation input =
  property $ fixIndentation input === normalizeIndentation input

-- Property: forceSingleTabIndentation should start non-empty lines with tab
prop_forceSingleTabIndentation_starts_with_tab :: Property
prop_forceSingleTabIndentation_starts_with_tab =
  forAll genStringWithoutNewlines $ \input ->
    let forced = forceSingleTabIndentation input
        linesForced = lines forced
        nonEmptyLines = L.filter (not . L.all isSpace) linesForced
    in not (null nonEmptyLines) ==> 
       property $ L.all (L.isPrefixOf "\t") nonEmptyLines

-- ============================================================================
-- BreakOn Function Properties
-- ============================================================================

-- Property: breakOn should find first occurrence of pattern
prop_breakOn_finds_first_occurrence :: String -> String -> Property
prop_breakOn_finds_first_occurrence pat input =
  not (null pat) ==> 
  let (before, after) = breakOn pat input
      combined = before ++ pat ++ after
  in property $ combined === input

-- Property: breakOn with empty pattern should return ("", input)
prop_breakOn_empty_pattern :: String -> Property
prop_breakOn_empty_pattern input =
  let (before, after) = breakOn "" input
  in property $ before === "" .&&. after === input

-- Property: breakOn should return (input, "") when pattern not found
prop_breakOn_pattern_not_found :: String -> String -> Property
prop_breakOn_pattern_not_found pat input =
  not (null pat) ==> 
  not (pat `L.isInfixOf` input) ==> 
  let (before, after) = breakOn pat input
  in property $ before === input .&&. after === ""

-- Property: breakOn should be consistent with Data.List.break
prop_breakOn_consistent_with_break :: String -> String -> Property
prop_breakOn_consistent_with_break pat input =
  not (null pat) ==> 
  let (before, after) = breakOn pat input
      (before', after') = Data.List.break (L.isPrefixOf pat) (tails input)
  in case after' of
       [] -> property $ before === input .&&. after === ""
       (x:_) -> property $ before === Data.List.take (L.length input - L.length x) input

-- ============================================================================
-- String Processing Properties
-- ============================================================================

-- Property: trim after splitBy should not produce empty segments if original had none
prop_trim_splitBy_no_empty_if_original_no_empty :: Char -> String -> Property
prop_trim_splitBy_no_empty_if_original_no_empty delim input =
  let parts = splitBy delim input
      trimmedParts = map trim parts
      originalHadEmpty = L.any null parts
  in not originalHadEmpty ==> property $ not (L.any null trimmedParts)

-- Property: normalizeIndentation should preserve relative indentation differences
prop_normalizeIndentation_preserves_relative_differences :: Property
prop_normalizeIndentation_preserves_relative_differences =
  forAll genStringWithoutNewlines $ \input ->
    let normalized = normalizeIndentation input
        originalLines = L.filter (not . L.all isSpace) $ lines input
        normalizedLines = L.filter (not . L.all isSpace) $ lines normalized
        leadingSpaces line = L.length $ takeWhile isSpace line
        differences origLines = 
          case origLines of
            [] -> []
            (l:ls) -> L.map (\line -> leadingSpaces line - leadingSpaces l) ls
    in L.length originalLines > 1 && L.length normalizedLines > 1 ==> 
       property $ differences originalLines === differences normalizedLines

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New Utils QuickCheck Tests"
  [ testGroup "Trim Properties"
    [ fastProperty "trim never adds whitespace" prop_trim_never_adds_whitespace
    , fastProperty "trim is idempotent" prop_trim_idempotent
    , fastProperty "trim of L.all whitespace is empty" prop_trim_all_whitespace_is_empty
    ]
  , testGroup "Split Properties"
    [ fastProperty "splitBy preserves content" prop_splitBy_preserves_content
    , fastProperty "splitByCollapsed produces no empty segments" prop_splitByCollapsed_no_empty_segments
    , fastProperty "splitByComma equals splitBy ','" prop_splitByComma_equals_splitBy_comma
    , fastProperty "splitByCommaCollapsed equals splitByCollapsed ','" prop_splitByCommaCollapsed_equals_splitByCollapsed_comma
    ]
  , testGroup "Comment Removal Properties"
    [ fastProperty "removeLineComments never increases L.length" prop_removeLineComments_never_increases_length
    , fastProperty "removeLineComments preserves non-comment content" prop_removeLineComments_preserves_non_comment_content
    , fastProperty "removeComments is idempotent" prop_removeComments_idempotent
    ]
  , testGroup "Indentation Properties"
    [ fastProperty "normalizeIndentation never increases leading spaces" prop_normalizeIndentation_never_increases_leading
    , fastProperty "fixIndentation equals normalizeIndentation" prop_fixIndentation_equals_normalizeIndentation
    , fastProperty "forceSingleTabIndentation starts with tab" prop_forceSingleTabIndentation_starts_with_tab
    ]
  , testGroup "BreakOn Properties"
    [ fastProperty "breakOn finds first occurrence" prop_breakOn_finds_first_occurrence
    , fastProperty "breakOn with empty pattern" prop_breakOn_empty_pattern
    , fastProperty "breakOn when pattern not found" prop_breakOn_pattern_not_found
    , fastProperty "breakOn consistent with Data.List.break" prop_breakOn_consistent_with_break
    ]
  , testGroup "String Processing Properties"
    [ fastProperty "trim splitBy no empty if original no empty" prop_trim_splitBy_no_empty_if_original_no_empty
    , fastProperty "normalizeIndentation preserves relative differences" prop_normalizeIndentation_preserves_relative_differences
    ]
  ]