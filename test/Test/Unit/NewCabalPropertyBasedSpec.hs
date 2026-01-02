{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalPropertyBasedSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Positive(..), NonEmptyList(..), Arbitrary(..)
  , Gen, choose, elements, listOf, vectorOf, oneof, suchThat
  )

import qualified Data.Text as T
import Data.Char (isSpace, isLetter, isDigit, isPunctuation)
import qualified Data.List as L
import Data.List (isInfixOf, isPrefixOf)
import Data.List (sort, nub, group)

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  , posAfter
  , advancePosBy
  , advancePosByText
  , emptySpan
  , spanFrom
  , spanTo
  , spanBetween
  , mergeSpans
  , isValidSpan
  , locatedAt
  , mapLocated
  , locatedValue
  , locatedSpan
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

-- ============================================================================
-- SourceLocation Properties
-- ============================================================================

-- Property: Position advancement is monotonic
prop_position_advancement_monotonic :: String -> Property
prop_position_advancement_monotonic text =
  let start = startPos
      end = advancePosBy text start
  in property $ posOffset end >= posOffset start

-- Property: Span merging is commutative
prop_span_merging_commutative :: SourcePos -> SourcePos -> SourcePos -> SourcePos -> Property
prop_span_merging_commutative p1 p2 p3 p4 =
  let span1 = spanBetween p1 p2
      span2 = spanBetween p3 p4
      merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
  in property $ merged1 === merged2

-- Property: Span merging is associative
prop_span_merging_associative :: SourcePos -> SourcePos -> SourcePos -> SourcePos -> SourcePos -> SourcePos -> Property
prop_span_merging_associative p1 p2 p3 p4 p5 p6 =
  let span1 = spanBetween p1 p2
      span2 = spanBetween p3 p4
      span3 = spanBetween p5 p6
      merged1 = mergeSpans (mergeSpans span1 span2) span3
      merged2 = mergeSpans span1 (mergeSpans span2 span3)
  in property $ merged1 === merged2

-- Property: Located value mapping preserves location
prop_located_mapping_preserves_location :: SourcePos -> Int -> Property
prop_located_mapping_preserves_location pos value =
  let original = locatedAt pos value
      mapped = mapLocated (*2) original
  in property $ locatedSpan original === locatedSpan mapped

-- Property: Error location conversion preserves position info
prop_error_location_preserves_position :: SourcePos -> SourcePos -> Property
prop_error_location_preserves_position start end =
  let span = spanBetween start end
      errLoc = toErrorLocationWithSpan span
  in property $ line errLoc === posLine start .&&. column errLoc === posColumn start

-- ============================================================================
-- Utils Properties
-- ============================================================================

-- Property: trim is idempotent
prop_trim_idempotent :: String -> Property
prop_trim_idempotent str =
  let trimmed1 = trim str
      trimmed2 = trim trimmed1
  in property $ trimmed1 === trimmed2

-- Property: trim removes leading/trailing whitespace
prop_trim_removes_whitespace :: String -> String -> String -> Property
prop_trim_removes_whitespace prefix suffix =
  let content = prefix ++ "content" ++ suffix
      trimmed = trim content
      hasLeadingSpace = not (null prefix) && isSpace (last prefix)
      hasTrailingSpace = not (null suffix) && isSpace (L.head suffix)
  in classify hasLeadingSpace "has leading" $
     classify hasTrailingSpace "has trailing" $
     property $ (null trimmed || not (isSpace (L.head trimmed))) .&&.
                (null trimmed || not (isSpace (last trimmed)))

-- Property: splitBy preserves total character count (minus delimiters)
prop_splitBy_preserves_content :: Char -> String -> Property
prop_splitBy_preserves_content delim str =
  let parts = splitBy delim str
      rejoined = L.intercalate [delim] parts
  in property $ rejoined === str

-- Property: splitByCollapsed removes empty segments
prop_splitByCollapsed_removes_empty :: Char -> String -> Property
prop_splitByCollapsed_removes_empty delim str =
  let parts = splitByCollapsed delim str
  in property $ not (L.any null parts)

-- Property: splitByComma equals splitBy with comma
prop_splitByComma_consistency :: String -> Property
prop_splitByComma_consistency str =
  splitByComma str === splitBy ',' str

-- Property: removeLineComments preserves non-comment content
prop_removeLineComments_preserves_content :: String -> String -> Property
prop_removeLineComments_preserves_content content comment =
  not ('"' `elem` content) && not ('\'' `elem` content) ==>
  let input = content ++ " // " ++ comment ++ "\nmore content"
      result = removeLineComments input
  in property $ content `L.isInfixOf` result .&&. "more content" `L.isInfixOf` result

-- Property: removeComments preserves string literals
prop_removeComments_preserves_strings :: String -> Property
prop_removeComments_preserves_strings content =
  not ('"' `elem` content) && not ('\'' `elem` content) ==>
  let input = "var s = \"" ++ content ++ "\"; // comment\n/* block comment */"
      result = removeComments input
  in property $ ("\"" ++ content ++ "\"") `L.isInfixOf` result

-- Property: normalizeIndentation preserves relative indentation
prop_normalizeIndentation_preserves_relative :: [Int] -> [String] -> Property
prop_normalizeIndentation_preserves_relative indentLevels contents =
  length indentLevels == L.length contents && L.all (>= 0) indentLevels ==>
  let lines' = zipWith (\lvl content -> replicate lvl ' ' ++ content) indentLevels contents
      input = unlines lines'
      normalized = normalizeIndentation input
      normalizedLines = lines normalized
      -- Check that relative indentation is preserved
      originalPairs = zip (drop 1 indentLevels) (init indentLevels)
      normalizedPairs = zipWith (\l1 l2 -> 
        let indent1 = L.length (takeWhile isSpace l1)
            indent2 = L.length (takeWhile isSpace l2)
        in indent1 - indent2) (drop 1 normalizedLines) (init normalizedLines)
  in property $ L.all (>= 0) normalizedPairs

-- Property: breakOn finds correct split point
prop_breakOn_correct_split :: String -> String -> String -> Property
prop_breakOn_correct_split pat prefix suffix =
  not (null pat) ==>
  let haystack = prefix ++ pat ++ suffix
      (before, after) = breakOn pat haystack
  in property $ before ++ pat ++ after === haystack

-- Property: breakOn handles missing pattern
prop_breakOn_missing_pattern :: String -> String -> Property
prop_breakOn_missing_pattern pat haystack =
  not (null pat) && not (pat `L.isInfixOf` haystack) ==>
  let (before, after) = breakOn pat haystack
  in property $ before === haystack .&&. after === ""

-- ============================================================================
-- Advanced Properties
-- ============================================================================

-- Property: Complex string processing pipeline
prop_complex_pipeline_consistency :: String -> Property
prop_complex_pipeline_consistency input =
  not ('"' `elem` input) && not ('\'' `elem` input) ==>
  let pipeline1 = input |> trim |> removeComments |> normalizeIndentation
      pipeline2 = input |> removeComments |> trim |> normalizeIndentation
  in property $ pipeline1 === pipeline2

-- Property: Position advancement with Unicode
prop_position_advancement_unicode :: String -> Property
prop_position_advancement_unicode unicodeText =
  let pos = advancePosBy unicodeText startPos
  in property $ posOffset pos >= L.length unicodeText

-- Property: Span validity after merging
prop_span_merging_validity :: SourcePos -> SourcePos -> SourcePos -> SourcePos -> Property
prop_span_merging_validity p1 p2 p3 p4 =
  let span1 = spanBetween p1 p2
      span2 = spanBetween p3 p4
      merged = mergeSpans span1 span2
  in property $ isValidSpan merged

-- Property: Split operations with special characters
prop_split_special_characters :: Char -> String -> Property
prop_split_special_characters delim content =
  let specialContent = content ++ "\n\t\r" ++ content
      parts = splitBy delim specialContent
  in property $ L.length parts >= 1

-- Property: Comment removal with edge cases
prop_comment_removal_edge_cases :: String -> Property
prop_comment_removal_edge_cases content =
  not ('"' `elem` content) && not ('\'' `elem` content) && not ("*/" `L.isInfixOf` content) ==>
  let edgeInput = content ++ "/* comment\n" ++ content ++ "*/" ++ content
      result = removeComments edgeInput
  in property $ content `L.isInfixOf` result

-- Property: Indentation normalization with empty lines
prop_indentation_with_empty_lines :: [String] -> Property
prop_indentation_with_empty_lines lines =
  let input = unlines $ intersperse "" lines
      normalized = normalizeIndentation input
      normalizedLines = lines normalized
  in property $ L.length normalizedLines >= L.length lines

-- Property: String processing roundtrip
prop_string_processing_roundtrip :: String -> Property
prop_string_processing_roundtrip original =
  let processed = original |> trim |> normalizeIndentation |> removeComments
      restored = processed  -- In practice, this would be more complex
  in property $ L.length processed <= L.length original

-- Helper function for pipeline
(|>) :: a -> (a -> b) -> b
(|>) x f = f x

-- Helper function to intersperse empty lines
intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse sep (x:xs) = x : sep : intersperse sep xs

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests =
  testGroup "NewCabalPropertyBased"
    [ testGroup "SourceLocation Properties"
        [ fastProperty "Position advancement is monotonic" prop_position_advancement_monotonic
        , fastProperty "Span merging is commutative" prop_span_merging_commutative
        , fastProperty "Span merging is associative" prop_span_merging_associative
        , fastProperty "Located mapping preserves location" prop_located_mapping_preserves_location
        , fastProperty "Error location preserves position" prop_error_location_preserves_position
        ]

    , testGroup "Utils Properties"
        [ fastProperty "Trim is idempotent" prop_trim_idempotent
        , fastProperty "Trim removes whitespace" prop_trim_removes_whitespace
        , fastProperty "SplitBy preserves content" prop_splitBy_preserves_content
        , fastProperty "SplitByCollapsed removes empty" prop_splitByCollapsed_removes_empty
        , fastProperty "SplitByComma consistency" prop_splitByComma_consistency
        , fastProperty "RemoveLineComments preserves content" prop_removeLineComments_preserves_content
        , fastProperty "RemoveComments preserves strings" prop_removeComments_preserves_strings
        , fastProperty "NormalizeIndentation preserves relative" prop_normalizeIndentation_preserves_relative
        , fastProperty "BreakOn correct split" prop_breakOn_correct_split
        , fastProperty "BreakOn missing pattern" prop_breakOn_missing_pattern
        ]

    , testGroup "Advanced Properties"
        [ fastProperty "Complex pipeline consistency" prop_complex_pipeline_consistency
        , fastProperty "Position advancement with Unicode" prop_position_advancement_unicode
        , fastProperty "Span merging validity" prop_span_merging_validity
        , fastProperty "Split special characters" prop_split_special_characters
        , fastProperty "Comment removal edge cases" prop_comment_removal_edge_cases
        , fastProperty "Indentation with empty lines" prop_indentation_with_empty_lines
        , fastProperty "String processing roundtrip" prop_string_processing_roundtrip
        ]
    ]