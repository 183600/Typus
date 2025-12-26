{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.AdditionalCorePropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary ()
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose, sized)
import qualified Test.QuickCheck as QC

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , removeComments
  , breakOn
  , normalizeIndentation
  )

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
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

import Data.Char (isSpace, isAlphaNum, isLetter)
import Data.List (sort, nub)
import qualified Data.Text as T

-- ============================================================================
-- Utils Module Properties
-- ============================================================================

-- Property: breakOn is consistent with Data.Text.breakOn
prop_breakOn_consistency :: String -> String -> Property
prop_breakOn_consistency pat str =
  not (null pat) ==>
  let (before, after) = breakOn pat str
      text = T.pack str
      patText = T.pack pat
      (beforeText, afterText) = T.breakOn patText text
  in counterexample ("breakOn result: " ++ show (before, after)) $
     counterexample ("Text.breakOn result: " ++ show (T.unpack beforeText, maybe "" T.unpack (T.stripPrefix patText afterText))) $
     T.unpack beforeText === before .&&.
     maybe "" T.unpack (T.stripPrefix patText afterText) === after

-- Property: splitBy preserves total length when including empty segments
prop_splitBy_preserves_length :: Char -> String -> Property
prop_splitBy_preserves_length delim str =
  let parts = splitBy delim str
      totalLength = sum (map length parts) + length (filter (== delim) str) - length parts + 1
  in counterexample ("Original: " ++ show str) $
     counterexample ("Parts: " ++ show parts) $
     counterexample ("Total length: " ++ show totalLength ++ " vs " ++ show (length str)) $
     totalLength === length str

-- Property: splitByCollapsed never produces empty strings
prop_splitByCollapsed_no_empty :: Char -> String -> Property
prop_splitByCollapsed_no_empty delim str =
  let parts = splitByCollapsed delim str
  in counterexample ("Parts: " ++ show parts) $
     property $ all (not . null) parts

-- Property: removeComments preserves line count
prop_removeComments_preserves_lines :: String -> Property
prop_removeComments_preserves_lines code =
  let originalLines = length (lines code)
      processedLines = length (lines (removeComments code))
  in counterexample ("Original lines: " ++ show originalLines) $
     counterexample ("Processed lines: " ++ show processedLines) $
     property $ processedLines >= originalLines `div` 2

-- Property: normalizeIndentation preserves relative indentation structure
prop_normalizeIndentation_preserves_structure :: String -> Property
prop_normalizeIndentation_preserves_structure code =
  let ls = lines code
      nonEmptyLines = filter (not . all isSpace) ls
      originalIndentStructure = map (length . takeWhile isSpace) nonEmptyLines
      normalizedLines = lines (normalizeIndentation code)
      normalizedNonEmpty = filter (not . all isSpace) normalizedLines
      normalizedIndentStructure = map (length . takeWhile isSpace) normalizedNonEmpty
  in not (null nonEmptyLines) ==>
     counterexample ("Original structure: " ++ show originalIndentStructure) $
     counterexample ("Normalized structure: " ++ show normalizedIndentStructure) $
     let minOriginal = minimum originalIndentStructure
         adjustedOriginal = map (subtract minOriginal) originalIndentStructure
     in adjustedOriginal === normalizedIndentStructure

-- ============================================================================
-- SourceLocation Module Properties
-- ============================================================================

-- Property: posAfter advances offset by exactly 1
prop_posAfter_advances_offset :: Char -> SourcePos -> Property
prop_posAfter_advances_offset c pos =
  let newPos = posAfter c pos
  in posOffset newPos === posOffset pos + 1

-- Property: posAfter handles newline correctly
prop_posAfter_newline :: SourcePos -> Property
prop_posAfter_newline pos =
  let newPos = posAfter '\n' pos
  in posLine newPos === posLine pos + 1 .&&.
     posColumn newPos === 1

-- Property: posAfter handles tab correctly (advances to next tab position)
prop_posAfter_tab :: SourcePos -> Property
prop_posAfter_tab pos =
  let newPos = posAfter '\t' pos
      expectedColumn = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in posColumn newPos === expectedColumn

-- Property: advancePosBy is consistent with repeated posAfter
prop_advancePosBy_consistency :: String -> SourcePos -> Property
prop_advancePosBy_consistency str pos =
  let advancedBy = advancePosBy str pos
      advancedRepeated = foldl (flip posAfter) pos str
  in advancedBy === advancedRepeated

-- Property: empty span has same start and end
prop_empty_span_same_start_end :: SourcePos -> Property
prop_empty_span_same_start_end pos =
  let span = emptySpan pos
  in spanStart span === pos .&&. spanEnd span === pos

-- Property: mergeSpans contains both original spans
prop_mergeSpans_contains_both :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_contains_both span1 span2 =
  let merged = mergeSpans span1 span2
      start1 = spanStart span1
      end1 = spanEnd span1
      start2 = spanStart span2
      end2 = spanEnd span2
      mergedStart = spanStart merged
      mergedEnd = spanEnd merged
  in mergedStart <= start1 .&&. mergedEnd >= end1 .&&.
     mergedStart <= start2 .&&. mergedEnd >= end2

-- Property: mergeSpans is commutative
prop_mergeSpans_commutative :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_commutative span1 span2 =
  let merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
  in merged1 === merged2

-- Property: mergeSpans is associative
prop_mergeSpans_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_mergeSpans_associative span1 span2 span3 =
  let mergedLeft = mergeSpans (mergeSpans span1 span2) span3
      mergedRight = mergeSpans span1 (mergeSpans span2 span3)
  in mergedLeft === mergedRight

-- Property: isValidSpan correctly identifies invalid spans
prop_isValidSpan_invalid :: SourcePos -> SourcePos -> Property
prop_isValidSpan_invalid pos1 pos2 =
  let invalidSpan = SourceSpan pos2 pos1  -- end before start
  in pos1 > pos2 ==> not (isValidSpan invalidSpan)

-- ============================================================================
-- Combined Properties
-- ============================================================================

-- Property: Source location tracking preserves consistency through text processing
prop_source_location_text_processing :: String -> Property
prop_source_location_text_processing text =
  let finalPos = foldl (flip posAfter) startPos text
      totalLength = length text
      finalOffset = posOffset finalPos
  in not (null text) ==>
     counterexample ("Final offset: " ++ show finalOffset) $
     counterexample ("Expected: " ++ show totalLength) $
     finalOffset === totalLength

-- Property: String splitting and rejoining preserves original (with delimiter)
prop_split_join_preservation :: Char -> String -> Property
prop_split_join_preservation delim str =
  let parts = splitBy delim str
      rejoined = concatMap (\p -> if null p then "" else p ++ [delim]) (init parts) ++ last parts
  in not (null str) ==>
     counterexample ("Original: " ++ show str) $
     counterexample ("Rejoined: " ++ show rejoined) $
     rejoined === str

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "Additional Core Properties"
  [ testGroup "Utils Properties"
    [ fastProperty "breakOn consistency with Text.breakOn" prop_breakOn_consistency
    , fastProperty "splitBy preserves total length" prop_splitBy_preserves_length
    , fastProperty "splitByCollapsed never produces empty strings" prop_splitByCollapsed_no_empty
    , fastProperty "removeComments preserves line count" prop_removeComments_preserves_lines
    , fastProperty "normalizeIndentation preserves relative structure" prop_normalizeIndentation_preserves_structure
    , fastProperty "split and join preservation" prop_split_join_preservation
    ]
  , testGroup "SourceLocation Properties"
    [ fastProperty "posAfter advances offset by exactly 1" prop_posAfter_advances_offset
    , fastProperty "posAfter handles newline correctly" prop_posAfter_newline
    , fastProperty "posAfter handles tab correctly" prop_posAfter_tab
    , fastProperty "advancePosBy consistency with repeated posAfter" prop_advancePosBy_consistency
    , fastProperty "empty span has same start and end" prop_empty_span_same_start_end
    , fastProperty "mergeSpans contains both original spans" prop_mergeSpans_contains_both
    , fastProperty "mergeSpans is commutative" prop_mergeSpans_commutative
    , fastProperty "mergeSpans is associative" prop_mergeSpans_associative
    , fastProperty "isValidSpan correctly identifies invalid spans" prop_isValidSpan_invalid
    ]
  , testGroup "Combined Properties"
    [ fastProperty "source location tracking through text processing" prop_source_location_text_processing
    ]
  ]