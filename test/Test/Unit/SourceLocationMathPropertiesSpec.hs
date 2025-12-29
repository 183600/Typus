{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.SourceLocationMathPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf1, elements)
import Test.QuickCheck.Arbitrary (Arbitrary(..))

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  , posAfter
  , advancePos
  , advancePosBy
  , emptySpan
  , spanFrom
  , spanTo
  , spanBetween
  , mergeSpans
  , isValidSpan
  , locatedAt
  , mapLocated
  , spanStart
  , spanEnd
  )

import qualified Data.Text as T

-- ============================================================================
-- Property Tests for SourceLocation Mathematical Properties
-- ============================================================================

-- Property: posAfter is consistent with advancePos for single characters
prop_posAfter_consistency_with_advancePos :: Char -> SourcePos -> Property
prop_posAfter_consistency_with_advancePos char pos =
  let advanced = advancePos char pos
      after = posAfter char pos
  in property $ advanced === after

-- Property: advancePosBy with zero count returns original position
prop_advancePosBy_zero_identity :: SourcePos -> Property
prop_advancePosBy_zero_identity pos =
  let unchanged = advancePosBy "" pos
  in property $ unchanged === pos

-- Property: advancePosBy is associative for character sequences
prop_advancePosBy_associative :: String -> String -> SourcePos -> Property
prop_advancePosBy_associative s1 s2 pos =
  let seq1 = advancePosBy (s1 ++ s2) pos
      mid = advancePosBy s1 pos
      seq2 = advancePosBy s2 mid
  in property $ seq1 === seq2

-- Property: spanFrom to spanTo roundtrip preserves position
prop_spanFrom_to_roundtrip :: SourcePos -> Property
prop_spanFrom_to_roundtrip pos =
  let span = spanFrom pos
      endPos = spanEnd span
      reconstructed = spanTo endPos
  in property $ spanStart reconstructed === spanStart span

-- Property: spanBetween creates valid span when start <= end
prop_spanBetween_valid_when_ordered :: SourcePos -> SourcePos -> Property
prop_spanBetween_valid_when_ordered start end =
  let ordered = if sourcePosLine start < sourcePosLine end || 
                   (sourcePosLine start == sourcePosLine end && sourcePosColumn start <= sourcePosColumn end)
                then Just (spanBetween start end)
                else Nothing
  in case ordered of
       Just span -> property $ isValidSpan span
       Nothing -> property $ True

-- Property: mergeSpans is commutative
prop_mergeSpans_commutative :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_commutative span1 span2 =
  let merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
  in property $ merged1 === merged2

-- Property: mergeSpans is associative
prop_mergeSpans_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_mergeSpans_associative span1 span2 span3 =
  let merged12_3 = mergeSpans (mergeSpans span1 span2) span3
      merged1_23 = mergeSpans span1 (mergeSpans span2 span3)
  in property $ merged12_3 === merged1_23

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
  in property $ 
    (sourcePosLine mergedStart <= sourcePosLine start1 && 
     sourcePosLine mergedEnd >= sourcePosLine end1) .&&.
    (sourcePosLine mergedStart <= sourcePosLine start2 && 
     sourcePosLine mergedEnd >= sourcePosLine end2)

-- Property: emptySpan has start and end at same position
prop_emptySpan_start_end_equal :: SourcePos -> Property
prop_emptySpan_start_end_equal pos =
  let span = spanFrom pos
  in property $ spanStart span === spanEnd span

-- Property: locatedAt preserves value and sets location
prop_locatedAt_preserves_value :: String -> SourcePos -> Property
prop_locatedAt_preserves_value value pos =
  let located = locatedAt value pos
  in property $ locatedValue located === value .&&. locatedPos located === pos

-- Property: mapLocated preserves location structure
prop_mapLocated_preserves_location :: String -> String -> SourcePos -> Property
prop_mapLocated_preserves_location original transformed pos =
  let located = locatedAt original pos
      mapped = mapLocated (const transformed) located
  in property $ locatedPos mapped === locatedPos located .&&. 
                locatedValue mapped === transformed

-- Property: posAfter newline always increments line and resets column
prop_posAfter_newline_properties :: SourcePos -> Property
prop_posAfter_newline_properties pos =
  let after = posAfter '\n' pos
  in property $ sourcePosLine after === sourcePosLine pos + 1 .&&.
                sourcePosColumn after === 1 .&&.
                sourcePosOffset after === sourcePosOffset pos + 1

-- Property: posAfter tab respects tab stop alignment
prop_posAfter_tab_alignment :: SourcePos -> Property
prop_posAfter_tab_alignment pos =
  let after = posAfter '\t' pos
      expectedCol = ((sourcePosColumn pos - 1) `div` 8 + 1) * 8 + 1
  in property $ sourcePosLine after === sourcePosLine pos .&&.
                sourcePosColumn after === expectedCol .&&.
                sourcePosOffset after === sourcePosOffset pos + 1

-- Property: advancePosBy on empty string is identity
prop_advancePosBy_empty_identity :: SourcePos -> Property
prop_advancePosBy_empty_identity pos =
  let unchanged = advancePosBy "" pos
  in property $ unchanged === pos

-- Property: advancePosBy on single character matches posAfter
prop_advancePosBy_single_char :: Char -> SourcePos -> Property
prop_advancePosBy_single_char char pos =
  let advanced = advancePosBy [char] pos
      after = posAfter char pos
  in property $ advanced === after

-- Property: advancePosBy offset count equals string length
prop_advancePosBy_offset_count :: String -> SourcePos -> Property
prop_advancePosBy_offset_count str pos =
  let advanced = advancePosBy str pos
      expectedOffset = sourcePosOffset pos + length str
  in property $ sourcePosOffset advanced === expectedOffset

-- Property: isValidSpan correctly identifies valid spans
prop_isValidSpan_logic :: SourceSpan -> Property
prop_isValidSpan_logic span =
  let start = spanStart span
      end = spanEnd span
      valid = sourcePosLine start < sourcePosLine end || 
              (sourcePosLine start == sourcePosLine end && sourcePosColumn start <= sourcePosColumn end)
  in property $ isValidSpan span === valid

-- Property: spanBetween returns Nothing when end precedes start
prop_spanBetween_nothing_when_unordered :: SourcePos -> SourcePos -> Property
prop_spanBetween_nothing_when_unordered start end =
  let unordered = sourcePosLine start > sourcePosLine end || 
                  (sourcePosLine start == sourcePosLine end && sourcePosColumn start > sourcePosColumn end)
  in unordered ==> 
     property $ spanBetween start end === Nothing

-- Property: spanBetween returns Just when start <= end
prop_spanBetween_just_when_ordered :: SourcePos -> SourcePos -> Property
prop_spanBetween_just_when_ordered start end =
  let ordered = sourcePosLine start < sourcePosLine end || 
                (sourcePosLine start == sourcePosLine end && sourcePosColumn start <= sourcePosColumn end)
  in ordered ==> 
     case spanBetween start end of
       Just span -> property $ spanStart span === start .&&. spanEnd span === end
       Nothing -> property $ False

-- Property: spanTo creates span from startPos to given position
prop_spanTo_from_start :: SourcePos -> Property
prop_spanTo_from_start pos =
  let span = spanTo pos
      start = startPos
  in property $ spanStart span === start .&&. spanEnd span === pos

-- Property: mergeSpans with empty span returns the other span
prop_mergeSpans_empty_identity :: SourceSpan -> Property
prop_mergeSpans_empty_identity span =
  let empty = emptySpan
      merged1 = mergeSpans empty span
      merged2 = mergeSpans span empty
  in property $ merged1 === span .&&. merged2 === span

-- Property: spanBetween with same positions returns empty span at that position
prop_spanBetween_same_positions :: SourcePos -> Property
prop_spanBetween_same_positions pos =
  case spanBetween pos pos of
    Just span -> property $ spanStart span === pos .&&. spanEnd span === pos
    Nothing -> property $ False

tests :: TestTree
tests = testGroup "SourceLocation Math Properties"
  [ fastProperty "posAfter consistency with advancePos" prop_posAfter_consistency_with_advancePos
  , fastProperty "advancePosBy zero identity" prop_advancePosBy_zero_identity
  , fastProperty "advancePosBy associative" prop_advancePosBy_associative
  , fastProperty "spanFrom to roundtrip" prop_spanFrom_to_roundtrip
  , fastProperty "spanBetween valid when ordered" prop_spanBetween_valid_when_ordered
  , fastProperty "mergeSpans commutative" prop_mergeSpans_commutative
  , fastProperty "mergeSpans associative" prop_mergeSpans_associative
  , fastProperty "mergeSpans contains both spans" prop_mergeSpans_contains_both
  , fastProperty "emptySpan start end equal" prop_emptySpan_start_end_equal
  , fastProperty "locatedAt preserves value" prop_locatedAt_preserves_value
  , fastProperty "mapLocated preserves location" prop_mapLocated_preserves_location
  , fastProperty "posAfter newline properties" prop_posAfter_newline_properties
  , fastProperty "posAfter tab alignment" prop_posAfter_tab_alignment
  , fastProperty "advancePosBy empty identity" prop_advancePosBy_empty_identity
  , fastProperty "advancePosBy single character" prop_advancePosBy_single_char
  , fastProperty "advancePosBy offset count" prop_advancePosBy_offset_count
  , fastProperty "isValidSpan logic" prop_isValidSpan_logic
  , fastProperty "spanBetween nothing when unordered" prop_spanBetween_nothing_when_unordered
  , fastProperty "spanBetween just when ordered" prop_spanBetween_just_when_ordered
  , fastProperty "spanTo from start" prop_spanTo_from_start
  , fastProperty "mergeSpans empty identity" prop_mergeSpans_empty_identity
  , fastProperty "spanBetween same positions" prop_spanBetween_same_positions
  ]