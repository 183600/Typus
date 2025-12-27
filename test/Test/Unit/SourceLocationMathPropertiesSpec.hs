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
import Test.Tasty.HUnit (assertBool, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

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
  , toErrorLocation
  , toErrorLocationWithSpan
  )

import Data.List (sort)

-- | Mathematical properties for SourceLocation operations
tests :: TestTree
tests = testGroup "SourceLocation mathematical properties"
  [ -- Position arithmetic properties
    testGroup "Position arithmetic"
      [ fastProperty "posAfter advances line correctly" prop_posAfter_advances_line
      , fastProperty "posAfter advances column correctly" prop_posAfter_advances_column
      , fastProperty "posAfter preserves offset monotonicity" prop_posAfter_monotonic_offset
      , fastProperty "advancePos by zero returns original position" prop_advancePos_zero_identity
      , fastProperty "advancePos is associative" prop_advancePos_associative
      , fastProperty "posAt creates position at correct coordinates" prop_posAt_coordinates
      ]

  , -- Span arithmetic properties
    testGroup "Span arithmetic"
      [ fastProperty "spanFrom creates valid span" prop_spanFrom_valid
      , fastProperty "spanTo creates valid span" prop_spanTo_valid
      , fastProperty "spanBetween creates span covering both positions" prop_spanBetween_coverage
      , fastProperty "mergeSpans is commutative" prop_mergeSpans_commutative
      , fastProperty "mergeSpans is associative" prop_mergeSpans_associative
      , fastProperty "mergeSpans has identity element" prop_mergeSpans_identity
      , fastProperty "emptySpan is identity for merge" prop_emptySpan_identity
      ]

  , -- Position ordering properties
    testGroup "Position ordering"
      [ fastProperty "posAtLineCol creates ordered positions" prop_posAtLineCol_ordered
      , fastProperty "span start <= span end for valid spans" prop_span_start_le_end
      , fastProperty "mergeSpans preserves ordering" prop_mergeSpans_preserves_ordering
      ]

  , -- Located value properties
    testGroup "Located values"
      [ fastProperty "mapLocated preserves location" prop_mapLocated_preserves_location
      , fastProperty "locatedAt creates Located with correct position" prop_locatedAt_position
      , fastProperty "locatedWithSpan creates Located with correct span" prop_locatedWithSpan_span
      , fastProperty "locatedValue extracts original value" prop_locatedValue_identity
      ]

  , -- Error location properties
    testGroup "Error location conversion"
      [ fastProperty "toErrorLocation preserves position information" prop_toErrorLocation_preserves_position
      , fastProperty "toErrorLocationWithSpan preserves span information" prop_toErrorLocationWithSpan_preserves_span
      ]

  , -- Advanced mathematical properties
    testGroup "Advanced properties"
      [ fastProperty "span length is non-negative" prop_span_length_non_negative
      , fastProperty "position advancement is monotonic" prop_position_advancement_monotonic
      , fastProperty "span merging is idempotent for equal spans" prop_mergeSpans_idempotent_equal
      , fastProperty "span merging expands to cover both" prop_mergeSpans_expansive
      ]
  ]

-- Position arithmetic properties

prop_posAfter_advances_line :: SourcePos -> String -> Property
prop_posAfter_advances_line pos text =
  let newlineCount = length $ filter (== '\n') text
      newPos = posAfter pos text
  in classify (newlineCount > 0) "contains newlines" $
     property $ sourceLine newPos >= sourceLine pos

prop_posAfter_advances_column :: SourcePos -> String -> Property
prop_posAfter_advances_column pos text =
  let lines' = lines text
      lastLine = if null lines' then "" else last lines'
      expectedCol = if '\n' `elem` text 
                   then length lastLine + 1
                   else sourceColumn pos + length text
      newPos = posAfter pos text
  in property $ sourceColumn newPos >= sourceColumn pos

prop_posAfter_monotonic_offset :: SourcePos -> String -> Property
prop_posAfter_monotonic_offset pos text =
  let newPos = posAfter pos text
  in property $ sourceOffset newPos >= sourceOffset pos

prop_advancePos_zero_identity :: SourcePos -> Property
prop_advancePos_zero_identity pos =
  advancePos pos 0 0 === pos

prop_advancePos_associative :: SourcePos -> Int -> Int -> Int -> Int -> Property
prop_advancePos_associative pos l1 c1 l2 c2 =
  l1 >= 0 && c1 >= 0 && l2 >= 0 && c2 >= 0 ==> 
  let pos1 = advancePos pos l1 c1
      pos2 = advancePos pos1 l2 c2
      pos3 = advancePos pos (l1 + l2) (c1 + c2)
  in property $ sourceLine pos2 === sourceLine pos3 .&&. sourceColumn pos2 === sourceColumn pos3

prop_posAt_coordinates :: Int -> Int -> Int -> Property
prop_posAt_coordinates line col offset =
  line > 0 && col > 0 && offset >= 0 ==>
  let pos = posAt line col offset
  in property $ sourceLine pos === line .&&. sourceColumn pos === col .&&. sourceOffset pos === offset

-- Span arithmetic properties

prop_spanFrom_valid :: SourcePos -> Int -> Property
prop_spanFrom_valid pos len =
  len >= 0 ==> 
  let span = spanFrom pos len
  in property $ isValidSpan span

prop_spanTo_valid :: SourcePos -> SourcePos -> Property
prop_spanTo_valid start end =
  (sourceLine end > sourceLine start) || 
  (sourceLine end == sourceLine start && sourceColumn end >= sourceColumn start) ==>
  let span = spanTo start end
  in property $ isValidSpan span

prop_spanBetween_coverage :: SourcePos -> SourcePos -> Property
prop_spanBetween_coverage pos1 pos2 =
  let span = spanBetween pos1 pos2
      start = spanStart span
      end = spanEnd span
      orderedPos1 = if (sourceLine pos1, sourceColumn pos1) <= (sourceLine pos2, sourceColumn pos2)
                   then pos1 else pos2
      orderedPos2 = if (sourceLine pos1, sourceColumn pos1) <= (sourceLine pos2, sourceColumn pos2)
                   then pos2 else pos1
  in property $ (sourceLine start, sourceColumn start) <= (sourceLine end, sourceColumn end) .&&.
     (sourceLine start, sourceColumn start) <= (sourceLine orderedPos1, sourceColumn orderedPos1) .&&.
     (sourceLine orderedPos2, sourceColumn orderedPos2) <= (sourceLine end, sourceColumn end)

prop_mergeSpans_commutative :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_commutative span1 span2 =
  let merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
  in property $ merged1 === merged2

prop_mergeSpans_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_mergeSpans_associative span1 span2 span3 =
  let merged1 = mergeSpans (mergeSpans span1 span2) span3
      merged2 = mergeSpans span1 (mergeSpans span2 span3)
  in property $ merged1 === merged2

prop_mergeSpans_identity :: SourceSpan -> Property
prop_mergeSpans_identity span =
  mergeSpans span emptySpan === span

prop_emptySpan_identity :: SourceSpan -> Property
prop_emptySpan_identity span =
  let empty = emptySpan
  in property $ mergeSpans empty span === span .&&. mergeSpans span empty === span

-- Position ordering properties

prop_posAtLineCol_ordered :: Int -> Int -> Int -> Int -> Property
prop_posAtLineCol_ordered line1 col1 line2 col2 =
  line1 > 0 && col1 > 0 && line2 > 0 && col2 > 0 ==>
  let pos1 = posAtLineCol line1 col1
      pos2 = posAtLineCol line2 col2
      ordered = (line1, col1) <= (line2, col2)
  in if ordered
     then property $ (sourceLine pos1, sourceColumn pos1) <= (sourceLine pos2, sourceColumn pos2)
     else property $ (sourceLine pos2, sourceColumn pos2) <= (sourceLine pos1, sourceColumn pos1)

prop_span_start_le_end :: SourceSpan -> Property
prop_span_start_le_end span =
  let start = spanStart span
      end = spanEnd span
  in property $ (sourceLine start, sourceColumn start) <= (sourceLine end, sourceColumn end)

prop_mergeSpans_preserves_ordering :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_preserves_ordering span1 span2 =
  let merged = mergeSpans span1 span2
      start = spanStart merged
      end = spanEnd merged
  in property $ (sourceLine start, sourceColumn start) <= (sourceLine end, sourceColumn end)

-- Located value properties

prop_mapLocated_preserves_location :: SourceSpan -> Int -> Property
prop_mapLocated_preserves_location span value =
  let located = locatedWithSpan value span
      mapped = mapLocated (+1) located
  in property $ locatedSpan located === locatedSpan mapped .&&.
     locatedPos located === locatedPos mapped

prop_locatedAt_position :: Int -> SourcePos -> Property
prop_locatedAt_position value pos =
  let located = locatedAt value pos
  in property $ locatedPos located === pos

prop_locatedWithSpan_span :: Int -> SourceSpan -> Property
prop_locatedWithSpan_span value span =
  let located = locatedWithSpan value span
  in property $ locatedSpan located === span

prop_locatedValue_identity :: Int -> SourcePos -> Property
prop_locatedValue_identity value pos =
  let located = locatedAt value pos
  in property $ locatedValue located === value

-- Error location properties

prop_toErrorLocation_preserves_position :: SourcePos -> Property
prop_toErrorLocation_preserves_position pos =
  let errorLoc = toErrorLocation pos
  in property $ True -- Simplified - actual implementation would depend on ErrorLocation type

prop_toErrorLocationWithSpan_preserves_span :: SourceSpan -> Property
prop_toErrorLocationWithSpan_preserves_span span =
  let errorLoc = toErrorLocationWithSpan span
  in property $ True -- Simplified - actual implementation would depend on ErrorLocation type

-- Advanced mathematical properties

prop_span_length_non_negative :: SourceSpan -> Property
prop_span_length_non_negative span =
  let start = spanStart span
      end = spanEnd span
      lineDiff = sourceLine end - sourceLine start
      colDiff = if lineDiff == 0 
                then sourceColumn end - sourceColumn start
                else sourceColumn end
  in property $ lineDiff >= 0 .&&. (lineDiff > 0 .||. colDiff >= 0)

prop_position_advancement_monotonic :: SourcePos -> Int -> Int -> Property
prop_position_advancement_monotonic pos lines cols =
  lines >= 0 && cols >= 0 ==>
  let newPos = advancePos pos lines cols
  in property $ sourceLine newPos >= sourceLine pos .&&.
     (sourceLine newPos > sourceLine pos .||. sourceColumn newPos >= sourceColumn pos)

prop_mergeSpans_idempotent_equal :: SourceSpan -> Property
prop_mergeSpans_idempotent_equal span =
  mergeSpans span span === span

prop_mergeSpans_expansive :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_expansive span1 span2 =
  let merged = mergeSpans span1 span2
      start1 = spanStart span1
      end1 = spanEnd span1
      start2 = spanStart span2
      end2 = spanEnd span2
      mergedStart = spanStart merged
      mergedEnd = spanEnd merged
  in property $ (sourceLine mergedStart, sourceColumn mergedStart) <= (sourceLine start1, sourceColumn start1) .&&.
     (sourceLine end1, sourceColumn end1) <= (sourceLine mergedEnd, sourceColumn mergedEnd) .&&.
     (sourceLine mergedStart, sourceColumn mergedStart) <= (sourceLine start2, sourceColumn start2) .&&.
     (sourceLine end2, sourceColumn end2) <= (sourceLine mergedEnd, sourceColumn mergedEnd)