{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.SourceLocationNewQuickCheckTests (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, choose, listOf)

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , HasLocation(..)
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
  , advancePosByText
  , advancePosByLine
  , toErrorLocation
  , toErrorLocationWithSpan
  )

import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import Data.Char (isSpace)
import qualified Data.List as L

-- ============================================================================
-- Arbitrary Instances for SourceLocation Types
-- ============================================================================

instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 1000)
    column <- choose (1, 1000)
    offset <- choose (0, 10000)
    return $ SourcePos line column offset

instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    -- Ensure we get both valid L.and invalid spans
    let validEnd = if start <= end then end else start
    oneof [return $ spanBetween start validEnd, return $ spanBetween end start]

instance Arbitrary a => Arbitrary (Located a) where
  arbitrary = do
    value <- arbitrary
    pos <- arbitrary
    span <- arbitrary
    return $ Located value pos span

-- ============================================================================
-- SourcePos Properties
-- ============================================================================

-- Property: startPos is consistent
prop_startPos_consistent :: Property
prop_startPos_consistent =
  property $ posLine startPos === 1 .&&.
             posColumn startPos === 1 .&&.
             posOffset startPos === 0

-- Property: posAfter advances line for newline
prop_posAfter_newline :: SourcePos -> Property
prop_posAfter_newline pos =
  let newPos = posAfter '\n' pos
  in property $ posLine newPos === posLine pos + 1 .&&.
             posColumn newPos === 1 .&&.
             posOffset newPos === posOffset pos + 1

-- Property: posAfter advances column for regular characters
prop_posAfter_regular_char :: SourcePos -> Char -> Property
prop_posAfter_regular_char pos char =
  char /= '\n' && char /= '\t' ==> 
  let newPos = posAfter char pos
  in property $ posLine newPos === posLine pos .&&.
             posColumn newPos === posColumn pos + 1 .&&.
             posOffset newPos === posOffset pos + 1

-- Property: posAfter handles tab correctly (8-space alignment)
prop_posAfter_tab :: SourcePos -> Property
prop_posAfter_tab pos =
  let newPos = posAfter '\t' pos
      expectedCol = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in property $ posLine newPos === posLine pos .&&.
             posColumn newPos === expectedCol .&&.
             posOffset newPos === posOffset pos + 1

-- Property: posAt creates position with correct line L.and column
prop_posAt_correct :: Int -> Int -> Property
prop_posAt_correct line col =
  line > 0 && col > 0 ==>
  let pos = posAt line col
  in property $ posLine pos === line .&&. posColumn pos === col .&&. posOffset pos === 0

-- Property: posAtLineCol creates position with L.all fields
prop_posAtLineCol_correct :: Int -> Int -> Int -> Property
prop_posAtLineCol_correct line col offset =
  line > 0 && col > 0 && offset >= 0 ==>
  let pos = posAtLineCol line col offset
  in property $ posLine pos === line .&&. posColumn pos === col .&&. posOffset pos === offset

-- ============================================================================
-- SourceSpan Properties
-- ============================================================================

-- Property: emptySpan has same start L.and end
prop_emptySpan_same_start_end :: SourcePos -> Property
prop_emptySpan_same_start_end pos =
  let span = emptySpan pos
  in property $ spanStart span === pos .&&. spanEnd span === pos

-- Property: spanFrom creates empty span at position
prop_spanFrom_creates_empty :: SourcePos -> Property
prop_spanFrom_creates_empty pos =
  let span = spanFrom pos
  in property $ spanStart span === pos .&&. spanEnd span === pos

-- Property: spanTo creates empty span at position
prop_spanTo_creates_empty :: SourcePos -> Property
prop_spanTo_creates_empty pos =
  let span = spanTo pos
  in property $ spanStart span === pos .&&. spanEnd span === pos

-- Property: spanBetween creates span with correct bounds
prop_spanBetween_correct_bounds :: SourcePos -> SourcePos -> Property
prop_spanBetween_correct_bounds start end =
  let span = spanBetween start end
  in property $ spanStart span === start .&&. spanEnd span === end

-- Property: mergeSpans contains both original spans
prop_mergeSpans_contains_both :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_contains_both span1 span2 =
  let merged = mergeSpans span1 span2
  in property $ spanStart merged <= spanStart span1 .&&.
             spanEnd merged >= spanEnd span1 .&&.
             spanStart merged <= spanStart span2 .&&.
             spanEnd merged >= spanEnd span2

-- Property: mergeSpans is commutative
prop_mergeSpans_commutative :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_commutative span1 span2 =
  mergeSpans span1 span2 === mergeSpans span2 span1

-- Property: mergeSpans is associative
prop_mergeSpans_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_mergeSpans_associative span1 span2 span3 =
  mergeSpans (mergeSpans span1 span2) span3 === mergeSpans span1 (mergeSpans span2 span3)

-- Property: isValidSpan checks start <= end
prop_isValidSpan_correct :: SourceSpan -> Property
prop_isValidSpan_correct span =
  let start = spanStart span
      end = spanEnd span
      expected = start <= end
  in property $ isValidSpan span === expected

-- ============================================================================
-- Located Properties
-- ============================================================================

-- Property: locatedAt creates located value with empty span
prop_locatedAt_empty_span :: SourcePos -> Int -> Property
prop_locatedAt_empty_span pos value =
  let located = locatedAt pos value
  in property $ locatedValue located === value .&&.
             locatedPos located === pos .&&.
             spanStart (locatedSpan located) === pos .&&.
             spanEnd (locatedSpan located) === pos

-- Property: locatedWithSpan creates located value with given span
prop_locatedWithSpan_correct :: SourceSpan -> String -> Property
prop_locatedWithSpan_correct span value =
  let located = locatedWithSpan span value
  in property $ locatedValue located === value .&&.
             locatedSpan located === span .&&.
             locatedPos located === spanStart span

-- Property: mapLocated preserves location
prop_mapLocated_preserves_location :: SourceSpan -> Int -> Property
prop_mapLocated_preserves_location span value =
  let located = locatedWithSpan span value
      mapped = mapLocated (*2) located
  in property $ locatedValue mapped === value * 2 .&&.
             locatedSpan mapped === span .&&.
             locatedPos mapped === spanStart span

-- Property: HasLocation instance works correctly
prop_hasLocation_instance :: Located String -> Property
prop_hasLocation_instance located =
  getLocation located === locatedSpan located

-- ============================================================================
-- Position Advancement Properties
-- ============================================================================

-- Property: advancePos L.and posAfter are the same
prop_advancePos_equals_posAfter :: SourcePos -> Char -> Property
prop_advancePos_equals_posAfter pos char =
  advancePos char pos === posAfter char pos

-- Property: advancePosBy is consistent with repeated advancePos
prop_advancePosBy_consistent :: SourcePos -> String -> Property
prop_advancePosBy_consistent pos chars =
  let advanced = advancePosBy chars pos
      manual = L.foldl (flip advancePos) pos chars
  in property $ advanced === manual

-- Property: advancePosByText is consistent with advancePosBy
prop_advancePosByText_consistent :: SourcePos -> Text -> Property
prop_advancePosByText_consistent pos text =
  let textAdvanced = advancePosByText text pos
      stringAdvanced = advancePosBy (T.unpack text) pos
  in property $ textAdvanced === stringAdvanced

-- Property: advancePosByLine advances line number
prop_advancePosByLine_advances_line :: SourcePos -> Int -> Property
prop_advancePosByLine_advances_line pos numLines =
  numLines > 0 ==>
  let advanced = advancePosByLine numLines pos
  in property $ posLine advanced === posLine pos + numLines .&&.
             posColumn advanced === 1

-- Property: advancePosByLine preserves line advancement
prop_advancePosByLine_preserves :: SourcePos -> Int -> Int -> Property
prop_advancePosByLine_preserves pos n1 n2 =
  n1 > 0 && n2 > 0 ==>
  let advanced1 = advancePosByLine n1 pos
      advanced2 = advancePosByLine n2 advanced1
      combined = advancePosByLine (n1 + n2) pos
  in property $ advanced2 === combined

-- ============================================================================
-- Error Location Properties
-- ============================================================================

-- Property: toErrorLocation preserves line L.and column
prop_toErrorLocation_preserves_pos :: SourcePos -> Property
prop_toErrorLocation_preserves_pos pos =
  let errLoc = toErrorLocation pos
  in property $ line errLoc === posLine pos .&&.
             column errLoc === posColumn pos

-- Property: toErrorLocationWithSpan preserves span information
prop_toErrorLocationWithSpan_preserves_span :: SourceSpan -> Property
prop_toErrorLocationWithSpan_preserves_span span =
  let errLoc = toErrorLocationWithSpan span
      start = spanStart span
      end = spanEnd span
  in property $ line errLoc === posLine start .&&.
             column errLoc === posColumn start .&&.
             endLine errLoc === Just (posLine end) .&&.
             endColumn errLoc === Just (posColumn end)

-- ============================================================================
-- Complex Interaction Properties
-- ============================================================================

-- Property: Position advancement roundtrip with different methods
prop_position_advancement_roundtrip :: SourcePos -> String -> Property
prop_position_advancement_roundtrip pos chars =
  let advanced1 = advancePosBy chars pos
      -- Simulate going back (simplified)
      roundtrip = advancePosBy (L.reverse chars) advanced1
  in property $ posOffset roundtrip >= posOffset pos

-- Property: Span merging with nested spans
prop_span_merging_nested :: SourcePos -> Int -> Int -> Int -> Property
prop_span_merging_nested pos len1 len2 offset =
  len1 > 0 && len2 > 0 && offset >= 0 && offset <= len1 ==>
  let start1 = pos
      end1 = pos { posOffset = posOffset pos + len1, posColumn = posColumn pos + len1 }
      span1 = spanBetween start1 end1
      nestedStart = pos { posOffset = posOffset pos + offset, posColumn = posColumn pos + offset }
      nestedEnd = pos { posOffset = posOffset pos + offset + len2, posColumn = posColumn pos + offset + len2 }
      nestedSpan = spanBetween nestedStart nestedEnd
      merged = mergeSpans span1 nestedSpan
  in property $ merged === span1

-- Property: Located value mapping chain
prop_located_mapping_chain :: SourceSpan -> Int -> Property
prop_located_mapping_chain span value =
  let located = locatedWithSpan span value
      operations = [+1, (*2), (-1), (`div` 2)]
      result = L.foldl (\acc op -> mapLocated op acc) located operations
      expected = L.foldl (\acc op -> op acc) value operations
  in property $ locatedValue result === expected

-- Property: Complex text advancement with newlines L.and tabs
prop_complex_text_advancement :: SourcePos -> [Char] -> Property
prop_complex_text_advancement pos chars =
  let text = T.pack chars
      finalPos = advancePosByText text pos
      -- Count newlines L.and tabs to verify consistency
      newlineCount = L.length (L.filter (== '\n') chars)
      tabCount = L.length (L.filter (== '\t') chars)
  in property $ posLine finalPos >= posLine pos .&&.
             posLine finalPos <= posLine pos + newlineCount + 1

-- Property: Span validity after merging
prop_span_validity_after_merge :: SourceSpan -> SourceSpan -> Property
prop_span_validity_after_merge span1 span2 =
  let merged = mergeSpans span1 span2
  in property $ isValidSpan merged

-- Property: Location tracking consistency
prop_location_tracking_consistency :: SourcePos -> String -> Property
prop_location_tracking_consistency pos chars =
  let finalPos = advancePosBy chars pos
      span = spanBetween pos finalPos
  in property $ isValidSpan span ==> 
             spanStart span === pos .&&.
             spanEnd span === finalPos

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "SourceLocation New QuickCheck Tests"
  [ testGroup "SourcePos Properties"
    [ fastProperty "startPos is consistent" prop_startPos_consistent
    , fastProperty "posAfter advances line for newline" prop_posAfter_newline
    , fastProperty "posAfter advances column for regular characters" prop_posAfter_regular_char
    , fastProperty "posAfter handles tab correctly" prop_posAfter_tab
    , fastProperty "posAt creates position with correct line L.and column" prop_posAt_correct
    , fastProperty "posAtLineCol creates position with L.all fields" prop_posAtLineCol_correct
    ]

  , testGroup "SourceSpan Properties"
    [ fastProperty "emptySpan has same start L.and end" prop_emptySpan_same_start_end
    , fastProperty "spanFrom creates empty span at position" prop_spanFrom_creates_empty
    , fastProperty "spanTo creates empty span at position" prop_spanTo_creates_empty
    , fastProperty "spanBetween creates span with correct bounds" prop_spanBetween_correct_bounds
    , fastProperty "mergeSpans contains both original spans" prop_mergeSpans_contains_both
    , fastProperty "mergeSpans is commutative" prop_mergeSpans_commutative
    , fastProperty "mergeSpans is associative" prop_mergeSpans_associative
    , fastProperty "isValidSpan checks start <= end" prop_isValidSpan_correct
    ]

  , testGroup "Located Properties"
    [ fastProperty "locatedAt creates located value with empty span" prop_locatedAt_empty_span
    , fastProperty "locatedWithSpan creates located value with given span" prop_locatedWithSpan_correct
    , fastProperty "mapLocated preserves location" prop_mapLocated_preserves_location
    , fastProperty "HasLocation instance works correctly" prop_hasLocation_instance
    ]

  , testGroup "Position Advancement Properties"
    [ fastProperty "advancePos L.and posAfter are the same" prop_advancePos_equals_posAfter
    , fastProperty "advancePosBy is consistent with repeated advancePos" prop_advancePosBy_consistent
    , fastProperty "advancePosByText is consistent with advancePosBy" prop_advancePosByText_consistent
    , fastProperty "advancePosByLine advances line number" prop_advancePosByLine_advances_line
    , fastProperty "advancePosByLine preserves line advancement" prop_advancePosByLine_preserves
    ]

  , testGroup "Error Location Properties"
    [ fastProperty "toErrorLocation preserves line L.and column" prop_toErrorLocation_preserves_pos
    , fastProperty "toErrorLocationWithSpan preserves span information" prop_toErrorLocationWithSpan_preserves_span
    ]

  , testGroup "Complex Interaction Properties"
    [ fastProperty "Position advancement roundtrip with different methods" prop_position_advancement_roundtrip
    , fastProperty "Span merging with nested spans" prop_span_merging_nested
    , fastProperty "Located value mapping chain" prop_located_mapping_chain
    , fastProperty "Complex text advancement with newlines L.and tabs" prop_complex_text_advancement
    , fastProperty "Span validity after merging" prop_span_validity_after_merge
    , fastProperty "Location tracking consistency" prop_location_tracking_consistency
    ]
  ]