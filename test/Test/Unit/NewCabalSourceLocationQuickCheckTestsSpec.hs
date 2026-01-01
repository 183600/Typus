{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewCabalSourceLocationQuickCheckTestsSpec where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), forAll, counterexample, suchThat)
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
  , advancePosByText
  , advancePosByLine
  , toErrorLocation
  , toErrorLocationWithSpan
  )
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import Data.Char (isSpace)

-- ============================================================================
-- QuickCheck Generators
-- ============================================================================

-- Generate valid source positions (line L.and column >= 1, offset >= 0)
genSourcePos :: Gen SourcePos
genSourcePos = do
  line <- choose (1, 1000)
  col <- choose (1, 1000)
  offset <- choose (0, 1000000)
  return $ SourcePos line col offset

-- Generate source spans
genSourceSpan :: Gen SourceSpan
genSourceSpan = do
  start <- genSourcePos
  -- Ensure end is after start
  let minEndLine = posLine start
      minEndCol = if posLine start == minEndLine then max (posColumn start) (posColumn start + 1) else 1
  endLine <- choose (minEndLine, minEndLine + 100)
  endCol <- if endLine == posLine start 
            then choose (minEndCol, minEndCol + 100) 
            else choose (1, 1000)
  let endOffset = posOffset start + (endLine - posLine start) * 100 + (endCol - posColumn start)
  return $ SourceSpan start (SourcePos endLine endCol endOffset)

-- Generate located values
genLocatedInt :: Gen (Located Int)
genLocatedInt = do
  pos <- genSourcePos
  value <- choose (-1000, 1000)
  return $ locatedAt pos value

-- Generate strings for position advancement
genAdvancementString :: Gen String
genAdvancementString = do
  L.length' <- choose (0, 100)
  listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n\r.,;:!?()[]{}<>+-*/%=|&^~@#`'\""

-- Generate text for position advancement
genAdvancementText :: Gen Text
genAdvancementText = T.pack <$> genAdvancementString

-- ============================================================================
-- Properties for SourcePos
-- ============================================================================

prop_posAt_consistency :: Int -> Int -> Property
prop_posAt_consistency line col =
  let pos1 = posAt line col
      pos2 = posAtLineCol line col 0
  in posLine pos1 === posLine pos2 &&
     posColumn pos1 === posColumn pos2

prop_startPos_valid :: Property
prop_startPos_valid =
  posLine startPos === 1 &&
  posColumn startPos === 1 &&
  posOffset startPos === 0

prop_posAfter_newline_increments_line :: SourcePos -> Property
prop_posAfter_newline_increments_line pos =
  let newPos = posAfter '\n' pos
  in posLine newPos === posLine pos + 1 &&
     posColumn newPos === 1 &&
     posOffset newPos === posOffset pos + 1

prop_posAfter_tab_advances_to_next_tab_stop :: SourcePos -> Property
prop_posAfter_tab_advances_to_next_tab_stop pos =
  let newPos = posAfter '\t' pos
      expectedCol = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in posColumn newPos === expectedCol &&
     posLine newPos === posLine pos &&
     posOffset newPos === posOffset pos + 1

prop_posAfter_regular_char_increments_column :: SourcePos -> Property
prop_posAfter_regular_char_increments_column pos =
  forAll (suchThat arbitrary (`notElem` "\n\t")) $ \c ->
    let newPos = posAfter c pos
    in posColumn newPos === posColumn pos + 1 &&
       posLine newPos === posLine pos &&
       posOffset newPos === posOffset pos + 1

-- ============================================================================
-- Properties for SourceSpan
-- ============================================================================

prop_emptySpan_same_start_end :: SourcePos -> Property
prop_emptySpan_same_start_end pos =
  let span = emptySpan pos
  in spanStart span === pos &&
     spanEnd span === pos

prop_spanFrom_creates_empty_span :: SourcePos -> Property
prop_spanFrom_creates_empty_span pos =
  spanFrom pos === emptySpan pos

prop_spanTo_creates_empty_span :: SourcePos -> Property
prop_spanTo_creates_empty_span pos =
  spanTo pos === emptySpan pos

prop_spanBetween_order :: SourcePos -> SourcePos -> Property
prop_spanBetween_order pos1 pos2 =
  let span = spanBetween pos1 pos2
      (start, end) = if pos1 <= pos2 then (pos1, pos2) else (pos2, pos1)
  in spanStart span === start &&
     spanEnd span === end

prop_mergeSpans_contains_both :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_contains_both span1 span2 =
  let merged = mergeSpans span1 span2
  in spanStart merged === min (spanStart span1) (spanStart span2) &&
     spanEnd merged === max (spanEnd span1) (spanEnd span2)

prop_isValidSpan_check :: SourceSpan -> Property
prop_isValidSpan_check span =
  isValidSpan span === (spanStart span <= spanEnd span)

-- ============================================================================
-- Properties for Located
-- ============================================================================

prop_locatedAt_position :: SourcePos -> Int -> Property
prop_locatedAt_position pos value =
  let located = locatedAt pos value
  in locatedPos located === pos &&
     locatedValue located === value

prop_locatedWithSpan_span :: SourceSpan -> Int -> Property
prop_locatedWithSpan_span span value =
  let located = locatedWithSpan span value
  in locatedSpan located === span &&
     locatedValue located === value &&
     locatedPos located === spanStart span

prop_mapLocated_preserves_location :: Located Int -> Property
prop_mapLocated_preserves_location located =
  let doubled = mapLocated (*2) located
  in locatedPos doubled === locatedPos located &&
     locatedSpan doubled === locatedSpan located &&
     locatedValue doubled === locatedValue located * 2

-- ============================================================================
-- Properties for Position Advancement
-- ============================================================================

prop_advancePos_consistency :: Char -> SourcePos -> Property
prop_advancePos_consistency c pos =
  advancePos c pos === posAfter c pos

prop_advancePosBy_empty_string :: SourcePos -> Property
prop_advancePosBy_empty_string pos =
  advancePosBy "" pos === pos

prop_advancePosBy_single_char :: Char -> SourcePos -> Property
prop_advancePosBy_single_char c pos =
  advancePosBy [c] pos === posAfter c pos

prop_advancePosByText_consistency :: Property
prop_advancePosByText_consistency =
  forAll genAdvancementText $ \text ->
    forAll genSourcePos $ \pos ->
      advancePosByText text pos === advancePosBy (T.unpack text) pos

prop_advancePosBy_line_increments :: Int -> SourcePos -> Property
prop_advancePosBy_line_increments n pos =
  let newPos = advancePosByLine n pos
  in posLine newPos === posLine pos + n &&
     posColumn newPos === 1

-- ============================================================================
-- Properties for Error Location Conversion
-- ============================================================================

prop_toErrorLocation_preserves_position :: SourcePos -> Property
prop_toErrorLocation_preserves_position pos =
  let errLoc = toErrorLocation pos
  in line errLoc === posLine pos &&
     column errLoc === posColumn pos

prop_toErrorLocationWithSpan_preserves_span :: SourceSpan -> Property
prop_toErrorLocationWithSpan_preserves_span span =
  let errLoc = toErrorLocationWithSpan span
      start = spanStart span
      endPos = spanEnd span
  in line errLoc === posLine start &&
     column errLoc === posColumn start &&
     endLine errLoc === Just (posLine endPos) &&
     endColumn errLoc === Just (posColumn endPos)

-- ============================================================================
-- Properties for Position Mathematics
-- ============================================================================

prop_pos_ordering_consistent :: SourcePos -> SourcePos -> Property
prop_pos_ordering_consistent pos1 pos2 =
  let offset1 = posOffset pos1
      offset2 = posOffset pos2
  in (pos1 <= pos2) === (offset1 <= offset2)

prop_span_length_calculation :: SourceSpan -> Property
prop_span_length_calculation span =
  let expectedLength = posOffset (spanEnd span) - posOffset (spanStart span)
  in expectedLength >= 0

-- ============================================================================
-- Additional Properties for Edge Cases
-- ============================================================================

prop_advancePos_multiple_chars :: Property
prop_advancePos_multiple_chars =
  forAll genAdvancementString $ \s ->
    forAll genSourcePos $ \pos ->
      advancePosBy s pos === L.foldl (flip advancePos) pos s

prop_mergeSpans_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_mergeSpans_associative span1 span2 span3 =
  mergeSpans span1 (mergeSpans span2 span3) === mergeSpans (mergeSpans span1 span2) span3

prop_mergeSpans_idempotent :: SourceSpan -> Property
prop_mergeSpans_idempotent span =
  mergeSpans span span === span

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "SourceLocation QuickCheck Tests"
  [ testGroup "SourcePos"
    [ testProperty "posAt consistency with posAtLineCol" prop_posAt_consistency
    , testProperty "startPos has valid values" prop_startPos_valid
    , testProperty "posAfter newline increments line" prop_posAfter_newline_increments_line
    , testProperty "posAfter tab advances to next tab stop" prop_posAfter_tab_advances_to_next_tab_stop
    , testProperty "posAfter regular char increments column" prop_posAfter_regular_char_increments_column
    ]
  , testGroup "SourceSpan"
    [ testProperty "emptySpan has same start L.and end" prop_emptySpan_same_start_end
    , testProperty "spanFrom creates empty span" prop_spanFrom_creates_empty_span
    , testProperty "spanTo creates empty span" prop_spanTo_creates_empty_span
    , testProperty "spanBetween maintains order" prop_spanBetween_order
    , testProperty "mergeSpans contains both spans" prop_mergeSpans_contains_both
    , testProperty "isValidSpan correctly validates" prop_isValidSpan_check
    ]
  , testGroup "Located"
    [ testProperty "locatedAt preserves position" prop_locatedAt_position
    , testProperty "locatedWithSpan preserves span" prop_locatedWithSpan_span
    , testProperty "mapLocated preserves location" prop_mapLocated_preserves_location
    ]
  , testGroup "Position Advancement"
    [ testProperty "advancePos consistency with posAfter" prop_advancePos_consistency
    , testProperty "advancePosBy empty string" prop_advancePosBy_empty_string
    , testProperty "advancePosBy single character" prop_advancePosBy_single_char
    , testProperty "advancePosByText consistency with advancePosBy" prop_advancePosByText_consistency
    , testProperty "advancePosBy line increments" prop_advancePosBy_line_increments
    ]
  , testGroup "Error Location Conversion"
    [ testProperty "toErrorLocation preserves position" prop_toErrorLocation_preserves_position
    , testProperty "toErrorLocationWithSpan preserves span" prop_toErrorLocationWithSpan_preserves_span
    ]
  , testGroup "Position Mathematics"
    [ testProperty "position ordering consistent with offset" prop_pos_ordering_consistent
    , testProperty "span L.length calculation" prop_span_length_calculation
    ]
  , testGroup "Edge Cases"
    [ testProperty "advancePos multiple chars" prop_advancePos_multiple_chars
    , testProperty "mergeSpans associative" prop_mergeSpans_associative
    , testProperty "mergeSpans idempotent" prop_mergeSpans_idempotent
    ]
  ]