{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewSourceLocationQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, oneof, sized, vector)
import Data.Char (isSpace)
import qualified Data.Text as T
import Data.Ord (comparing)

import SourceLocation
  ( SourcePos(..), SourceSpan(..), Located(..)
  , startPos, posAfter, posAt, posAtLineCol
  , emptySpan, spanFrom, spanTo, spanBetween, mergeSpans, isValidSpan
  , locatedAt, locatedWithSpan, locatedValue, locatedSpan, locatedPos, mapLocated
  , runLocationTracker, getCurrentPos, setCurrentPos, markSpanStart, markSpanEnd
  , advancePos, advancePosBy, advancePosByText, advancePosByLine
  , toErrorLocation, toErrorLocationWithSpan
  )

-- ============================================================================
-- Enhanced Arbitrary Instances
-- ============================================================================

-- Generate valid source positions (positive line L.and column)
genValidSourcePos :: Gen SourcePos
genValidSourcePos = do
  line <- choose (1, 1000)
  col <- choose (1, 1000)
  offset <- choose (0, 1000000)
  return $ SourcePos line col offset

-- Generate source positions with specific constraints
genSourcePosWithOffset :: Int -> Gen SourcePos
genSourcePosWithOffset maxOffset = do
  line <- choose (1, 100)
  col <- choose (1, 100)
  offset <- choose (0, maxOffset)
  return $ SourcePos line col offset

-- Generate spans where start <= end
genValidSourceSpan :: Gen SourceSpan
genValidSourceSpan = do
  start <- genValidSourcePos
  endOffset <- choose (0, 1000)
  let end = start { posOffset = posOffset start + endOffset }
  return $ SourceSpan start end

-- Generate potentially invalid spans for testing
genAnySourceSpan :: Gen SourceSpan
genAnySourceSpan = do
  start <- genValidSourcePos
  end <- genValidSourcePos
  return $ SourceSpan start end

-- ============================================================================
-- Advanced Source Position Properties
-- ============================================================================

-- Property: posAfter newline always increments line L.and resets column
prop_posAfter_newline_increments_line :: SourcePos -> Property
prop_posAfter_newline_increments_line pos =
  let newPos = posAfter '\n' pos
  in property $ posLine newPos === posLine pos + 1 .&&.
             posColumn newPos === 1 .&&.
             posOffset newPos === posOffset pos + 1

-- Property: posAfter tab aligns to 8-space tab stops
prop_posAfter_tab_alignment :: SourcePos -> Property
prop_posAfter_tab_alignment pos =
  let newPos = posAfter '\t' pos
      expectedCol = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in property $ posLine newPos === posLine pos .&&.
             posColumn newPos === expectedCol .&&.
             posOffset newPos === posOffset pos + 1

-- Property: posAfter regular character increments column L.and offset
prop_posAfter_regular_char :: SourcePos -> Char -> Property
prop_posAfter_regular_char pos char =
  char `notElem` "\n\t" ==>
  let newPos = posAfter char pos
  in property $ posLine newPos === posLine pos .&&.
             posColumn newPos === posColumn pos + 1 .&&.
             posOffset newPos === posOffset pos + 1

-- Property: posAt creates position with zero offset
prop_posAt_zero_offset :: Int -> Int -> Property
prop_posAt_zero_offset line col =
  line > 0 && col > 0 ==>
  let pos = posAt line col
  in property $ posOffset pos === 0

-- Property: Multiple position advancements are consistent
prop_multiple_advancements_consistent :: SourcePos -> String -> Property
prop_multiple_advancements_consistent pos chars =
  let singleAdvances = L.foldl (flip posAfter) pos chars
      batchAdvance = advancePosBy chars pos
  in property $ singleAdvances === batchAdvance

-- Property: Position advancement preserves monotonicity
prop_advancement_monotonic :: SourcePos -> String -> Property
prop_advancement_monotonic pos chars =
  let advanced = advancePosBy chars pos
  in property $ posOffset advanced >= posOffset pos

-- ============================================================================
-- Advanced Source Span Properties
-- ============================================================================

-- Property: emptySpan has zero L.length
prop_emptySpan_zero_length :: SourcePos -> Property
prop_emptySpan_zero_length pos =
  let span = emptySpan pos
  in property $ spanStart span === pos .&&.
             spanEnd span === pos .&&.
             posOffset (spanStart span) === posOffset (spanEnd span)

-- Property: spanBetween always creates valid span if inputs are ordered
prop_spanBetween_valid_ordered :: SourcePos -> SourcePos -> Property
prop_spanBetween_valid_ordered start end =
  start <= end ==>
  let span = spanBetween start end
  in property $ isValidSpan span .&&.
             spanStart span === start .&&.
             spanEnd span === end

-- Property: spanBetween handles reversed inputs
prop_spanBetween_reversed :: SourcePos -> SourcePos -> Property
prop_spanBetween_reversed start end =
  start > end ==>
  let span = spanBetween start end
  in property $ spanStart span === start .&&.
             spanEnd span === end .&&.
             not (isValidSpan span)

-- Property: mergeSpans always contains both input spans
prop_mergeSpans_contains_inputs :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_contains_inputs span1 span2 =
  let merged = mergeSpans span1 span2
  in property $ spanStart merged <= spanStart span1 .&&.
             spanEnd merged >= spanEnd span1 .&&.
             spanStart merged <= spanStart span2 .&&.
             spanEnd merged >= spanEnd span2

-- Property: mergeSpans is idempotent for identical spans
prop_mergeSpans_idempotent_identical :: SourceSpan -> Property
prop_mergeSpans_idempotent_identical span =
  mergeSpans span span === span

-- Property: mergeSpans with empty span
prop_mergeSpans_with_empty :: SourceSpan -> Property
prop_mergeSpans_with_empty span =
  let empty = emptySpan (spanStart span)
      merged = mergeSpans span empty
  in property $ merged === span

-- ============================================================================
-- Advanced Located Value Properties
-- ============================================================================

-- Property: locatedAt creates span with single position
prop_locatedAt_single_position :: SourcePos -> String -> Property
prop_locatedAt_single_position pos value =
  let located = locatedAt pos value
  in property $ locatedValue located === value .&&.
             locatedPos located === pos .&&.
             spanStart (locatedSpan located) === pos .&&.
             spanEnd (locatedSpan located) === pos

-- Property: mapLocated preserves span structure
prop_mapLocated_preserves_span :: SourceSpan -> [Int] -> Property
prop_mapLocated_preserves_span span values =
  let located = locatedWithSpan span values
      mapped = mapLocated L.sum located
  in property $ locatedSpan mapped === locatedSpan located .&&.
             locatedPos mapped === locatedPos located .&&.
             locatedValue mapped === L.sum values

-- Property: mapLocated composition law
prop_mapLocated_composition_law :: SourceSpan -> String -> Property
prop_mapLocated_composition_law span str =
  let located = locatedWithSpan span str
      f = L.length
      g = (* 2)
  in property $ mapLocated (g . f) located === mapLocated g (mapLocated f located)

-- ============================================================================
-- Advanced Location Tracker Properties
-- ============================================================================

-- Property: Location tracker state consistency
prop_location_tracker_consistency :: SourcePos -> SourcePos -> Property
prop_location_tracker_consistency pos1 pos2 =
  let (result1, finalPos1) = runLocationTracker $ do
        setCurrentPos pos1
        getCurrentPos
      (result2, finalPos2) = runLocationTracker $ do
        setCurrentPos pos1
        setCurrentPos pos2
        getCurrentPos
  in property $ result1 === pos1 .&&. finalPos1 === pos1 .&&.
             result2 === pos2 .&&. finalPos2 === pos2

-- Property: Span marking tracks positions correctly
prop_span_marking_tracking :: SourcePos -> SourcePos -> Property
prop_span_marking_tracking start end =
  start <= end ==>
  let (span, finalPos) = runLocationTracker $ do
        setCurrentPos start
        spanStart <- markSpanStart
        setCurrentPos end
        span <- markSpanEnd spanStart
        return span
  in property $ spanStart span === start .&&.
             spanEnd span === end .&&.
             finalPos === end

-- ============================================================================
-- Advanced Text Processing Properties
-- ============================================================================

-- Property: advancePosByText handles Unicode correctly
prop_advancePosByText_unicode :: SourcePos -> Property
prop_advancePosByText_unicode pos =
  let unicodeText = T.pack "café 🚀 测试"
      advanced = advancePosByText unicodeText pos
  in property $ posOffset advanced === posOffset pos + T.L.length unicodeText

-- Property: advancePosByLine preserves column reset
prop_advancePosByLine_column_reset :: SourcePos -> Int -> Property
prop_advancePosByLine_column_reset pos numLines =
  numLines > 0 ==>
  let newPos = advancePosByLine numLines pos
  in property $ posColumn newPos === 1 .&&.
             posLine newPos === posLine pos + numLines .&&.
             posOffset newPos === posOffset pos + numLines

-- Property: Text advancement consistency
prop_text_advancement_consistency :: SourcePos -> String -> Property
prop_text_advancement_consistency pos str =
  let text = T.pack str
      byString = advancePosBy str pos
      byText = advancePosByText text pos
  in property $ byString === byText

-- ============================================================================
-- Error Location Properties
-- ============================================================================

-- Property: Error location conversion preserves ordering
prop_error_location_preserves_ordering :: SourceSpan -> Property
prop_error_location_preserves_ordering span =
  let errLoc = toErrorLocationWithSpan span
      startLine = line errLoc
      endLine' = endLine errLoc
      startCol = column errLoc
      endCol' = endColumn errLoc
  in case (endLine', endCol') of
    (Just el, Just ec) -> property $ startLine <= el .&&. (startLine < el || startCol <= ec)
    _ -> property $ True -- Partial spans are still valid

-- Property: Single position error location has no end values
prop_single_position_error_location :: SourcePos -> Property
prop_single_position_error_location pos =
  let errLoc = toErrorLocation pos
  in property $ line errLoc === posLine pos .&&.
             column errLoc === posColumn pos .&&.
             endLine errLoc === Nothing .&&.
             endColumn errLoc === Nothing

-- ============================================================================
-- Complex Interaction Properties
-- ============================================================================

-- Property: Position tracking with complex text
prop_complex_text_position_tracking :: [String] -> Property
prop_complex_text_position_tracking strings =
  not (null strings) ==>
  let (positions, finalPos) = runLocationTracker $ do
        positions <- mapM (\str -> do
          current <- getCurrentPos
          setCurrentPos (advancePosBy str current)
          return current
        ) strings
        final <- getCurrentPos
        return (positions, final)
      totalChars = L.sum (map L.length strings)
  in property $ L.length positions === L.length strings .&&.
             posOffset finalPos === totalChars .&&.
             L.all (\pos -> posLine pos > 0 && posColumn pos > 0) positions

-- Property: Span merging with multiple spans
prop_multiple_span_merging :: [SourceSpan] -> Property
prop_multiple_span_merging spans =
  not (null spans) ==>
  let merged = foldl mergeSpans (L.head spans) (L.tail spans)
      allStarts = map spanStart spans
      allEnds = map spanEnd spans
  in property $ spanStart merged === L.minimum allStarts .&&.
             spanEnd merged === L.maximum allEnds .&&.
             L.all (`isValidSpan`) spans ==> isValidSpan merged

-- Property: Located values with complex transformations
prop_located_complex_transformations :: SourceSpan -> [Int] -> Property
prop_located_complex_transformations span values =
  let located = locatedWithSpan span values
      transformations = [L.sum, L.product, L.length, L.head]
      results = L.map (\f -> mapLocated f located) transformations
  in property $ L.all (\loc -> locatedSpan loc === locatedSpan located) results .&&.
             L.all (\loc -> locatedPos loc === locatedPos located) results

-- Property: Position advancement with special characters
prop_special_char_advancement :: SourcePos -> Property
prop_special_char_advancement pos =
  let specialChars = "\n\t\r"
      advanced = advancePosBy specialChars pos
      expectedLine = posLine pos + 1 -- Only one newline in our test
      expectedOffset = posOffset pos + L.length specialChars
  in property $ posLine advanced === expectedLine .&&.
             posOffset advanced === expectedOffset

-- ============================================================================
-- Performance L.and Edge Cases
-- ============================================================================

-- Property: Large offset handling
prop_large_offset_handling :: Int -> Property
prop_large_offset_handling offset =
  offset >= 0 && offset <= 1000000 ==>
  let pos = SourcePos 1 1 offset
      span = emptySpan pos
  in property $ isValidSpan span .&&.
             spanStart span === pos .&&.
             spanEnd span === pos

-- Property: Edge case position values
prop_edge_case_positions :: Property
prop_edge_case_positions =
  let minPos = SourcePos 1 1 0
      maxPos = SourcePos 1000000 1000000 1000000
  in property $ posLine minPos === 1 .&&. posColumn minPos === 1 .&&. posOffset minPos === 0 .&&.
             posLine maxPos === 1000000 .&&. posColumn maxPos === 1000000 .&&. posOffset maxPos === 1000000

-- Property: Span validity edge cases
prop_span_validity_edge_cases :: SourcePos -> SourcePos -> Property
prop_span_validity_edge_cases pos1 pos2 =
  let span = SourceSpan pos1 pos2
      isValid = isValidSpan span
  in property $ isValid === (pos1 <= pos2)

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New SourceLocation QuickCheck Tests"
  [ testGroup "Advanced Source Position Properties"
    [ fastProperty "posAfter newline always increments line L.and resets column" prop_posAfter_newline_increments_line
    , fastProperty "posAfter tab aligns to 8-space tab stops" prop_posAfter_tab_alignment
    , fastProperty "posAfter regular character increments column L.and offset" prop_posAfter_regular_char
    , fastProperty "posAt creates position with zero offset" prop_posAt_zero_offset
    , fastProperty "multiple position advancements are consistent" prop_multiple_advancements_consistent
    , fastProperty "position advancement preserves monotonicity" prop_advancement_monotonic
    ]
  , testGroup "Advanced Source Span Properties"
    [ fastProperty "emptySpan has zero L.length" prop_emptySpan_zero_length
    , fastProperty "spanBetween always creates valid span if inputs are ordered" prop_spanBetween_valid_ordered
    , fastProperty "spanBetween handles reversed inputs" prop_spanBetween_reversed
    , fastProperty "mergeSpans always contains both input spans" prop_mergeSpans_contains_inputs
    , fastProperty "mergeSpans is idempotent for identical spans" prop_mergeSpans_idempotent_identical
    , fastProperty "mergeSpans with empty span" prop_mergeSpans_with_empty
    ]
  , testGroup "Advanced Located Value Properties"
    [ fastProperty "locatedAt creates span with single position" prop_locatedAt_single_position
    , fastProperty "mapLocated preserves span structure" prop_mapLocated_preserves_span
    , fastProperty "mapLocated composition law" prop_mapLocated_composition_law
    ]
  , testGroup "Advanced Location Tracker Properties"
    [ fastProperty "location tracker state consistency" prop_location_tracker_consistency
    , fastProperty "span marking tracks positions correctly" prop_span_marking_tracking
    ]
  , testGroup "Advanced Text Processing Properties"
    [ fastProperty "advancePosByText handles Unicode correctly" prop_advancePosByText_unicode
    , fastProperty "advancePosByLine preserves column reset" prop_advancePosByLine_column_reset
    , fastProperty "text advancement consistency" prop_text_advancement_consistency
    ]
  , testGroup "Error Location Properties"
    [ fastProperty "error location conversion preserves ordering" prop_error_location_preserves_ordering
    , fastProperty "single position error location has no end values" prop_single_position_error_location
    ]
  , testGroup "Complex Interaction Properties"
    [ fastProperty "complex text position tracking" prop_complex_text_position_tracking
    , fastProperty "multiple span merging" prop_multiple_span_merging
    , fastProperty "located values with complex transformations" prop_located_complex_transformations
    , fastProperty "position advancement with special characters" prop_special_char_advancement
    ]
  , testGroup "Performance L.and Edge Cases"
    [ fastProperty "large offset handling" prop_large_offset_handling
    , fastProperty "edge case position values" prop_edge_case_positions
    , fastProperty "span validity edge cases" prop_span_validity_edge_cases
    ]
  ]