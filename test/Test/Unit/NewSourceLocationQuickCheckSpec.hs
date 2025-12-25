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
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import qualified Test.QuickCheck as QC

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
  , runLocationTracker
  , getCurrentPos
  , setCurrentPos
  , markSpanStart
  , markSpanEnd
  , withLocationTracking
  , toErrorLocation
  , toErrorLocationWithSpan
  , advancePos
  , advancePosBy
  , advancePosByText
  , advancePosByLine
  )

import Compiler.Errors.Core (ErrorLocation(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sort, nub, intercalate)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

-- ============================================================================
-- Arbitrary Instances for QuickCheck Testing
-- ============================================================================

-- Generate arbitrary source position
instance Arbitrary SourcePos where
  arbitrary = do
    line <- QC.choose (1, 1000)
    column <- QC.choose (1, 1000)
    offset <- QC.choose (0, 1000000)
    return $ SourcePos line column offset

-- Generate arbitrary source span
instance Arbitrary SourceSpan where
  arbitrary = do
    start <- QC.arbitrary
    end <- QC.arbitrary
    -- Ensure end is not before start
    let normalizedEnd = if end >= start then end else start
    return $ SourceSpan start normalizedEnd

-- Generate arbitrary located value
instance Arbitrary a => Arbitrary (Located a) where
  arbitrary = do
    value <- QC.arbitrary
    pos <- QC.arbitrary
    span <- QC.arbitrary
    return $ Located value pos span

-- ============================================================================
-- Property Tests for Source Location
-- ============================================================================

-- Property: Start position has correct values
prop_start_position_correct :: Property
prop_start_position_correct =
  property $ posLine startPos === 1 .&&.
             posColumn startPos === 1 .&&.
             posOffset startPos === 0

-- Property: Position after newline increments line and resets column
prop_position_after_newline :: SourcePos -> Property
prop_position_after_newline pos =
  let newPos = posAfter '\n' pos
  in property $ posLine newPos === posLine pos + 1 .&&.
             posColumn newPos === 1 .&&.
             posOffset newPos === posOffset pos + 1

-- Property: Position after tab advances to next tab stop
prop_position_after_tab :: SourcePos -> Property
prop_position_after_tab pos =
  let newPos = posAfter '\t' pos
      expectedColumn = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in property $ posLine newPos === posLine pos .&&.
             posColumn newPos === expectedColumn .&&.
             posOffset newPos === posOffset pos + 1

-- Property: Position after regular character increments column
prop_position_after_regular_char :: Char -> SourcePos -> Property
prop_position_after_regular_char char pos =
  char /= '\n' && char /= '\t' ==>
  let newPos = posAfter char pos
  in property $ posLine newPos === posLine pos .&&.
             posColumn newPos === posColumn pos + 1 .&&.
             posOffset newPos === posOffset pos + 1

-- Property: Position at specific line and column
prop_position_at_line_col :: Int -> Int -> Property
prop_position_at_line_col line col =
  line > 0 && col > 0 ==>
  let pos = posAt line col
  in property $ posLine pos === line .&&.
             posColumn pos === col .&&.
             posOffset pos === 0

-- Property: Position at line, column, and offset
prop_position_at_line_col_offset :: Int -> Int -> Int -> Property
prop_position_at_line_col_offset line col offset =
  line > 0 && col > 0 && offset >= 0 ==>
  let pos = posAtLineCol line col offset
  in property $ posLine pos === line .&&.
             posColumn pos === col .&&.
             posOffset pos === offset

-- Property: Empty span has same start and end
prop_empty_span_same_start_end :: SourcePos -> Property
prop_empty_span_same_start_end pos =
  let span = emptySpan pos
  in property $ spanStart span === pos .&&. spanEnd span === pos

-- Property: Span from position creates empty span
prop_span_from_creates_empty_span :: SourcePos -> Property
prop_span_from_creates_empty_span pos =
  let span = spanFrom pos
  in property $ spanStart span === pos .&&. spanEnd span === pos

-- Property: Span to position creates empty span
prop_span_to_creates_empty_span :: SourcePos -> Property
prop_span_to_creates_empty_span pos =
  let span = spanTo pos
  in property $ spanStart span === pos .&&. spanEnd span === pos

-- Property: Span between positions uses correct order
prop_span_between_correct_order :: SourcePos -> SourcePos -> Property
prop_span_between_correct_order pos1 pos2 =
  let span = spanBetween pos1 pos2
  in property $ spanStart span === min pos1 pos2 .&&.
             spanEnd span === max pos1 pos2

-- Property: Merge spans creates correct bounds
prop_merge_spans_correct_bounds :: SourceSpan -> SourceSpan -> Property
prop_merge_spans_correct_bounds span1 span2 =
  let merged = mergeSpans span1 span2
  in property $ spanStart merged === min (spanStart span1) (spanStart span2) .&&.
             spanEnd merged === max (spanEnd span1) (spanEnd span2)

-- Property: Valid span check works correctly
prop_valid_span_check :: SourceSpan -> Property
prop_valid_span_check span =
  let valid = isValidSpan span
  in property $ valid === (spanStart span <= spanEnd span)

-- Property: Located value at position uses correct span
prop_located_at_uses_correct_span :: SourcePos -> Int -> Property
prop_located_at_uses_correct_span pos value =
  let located = locatedAt pos value
  in property $ locatedValue located === value .&&.
             locatedPos located === pos .&&.
             spanStart (locatedSpan located) === pos .&&.
             spanEnd (locatedSpan located) === pos

-- Property: Located value with span uses correct values
prop_located_with_span_uses_correct_values :: SourceSpan -> Int -> Property
prop_located_with_span_uses_correct_values span value =
  let located = locatedWithSpan span value
  in property $ locatedValue located === value .&&.
             locatedSpan located === span .&&.
             locatedPos located === spanStart span

-- Property: Map located applies function to value
prop_map_located_applies_function :: Int -> Int -> SourceSpan -> Property
prop_map_located_applies_function value increment span =
  let located = locatedWithSpan span value
      mapped = mapLocated (+ increment) located
  in property $ locatedValue mapped === value + increment .&&.
             locatedSpan mapped === span .&&.
             locatedPos mapped === spanStart span

-- Property: HasLocation instance works
prop_has_location_instance :: SourceSpan -> Int -> Property
prop_has_location_instance span value =
  let located = locatedWithSpan span value
  in property $ getLocation located === span

-- Property: Location tracker starts at start position
prop_location_tracker_starts_at_start :: Property
prop_location_tracker_starts_at_start =
  let pos = runLocationTracker getCurrentPos
  in property $ pos === startPos

-- Property: Location tracker position can be set and retrieved
prop_location_tracker_set_get :: SourcePos -> Property
prop_location_tracker_set_get pos =
  let retrievedPos = runLocationTracker $ do
        setCurrentPos pos
        getCurrentPos
  in property $ retrievedPos === pos

-- Property: Location tracker span marking works
prop_location_tracker_span_marking :: SourcePos -> Int -> Property
prop_location_tracker_span_marking startPos offset =
  offset >= 0 ==>
  let endPos = posAtLineCol (posLine startPos) (posColumn startPos + offset) (posOffset startPos + offset)
      (startMarked, endMarked) = runLocationTracker $ do
        setCurrentPos startPos
        start <- markSpanStart
        setCurrentPos endPos
        end <- markSpanEnd start
        return (start, end)
  in property $ startMarked === startPos .&&.
             spanStart endMarked === startPos .&&.
             spanEnd endMarked === endPos

-- Property: With location tracking returns correct result and position
prop_with_location_tracking :: SourcePos -> Int -> Property
prop_with_location_tracking startPos value =
  let (result, finalPos) = withLocationTracking startPos $ do
        setCurrentPos $ posAtLineCol (posLine startPos) (posColumn startPos + 1) (posOffset startPos + 1)
        return value
  in property $ result === value .&&.
             finalPos === posAtLineCol (posLine startPos) (posColumn startPos + 1) (posOffset startPos + 1)

-- Property: Convert position to error location
prop_position_to_error_location :: SourcePos -> Property
prop_position_to_error_location pos =
  let errorLoc = toErrorLocation pos
  in property $ line errorLoc === posLine pos .&&.
             column errorLoc === posColumn pos .&&.
             filePath errorLoc === Nothing .&&.
             endLine errorLoc === Nothing .&&.
             endColumn errorLoc === Nothing

-- Property: Convert span to error location with range
prop_span_to_error_location_with_span :: SourceSpan -> Property
prop_span_to_error_location_with_span span =
  let errorLoc = toErrorLocationWithSpan span
  in property $ line errorLoc === posLine (spanStart span) .&&.
             column errorLoc === posColumn (spanStart span) .&&.
             endLine errorLoc === Just (posLine (spanEnd span)) .&&.
             endColumn errorLoc === Just (posColumn (spanEnd span)) .&&.
             filePath errorLoc === Nothing

-- Property: Advance position by character matches posAfter
prop_advance_pos_matches_pos_after :: Char -> SourcePos -> Property
prop_advance_pos_matches_pos_after char pos =
  let advanced1 = advancePos char pos
      advanced2 = posAfter char pos
  in property $ advanced1 === advanced2

-- Property: Advance position by string works correctly
prop_advance_pos_by_string :: String -> SourcePos -> Property
prop_advance_pos_by_string str pos =
  let advanced = advancePosBy str pos
      expected = foldl (flip advancePos) pos str
  in property $ advanced === expected

-- Property: Advance position by text works correctly
prop_advance_pos_by_text :: Text -> SourcePos -> Property
prop_advance_pos_by_text text pos =
  let advanced = advancePosByText text pos
      expected = advancePosBy (T.unpack text) pos
  in property $ advanced === expected

-- Property: Advance position by line works correctly
prop_advance_pos_by_line :: Int -> SourcePos -> Property
prop_advance_pos_by_line numLines pos =
  numLines >= 0 ==>
  let advanced = advancePosByLine numLines pos
  in property $ posLine advanced === posLine pos + numLines .&&.
             posColumn advanced === 1 .&&.
             posOffset advanced === posOffset pos + numLines

-- Property: Position ordering is consistent with offset
prop_position_ordering_consistent_with_offset :: SourcePos -> SourcePos -> Property
prop_position_ordering_consistent_with_offset pos1 pos2 =
  let ordering = compare pos1 pos2
      offsetOrdering = compare (posOffset pos1) (posOffset pos2)
  in property $ ordering === offsetOrdering

-- Property: Span ordering is consistent with start position
prop_span_ordering_consistent_with_start :: SourceSpan -> SourceSpan -> Property
prop_span_ordering_consistent_with_start span1 span2 =
  let ordering = compare span1 span2
      startOrdering = compare (spanStart span1) (spanStart span2)
  in property $ ordering === startOrdering

-- Property: Located functor laws
prop_located_functor_identity :: SourceSpan -> Int -> Property
prop_located_functor_identity span value =
  let located = locatedWithSpan span value
      mapped = mapLocated id located
  in property $ mapped === located

prop_located_functor_composition :: SourceSpan -> Int -> Int -> Int -> Property
prop_located_functor_composition span value f g =
  let located = locatedWithSpan span value
      mapped1 = mapLocated (f . g) located
      mapped2 = mapLocated f (mapLocated g located)
  in property $ mapped1 === mapped2

-- Property: Source position equality works correctly
prop_source_position_equality :: SourcePos -> SourcePos -> Property
prop_source_position_equality pos1 pos2 =
  let equal = pos1 == pos2
  in property $ equal === (posLine pos1 == posLine pos2 &&
                          posColumn pos1 == posColumn pos2 &&
                          posOffset pos1 == posOffset pos2)

-- Property: Source span equality works correctly
prop_source_span_equality :: SourceSpan -> SourceSpan -> Property
prop_source_span_equality span1 span2 =
  let equal = span1 == span2
  in property $ equal === (spanStart span1 == spanStart span2 &&
                          spanEnd span1 == spanEnd span2)

-- Property: Located equality works correctly
prop_located_equality :: SourceSpan -> Int -> Int -> Property
prop_located_equality span value1 value2 =
  let located1 = locatedWithSpan span value1
      located2 = locatedWithSpan span value2
      equal = located1 == located2
  in property $ equal === (value1 == value2)

tests :: TestTree
tests =
  testGroup "New Source Location QuickCheck Tests"
    [ fastProperty "Start position has correct values" prop_start_position_correct
    , fastProperty "Position after newline increments line and resets column" prop_position_after_newline
    , fastProperty "Position after tab advances to next tab stop" prop_position_after_tab
    , fastProperty "Position after regular character increments column" prop_position_after_regular_char
    , fastProperty "Position at specific line and column" prop_position_at_line_col
    , fastProperty "Position at line, column, and offset" prop_position_at_line_col_offset
    , fastProperty "Empty span has same start and end" prop_empty_span_same_start_end
    , fastProperty "Span from position creates empty span" prop_span_from_creates_empty_span
    , fastProperty "Span to position creates empty span" prop_span_to_creates_empty_span
    , fastProperty "Span between positions uses correct order" prop_span_between_correct_order
    , fastProperty "Merge spans creates correct bounds" prop_merge_spans_correct_bounds
    , fastProperty "Valid span check works correctly" prop_valid_span_check
    , fastProperty "Located value at position uses correct span" prop_located_at_uses_correct_span
    , fastProperty "Located value with span uses correct values" prop_located_with_span_uses_correct_values
    , fastProperty "Map located applies function to value" prop_map_located_applies_function
    , fastProperty "HasLocation instance works" prop_has_location_instance
    , fastProperty "Location tracker starts at start position" prop_location_tracker_starts_at_start
    , fastProperty "Location tracker position can be set and retrieved" prop_location_tracker_set_get
    , fastProperty "Location tracker span marking works" prop_location_tracker_span_marking
    , fastProperty "With location tracking returns correct result and position" prop_with_location_tracking
    , fastProperty "Convert position to error location" prop_position_to_error_location
    , fastProperty "Convert span to error location with range" prop_span_to_error_location_with_span
    , fastProperty "Advance position by character matches posAfter" prop_advance_pos_matches_pos_after
    , fastProperty "Advance position by string works correctly" prop_advance_pos_by_string
    , fastProperty "Advance position by text works correctly" prop_advance_pos_by_text
    , fastProperty "Advance position by line works correctly" prop_advance_pos_by_line
    , fastProperty "Position ordering is consistent with offset" prop_position_ordering_consistent_with_offset
    , fastProperty "Span ordering is consistent with start position" prop_span_ordering_consistent_with_start
    , fastProperty "Located functor identity" prop_located_functor_identity
    , fastProperty "Located functor composition" prop_located_functor_composition
    , fastProperty "Source position equality works correctly" prop_source_position_equality
    , fastProperty "Source span equality works correctly" prop_source_span_equality
    , fastProperty "Located equality works correctly" prop_located_equality
    ]