{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewSourceLocationComprehensiveSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Gen, Arbitrary, arbitrary, oneof, elements, listOf, resize, choose)
import Data.Char (isSpace)
import Data.List (sort)
import qualified Data.Text as T

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
  , LocationTracker
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

-- ============================================================================
-- Arbitrary Instances
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
    -- Ensure end is after start
    let realEnd = if end >= start then end else start
    return $ SourceSpan start realEnd

instance Arbitrary a => Arbitrary (Located a) where
  arbitrary = do
    value <- arbitrary
    pos <- arbitrary
    span <- arbitrary
    return $ Located value pos span

-- ============================================================================
-- Property Tests
-- ============================================================================

-- Property: startPos is the origin position
prop_start_pos_properties :: Property
prop_start_pos_properties =
  property $ posLine startPos === 1 .&&.
             posColumn startPos === 1 .&&.
             posOffset startPos === 0

-- Property: posAfter correctly handles different characters
prop_posAfter_character_handling :: Property
prop_posAfter_character_handling =
  forAll arbitrary $ \pos ->
    let newlinePos = posAfter '\n' pos
        tabPos = posAfter '\t' pos
        normalPos = posAfter 'a' pos
    in property $ posLine newlinePos === posLine pos + 1 .&&.
               posColumn newlinePos === 1 .&&.
               posOffset newlinePos === posOffset pos + 1 .&&.
               posColumn tabPos >= posColumn pos + 1 .&&.
               posColumn normalPos === posColumn pos + 1

-- Property: posAt creates positions correctly
prop_posAt_creation :: Property
prop_posAt_creation =
  forAll (choose (1, 1000)) $ \line ->
  forAll (choose (1, 1000)) $ \col ->
    let pos = posAt line col
    in property $ posLine pos === line .&&.
               posColumn pos === col .&&.
               posOffset pos === 0

-- Property: spanBetween creates valid spans
prop_span_between_valid :: Property
prop_span_between_valid =
  forAll arbitrary $ \start ->
  forAll arbitrary $ \end ->
    let span = spanBetween start end
        valid = isValidSpan span
    in property $ spanStart span === start .&&.
               spanEnd span === end .&&.
               (valid === (start <= end))

-- Property: mergeSpans combines spans correctly
prop_merge_spans_combination :: Property
prop_merge_spans_combination =
  forAll arbitrary $ \span1 ->
  forAll arbitrary $ \span2 ->
    let merged = mergeSpans span1 span2
    in property $ spanStart merged === min (spanStart span1) (spanStart span2) .&&.
               spanEnd merged === max (spanEnd span1) (spanEnd span2) .&&.
               isValidSpan merged

-- Property: locatedAt creates located values correctly
prop_located_at_creation :: Property
prop_located_at_creation =
  forAll arbitrary $ \pos ->
  forAll arbitrary $ \value ->
    let located = locatedAt pos value
    in property $ locatedValue located === value .&&.
               locatedPos located === pos .&&.
               locatedSpan located === emptySpan pos

-- Property: locatedWithSpan creates located values with spans
prop_located_with_span_creation :: Property
prop_located_with_span_creation =
  forAll arbitrary $ \span ->
  forAll arbitrary $ \value ->
    let located = locatedWithSpan span value
    in property $ locatedValue located === value .&&.
               locatedSpan located === span .&&.
               locatedPos located === spanStart span

-- Property: mapLocated preserves location
prop_map_located_preservation :: Property
prop_map_located_preservation =
  forAll arbitrary $ \located ->
  forAll arbitrary $ \f ->
    let mapped = mapLocated f located
    in property $ locatedPos mapped === locatedPos located .&&.
               locatedSpan mapped === locatedSpan located

-- Property: advancePosBy processes strings correctly
prop_advance_pos_by_string :: Property
prop_advance_pos_by_string =
  forAll arbitrary $ \pos ->
  forAll (listOf (elements "abc\n\t")) $ \chars ->
    let advanced = advancePosBy chars pos
        manual = foldl (flip advancePos) pos chars
    in property $ advanced === manual

-- Property: advancePosByText handles Text correctly
prop_advance_pos_by_text :: Property
prop_advance_pos_by_text =
  forAll arbitrary $ \pos ->
  forAll (listOf (elements "abc\n\t")) $ \chars ->
    let text = T.pack chars
        advanced = advancePosByText text pos
        manual = advancePosBy chars pos
    in property $ advanced === manual

-- Property: advancePosByLine changes line and resets column
prop_advance_pos_by_line :: Property
prop_advance_pos_by_line =
  forAll arbitrary $ \pos ->
  forAll (choose (1, 100)) $ \numLines ->
    let advanced = advancePosByLine numLines pos
    in property $ posLine advanced === posLine pos + numLines .&&.
               posColumn advanced === 1

-- Property: LocationTracker maintains correct position
prop_location_tracker_position :: Property
prop_location_tracker_position =
  forAll arbitrary $ \initialPos ->
  forAll arbitrary $ \finalPos ->
    let result = runLocationTracker $ do
          setCurrentPos initialPos
          getCurrentPos
    in property $ result === initialPos

-- Property: markSpanStart and markSpanEnd work together
prop_span_marking :: Property
prop_span_marking =
  forAll arbitrary $ \startPos ->
  forAll (listOf (elements "abc")) $ \chars ->
    let (span, finalPos) = withLocationTracking startPos $ do
          start <- markSpanStart
          setCurrentPos (advancePosBy chars start)
          markSpanEnd start
    in property $ spanStart span === startPos .&&.
               spanEnd span === advancePosBy chars startPos

-- Property: toErrorLocation converts positions correctly
prop_to_error_location_position :: Property
prop_to_error_location_position =
  forAll arbitrary $ \pos ->
    let errorLoc = toErrorLocation pos
    in property $ line errorLoc === posLine pos .&&.
               column errorLoc === posColumn pos

-- Property: toErrorLocationWithSpan converts spans correctly
prop_to_error_location_span :: Property
prop_to_error_location_span =
  forAll arbitrary $ \span ->
    let errorLoc = toErrorLocationWithSpan span
    in property $ line errorLoc === posLine (spanStart span) .&&.
               column errorLoc === posColumn (spanStart span) .&&.
               endLine errorLoc === Just (posLine (spanEnd span)) .&&.
               endColumn errorLoc === Just (posColumn (spanEnd span))

-- Property: SourcePos ordering works correctly
prop_source_pos_ordering :: Property
prop_source_pos_ordering =
  forAll arbitrary $ \pos1 ->
  forAll arbitrary $ \pos2 ->
    let comparison = pos1 `compare` pos2
        offset1 = posOffset pos1
        offset2 = posOffset pos2
        expected = offset1 `compare` offset2
    in property $ comparison === expected

-- ============================================================================
-- Unit Tests
-- ============================================================================

-- Test basic position creation
test_position_creation :: TestTree
test_position_creation =
  testCase "Position creation" $ do
    let pos1 = posAt 5 10
        pos2 = posAtLineCol 3 7 100
    posLine pos1 @?= 5
    posColumn pos1 @?= 10
    posOffset pos1 @?= 0
    posLine pos2 @?= 3
    posColumn pos2 @?= 7
    posOffset pos2 @?= 100

-- Test position advancement
test_position_advancement :: TestTree
test_position_advancement =
  testCase "Position advancement" $ do
    let start = posAt 1 1
        afterNewline = posAfter '\n' start
        afterTab = posAfter '\t' start
        afterChar = posAfter 'a' start
    posLine afterNewline @?= 2
    posColumn afterNewline @?= 1
    posColumn afterTab @?= 9  -- Tab advances to next tab stop
    posColumn afterChar @?= 2

-- Test span operations
test_span_operations :: TestTree
test_span_operations =
  testCase "Span operations" $ do
    let start = posAt 1 5
        end = posAt 2 10
        span1 = spanBetween start end
        span2 = spanFrom start
        merged = mergeSpans span1 span2
    spanStart span1 @?= start
    spanEnd span1 @?= end
    spanStart span2 @?= start
    spanEnd span2 @?= start
    spanStart merged @?= start
    spanEnd merged @?= end

-- Test located values
test_located_values :: TestTree
test_located_values =
  testCase "Located values" $ do
    let pos = posAt 3 7
        span = spanBetween pos (posAt 3 10)
        located1 = locatedAt pos "test"
        located2 = locatedWithSpan span 42
        mapped = mapLocated (*2) located2
    locatedValue located1 @?= "test"
    locatedPos located1 @?= pos
    locatedValue located2 @?= 42
    locatedSpan located2 @?= span
    locatedValue mapped @?= 84
    locatedPos mapped @?= pos

-- Test location tracking
test_location_tracking :: TestTree
test_location_tracking =
  testCase "Location tracking" $ do
    let initial = posAt 1 1
        (result, final) = withLocationTracking initial $ do
          setCurrentPos (posAt 2 5)
          getCurrentPos
    result @?= posAt 2 5
    final @?= posAt 2 5

-- Test span marking
test_span_marking :: TestTree
test_span_marking =
  testCase "Span marking" $ do
    let start = posAt 1 1
        (span, _) = withLocationTracking start $ do
          spanStart <- markSpanStart
          setCurrentPos (posAt 1 10)
          markSpanEnd spanStart
    spanStart span @?= start
    spanEnd span @?= posAt 1 10

-- Test error location conversion
test_error_location_conversion :: TestTree
test_error_location_conversion =
  testCase "Error location conversion" $ do
    let pos = posAt 5 15
        span = spanBetween (posAt 5 10) (posAt 6 20)
        errorPos = toErrorLocation pos
        errorSpan = toErrorLocationWithSpan span
    line errorPos @?= 5
    column errorPos @?= 15
    line errorSpan @?= 5
    column errorSpan @?= 10
    endLine errorSpan @?= Just 6
    endColumn errorSpan @?= Just 20

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests =
  testGroup "New SourceLocation Comprehensive Tests"
    [ testGroup "Property-based tests"
        [ fastProperty "startPos is the origin position" prop_start_pos_properties
        , fastProperty "posAfter correctly handles different characters" prop_posAfter_character_handling
        , fastProperty "posAt creates positions correctly" prop_posAt_creation
        , fastProperty "spanBetween creates valid spans" prop_span_between_valid
        , fastProperty "mergeSpans combines spans correctly" prop_merge_spans_combination
        , fastProperty "locatedAt creates located values correctly" prop_located_at_creation
        , fastProperty "locatedWithSpan creates located values with spans" prop_located_with_span_creation
        , fastProperty "mapLocated preserves location" prop_map_located_preservation
        , fastProperty "advancePosBy processes strings correctly" prop_advance_pos_by_string
        , fastProperty "advancePosByText handles Text correctly" prop_advance_pos_by_text
        , fastProperty "advancePosByLine changes line and resets column" prop_advance_pos_by_line
        , fastProperty "LocationTracker maintains correct position" prop_location_tracker_position
        , fastProperty "markSpanStart and markSpanEnd work together" prop_span_marking
        , fastProperty "toErrorLocation converts positions correctly" prop_to_error_location_position
        , fastProperty "toErrorLocationWithSpan converts spans correctly" prop_to_error_location_span
        , fastProperty "SourcePos ordering works correctly" prop_source_pos_ordering
        ]
    , testGroup "Unit tests"
        [ test_position_creation
        , test_position_advancement
        , test_span_operations
        , test_located_values
        , test_location_tracking
        , test_span_marking
        , test_error_location_conversion
        ]
    ]