{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.NewCabalSourceLocationQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), (==>))
import TestSupport.QuickCheck (fastProperty)
import qualified Data.Text as T
import GHC.Generics (Generic)

import SourceLocation 
  ( SourcePos(..), SourceSpan(..), Located(..)
  , startPos, posAfter, posAt, posAtLineCol
  , emptySpan, spanFrom, spanTo, spanBetween, mergeSpans, isValidSpan
  , locatedAt, locatedWithSpan, locatedValue, locatedSpan, locatedPos, mapLocated
  , advancePos, advancePosBy, toErrorLocation, toErrorLocationWithSpan
  )

-- ============================================================================
-- Test Data Generators
-- ============================================================================

-- | Generate non-negative integers for line/column numbers
genNonNegativeInt :: Gen Int
genNonNegativeInt = getNonNegative <$> arbitrary

-- | Generate positive integers for line/column numbers  
genPositiveInt :: Gen Int
genPositiveInt = getPositive <$> arbitrary

-- | Generate valid source positions
genSourcePos :: Gen SourcePos
genSourcePos = SourcePos <$> genNonNegativeInt <*> genPositiveInt <*> genPositiveInt

-- | Generate valid source spans (start <= end)
genSourceSpan :: Gen SourceSpan
genSourceSpan = do
  startLine <- genNonNegativeInt
  startCol <- genPositiveInt
  endLine <- genNonNegativeInt
  endCol <- genPositiveInt
  -- Ensure span is valid by ordering positions
  let (line1, col1, line2, col2) = if (startLine, startCol) <= (endLine, endCol)
                                   then (startLine, startCol, endLine, endCol)
                                   else (endLine, endCol, startLine, startCol)
  return $ SourceSpan (SourcePos line1 col1 0) (SourcePos line2 col2 0)

-- | Generate located values
genLocated :: Gen a -> Gen (Located a)
genLocated gen = Located <$> genSourceSpan <*> gen

-- | Generate text for position advancement tests
genText :: Gen String
genText = listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n"

instance Arbitrary SourcePos where
  arbitrary = genSourcePos

instance Arbitrary SourceSpan where
  arbitrary = genSourceSpan

-- ============================================================================
-- Source Position Property Tests
-- ============================================================================

-- | Property: startPos should have line 0, column 1
prop_start_pos_properties :: Property
prop_start_pos_properties =
  let pos = startPos
  in posLine pos === 0 .&&. posColumn pos === 1

-- | Property: posAfter should advance column by 1 on same line
prop_pos_after_same_line :: SourcePos -> Property
prop_pos_after_same_line pos =
  let nextPos = posAfter pos
  in posLine nextPos === posLine pos .&&. posColumn nextPos === posColumn pos + 1

-- | Property: posAt should create position with given line and column
prop_pos_at_creation :: Int -> Int -> Property
prop_pos_at_creation line col =
  let pos = posAt line col
      validLine = line >= 0
      validCol = col > 0
  in validLine .&&. validCol ==> posLine pos === line .&&. posColumn pos === col

-- | Property: posAtLineCol should create position with given line and column
prop_pos_at_line_col :: Int -> Int -> Property
prop_pos_at_line_col line col =
  let pos = posAtLineCol line col
      validLine = line >= 0
      validCol = col > 0
  in validLine .&&. validCol ==> posLine pos === line .&&. posColumn pos === col

-- | Property: advancePos should handle empty text correctly
prop_advance_pos_empty :: SourcePos -> Property
prop_advance_pos_empty pos =
  let newPos = advancePos pos ""
  in newPos === pos

-- | Property: advancePos should handle single character correctly
prop_advance_pos_single_char :: SourcePos -> Char -> Property
prop_advance_pos_single_char pos char =
  let newPos = advancePos pos [char]
  in if char == '\n'
     then posLine newPos === posLine pos + 1 .&&. posColumn newPos === 1
     else posLine newPos === posLine pos .&&. posColumn newPos === posColumn pos + 1

-- | Property: advancePosBy should handle empty text correctly
prop_advance_pos_by_empty :: SourcePos -> Property
prop_advance_pos_by_empty pos =
  let newPos = advancePosBy pos ""
  in newPos === pos

-- | Property: advancePosBy should count newlines correctly
prop_advance_pos_by_newlines :: SourcePos -> Int -> Property
prop_advance_pos_by_newlines pos count =
  let validCount = count >= 0 && count <= 100
      text = replicate count '\n'
      newPos = advancePosBy pos text
  in validCount ==> posLine newPos === posLine pos + count .&&. posColumn newPos === 1

-- ============================================================================
-- Source Span Property Tests
-- ============================================================================

-- | Property: emptySpan should have start and end at same position
prop_empty_span_properties :: Property
prop_empty_span_properties =
  let span = emptySpan
  in spanStart span === spanEnd span

-- | Property: spanFrom should create span from position to same position
prop_span_from_identity :: SourcePos -> Property
prop_span_from_identity pos =
  let span = spanFrom pos
  in spanStart span === pos .&&. spanEnd span === pos

-- | Property: spanTo should create span from startPos to given position
prop_span_to_properties :: SourcePos -> Property
prop_span_to_properties pos =
  let span = spanTo pos
  in spanStart span === startPos .&&. spanEnd span === pos

-- | Property: spanBetween should create span with correct ordering
prop_span_between_ordering :: SourcePos -> SourcePos -> Property
prop_span_between_ordering pos1 pos2 =
  let span = spanBetween pos1 pos2
      start = spanStart span
      end = spanEnd span
  in (start <= end) === True

-- | Property: mergeSpans should contain both original spans
prop_merge_spans_containment :: SourceSpan -> SourceSpan -> Property
prop_merge_spans_containment span1 span2 =
  let merged = mergeSpans span1 span2
      start1 = spanStart span1
      end1 = spanEnd span1
      start2 = spanStart span2
      end2 = spanEnd span2
      mergedStart = spanStart merged
      mergedEnd = spanEnd merged
  in mergedStart <= start1 .&&. mergedEnd >= end1 .&&.
     mergedStart <= start2 .&&. mergedEnd >= end2

-- | Property: mergeSpans should be commutative
prop_merge_spans_commutative :: SourceSpan -> SourceSpan -> Property
prop_merge_spans_commutative span1 span2 =
  let merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
  in merged1 === merged2

-- | Property: mergeSpans should be associative
prop_merge_spans_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_merge_spans_associative span1 span2 span3 =
  let merged1 = mergeSpans (mergeSpans span1 span2) span3
      merged2 = mergeSpans span1 (mergeSpans span2 span3)
  in merged1 === merged2

-- | Property: isValidSpan should return true for properly ordered spans
prop_is_valid_span_ordered :: SourceSpan -> Property
prop_is_valid_span_ordered span =
  let start = spanStart span
      end = spanEnd span
  in (start <= end) ==> isValidSpan span === True

-- ============================================================================
-- Located Value Property Tests
-- ============================================================================

-- | Property: locatedAt should create located value with span from position
prop_located_at_properties :: SourcePos -> Int -> Property
prop_located_at_properties pos value =
  let located = locatedAt pos value
  in spanStart (locatedSpan located) === pos .&&. 
     spanEnd (locatedSpan located) === pos .&&.
     locatedValue located === value

-- | Property: locatedWithSpan should preserve span and value
prop_located_with_span :: SourceSpan -> String -> Property
prop_located_with_span span value =
  let located = locatedWithSpan span value
  in locatedSpan located === span .&&. locatedValue located === value

-- | Property: locatedPos should return start position of span
prop_located_pos :: SourceSpan -> String -> Property
prop_located_pos span value =
  let located = locatedWithSpan span value
  in locatedPos located === spanStart span

-- | Property: mapLocated should preserve span but transform value
prop_maplocated_preserve_span :: SourceSpan -> Int -> Property
prop_maplocated_preserve_span span value =
  let located = locatedWithSpan span value
      mapped = mapLocated (+1) located
  in locatedSpan mapped === span .&&. locatedValue mapped === value + 1

-- | Property: mapLocated should be composable
prop_maplocated_composable :: SourceSpan -> Int -> Property
prop_maplocated_composable span value =
  let located = locatedWithSpan span value
      mapped1 = mapLocated (*2) $ mapLocated (+1) located
      mapped2 = mapLocated ((*2) . (+1)) located
  in mapped1 === mapped2

-- ============================================================================
-- Error Location Property Tests
-- ============================================================================

-- | Property: toErrorLocation should handle valid positions
prop_to_error_location :: SourcePos -> Property
prop_to_error_location pos =
  let result = toErrorLocation pos
  in not (null result)  -- Should produce non-empty location string

-- | Property: toErrorLocationWithSpan should handle valid spans
prop_to_error_location_with_span :: SourceSpan -> Property
prop_to_error_location_with_span span =
  let result = toErrorLocationWithSpan span
  in not (null result)  -- Should produce non-empty location string

-- | Property: Error location strings should contain line information
prop_error_location_contains_line :: SourcePos -> Property
prop_error_location_contains_line pos =
  let result = toErrorLocation pos
      lineStr = show (posLine pos)
  in lineStr `isInfixOf` result

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "New Cabal SourceLocation QuickCheck Tests"
  [ -- Source Position Tests
    fastProperty "start pos properties" prop_start_pos_properties
  , fastProperty "pos after same line" prop_pos_after_same_line
  , fastProperty "pos at creation" prop_pos_at_creation
  , fastProperty "pos at line col" prop_pos_at_line_col
  , fastProperty "advance pos empty" prop_advance_pos_empty
  , fastProperty "advance pos single char" prop_advance_pos_single_char
  , fastProperty "advance pos by empty" prop_advance_pos_by_empty
  , fastProperty "advance pos by newlines" prop_advance_pos_by_newlines
  
  -- Source Span Tests
  , fastProperty "empty span properties" prop_empty_span_properties
  , fastProperty "span from identity" prop_span_from_identity
  , fastProperty "span to properties" prop_span_to_properties
  , fastProperty "span between ordering" prop_span_between_ordering
  , fastProperty "merge spans containment" prop_merge_spans_containment
  , fastProperty "merge spans commutative" prop_merge_spans_commutative
  , fastProperty "merge spans associative" prop_merge_spans_associative
  , fastProperty "is valid span ordered" prop_is_valid_span_ordered
  
  -- Located Value Tests
  , fastProperty "located at properties" prop_located_at_properties
  , fastProperty "located with span" prop_located_with_span
  , fastProperty "located pos" prop_located_pos
  , fastProperty "maplocated preserve span" prop_maplocated_preserve_span
  , fastProperty "maplocated composable" prop_maplocated_composable
  
  -- Error Location Tests
  , fastProperty "to error location" prop_to_error_location
  , fastProperty "to error location with span" prop_to_error_location_with_span
  , fastProperty "error location contains line" prop_error_location_contains_line
  ]

-- Helper function for infix notation
(.&&.) :: Property -> Property -> Property
(.&&.) = (Test.Tasty.QuickCheck..&&.)
