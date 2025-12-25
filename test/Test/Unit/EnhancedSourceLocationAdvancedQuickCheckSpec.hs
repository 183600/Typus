{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Test.Unit.EnhancedSourceLocationAdvancedQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), (.&&.), counterexample, forAll, oneof, elements, listOf, sized, choose, Positive(..))
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)
import SourceLocation
  ( SourcePos(..), SourceSpan(..), Located(..)
  , startPos, posAfter, posAt, posAtLineCol
  , emptySpan, spanFrom, spanTo, spanBetween, mergeSpans, isValidSpan
  , locatedAt, locatedWithSpan, locatedValue, locatedSpan, locatedPos, mapLocated
  , advancePos, advancePosBy, advancePosByText, advancePosByLine
  , toErrorLocation, toErrorLocationWithSpan
  )
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isSpace)
import Compiler.Errors.Core (ErrorLocation(..))

-- ============================================================================
-- SourceLocation Advanced QuickCheck Tests
-- ============================================================================

tests :: TestTree
tests = testGroup "SourceLocation Advanced QuickCheck Tests"
  [ testProperty "SourcePos ordering is consistent with offset" prop_pos_ordering_consistent
  , testProperty "posAfter updates line and column correctly" prop_pos_after_updates_correctly
  , testProperty "spanBetween always creates valid span" prop_span_between_valid
  , testProperty "mergeSpans is associative" prop_merge_spans_associative
  , testProperty "mergeSpans is commutative" prop_merge_spans_commutative
  , testProperty "locatedWithSpan preserves span information" prop_located_with_span_preserves
  , testProperty "mapLocated preserves location" prop_map_located_preserves
  , testProperty "advancePosBy is consistent with repeated posAfter" prop_advance_pos_by_consistent
  , testProperty "advancePosByText handles Unicode correctly" prop_advance_pos_by_text_unicode
  , testProperty "toErrorLocationWithSpan preserves range information" prop_to_error_location_with_span_preserves
  , testProperty "spanFrom and spanTo create single-point spans" prop_span_from_to_single_point
  , testProperty "posAtLineCol creates valid positions" prop_pos_at_line_col_valid
  , testCase "SourceLocation handles edge cases" test_source_location_edge_cases
  , testCase "Located values maintain invariants" test_located_invariants
  , testCase "Span merging handles edge cases" test_span_merging_edge_cases
  ]

-- ============================================================================
-- SourcePos Properties
-- ============================================================================

prop_pos_ordering_consistent :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> Property
prop_pos_ordering_consistent (Positive line1) (Positive col1) (Positive line2) (Positive col2) =
  let pos1 = posAtLineCol line1 col1 0
      pos2 = posAtLineCol line2 col2 0
  in if line1 == line2
     then col1 <= col2 ==> pos1 <= pos2
     else line1 <= line2 ==> pos1 <= pos2

prop_pos_after_updates_correctly :: SourcePos -> Char -> Property
prop_pos_after_updates_correctly pos c =
  let newPos = posAfter c pos
      expectedLine = if c == '\n' then posLine pos + 1 else posLine pos
      expectedCol = case c of
        '\n' -> 1
        '\t' -> ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
        _ -> posColumn pos + 1
      expectedOffset = posOffset pos + 1
  in counterexample ("Char: " ++ show c ++ ", Old pos: " ++ show pos ++ ", New pos: " ++ show newPos) $
     posLine newPos === expectedLine .&&.
     posColumn newPos === expectedCol .&&.
     posOffset newPos === expectedOffset

-- ============================================================================
-- SourceSpan Properties
-- ============================================================================

prop_span_between_valid :: SourcePos -> SourcePos -> Property
prop_span_between_valid pos1 pos2 =
  let start = min pos1 pos2
      end = max pos1 pos2
      span = spanBetween start end
  in isValidSpan span === True

prop_merge_spans_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_merge_spans_associative span1 span2 span3 =
  let left = mergeSpans (mergeSpans span1 span2) span3
      right = mergeSpans span1 (mergeSpans span2 span3)
  in left === right

prop_merge_spans_commutative :: SourceSpan -> SourceSpan -> Property
prop_merge_spans_commutative span1 span2 =
  let left = mergeSpans span1 span2
      right = mergeSpans span2 span1
  in left === right

prop_span_from_to_single_point :: SourcePos -> Property
prop_span_from_to_single_point pos =
  let spanFromPos = spanFrom pos
      spanToPos = spanTo pos
  in spanFromPos === spanToPos .&&. isValidSpan spanFromPos

-- ============================================================================
-- Located Properties
-- ============================================================================

prop_located_with_span_preserves :: SourceSpan -> String -> Property
prop_located_with_span_preserves span value =
  let located = locatedWithSpan span value
  in locatedSpan located === span .&&. locatedValue located === value

prop_map_located_preserves :: SourceSpan -> String -> Property
prop_map_located_preserves span value =
  let located = locatedWithSpan span value
      mapped = mapLocated reverse located
  in locatedSpan mapped === span .&&. locatedValue mapped === reverse value

-- ============================================================================
-- Position Advancement Properties
-- ============================================================================

prop_advance_pos_by_consistent :: SourcePos -> String -> Property
prop_advance_pos_by_consistent pos chars =
  let advanceBy = advancePosBy chars pos
      advanceFold = foldl (flip posAfter) pos chars
  in advanceBy === advanceFold

prop_advance_pos_by_text_unicode :: SourcePos -> Property
prop_advance_pos_by_text_unicode pos =
  forAll genUnicodeString $ \text ->
    let advanceByText = advancePosByText text pos
        advanceByString = advancePosBy (T.unpack text) pos
    in advanceByText === advanceByString

-- ============================================================================
-- Error Location Properties
-- ============================================================================

prop_to_error_location_with_span_preserves :: SourceSpan -> Property
prop_to_error_location_with_span_preserves span =
  let errorLoc = toErrorLocationWithSpan span
      start = spanStart span
      end = spanEnd span
  in line errorLoc === posLine start .&&.
     column errorLoc === posColumn start .&&.
     endLine errorLoc === Just (posLine end) .&&.
     endColumn errorLoc === Just (posColumn end)

-- ============================================================================
-- Position Creation Properties
-- ============================================================================

prop_pos_at_line_col_valid :: Positive Int -> Positive Int -> Property
prop_pos_at_line_col_valid (Positive line) (Positive col) =
  let pos = posAtLineCol line col 0
  in posLine pos === line .&&.
     posColumn pos === col .&&.
     posOffset pos === 0

-- ============================================================================
-- Specific Test Cases
-- ============================================================================

test_source_location_edge_cases :: IO ()
test_source_location_edge_cases = do
  -- Test position advancement with special characters
  let pos = startPos
      posAfterNewline = posAfter '\n' pos
      posAfterTab = posAfter '\t' pos
      posAfterBackspace = posAfter '\b' pos
  assertEqual "Newline should advance line" 2 (posLine posAfterNewline)
  assertEqual "Newline should reset column" 1 (posColumn posAfterNewline)
  assertEqual "Tab should advance to next tab stop" 9 (posColumn posAfterTab)
  assertEqual "Backspace should act as normal character" 2 (posColumn posAfterBackspace)
  
  -- Test empty span validity
  let emptyPosSpan = emptySpan pos
  assertBool "Empty span should be valid" $ isValidSpan emptyPosSpan
  
  -- Test span merging with identical spans
  let span1 = spanBetween pos posAfterNewline
      mergedIdentical = mergeSpans span1 span1
  assertEqual "Merging identical spans should preserve the span" span1 mergedIdentical

test_located_invariants :: IO ()
test_located_invariants = do
  let pos = posAt 1 1
      span = spanFrom pos
      value = "test"
      located = locatedWithSpan span value
  
  -- Test that located values maintain their invariants
  assertEqual "Located value should match original" value (locatedValue located)
  assertEqual "Located span should match original" span (locatedSpan located)
  assertEqual "Located position should match span start" pos (locatedPos located)
  
  -- Test mapping preserves location
  let mapped = mapLocated (++ " modified") located
  assertEqual "Mapped value should be modified" (value ++ " modified") (locatedValue mapped)
  assertEqual "Mapped location should be preserved" span (locatedSpan mapped)

test_span_merging_edge_cases :: IO ()
test_span_merging_edge_cases = do
  let pos1 = posAt 1 1
      pos2 = posAt 1 10
      pos3 = posAt 2 5
      span1 = spanBetween pos1 pos2
      span2 = spanBetween pos2 pos3
      span3 = spanBetween pos1 pos3
  
  -- Test merging adjacent spans
  let mergedAdjacent = mergeSpans span1 span2
  assertEqual "Merging adjacent spans should create covering span" span3 mergedAdjacent
  
  -- Test merging overlapping spans
  let overlappingSpan1 = spanBetween pos1 (posAt 1 15)
      overlappingSpan2 = spanBetween (posAt 1 8) pos3
      expectedMerged = spanBetween pos1 pos3
      actualMerged = mergeSpans overlappingSpan1 overlappingSpan2
  assertEqual "Merging overlapping spans should create covering span" expectedMerged actualMerged

-- ============================================================================
-- Helper Generators
-- ============================================================================

instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 1000)
    col <- choose (1, 1000)
    offset <- choose (0, 100000)
    return $ SourcePos line col offset

instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    endOffset <- choose (0, 100)
    let end = start { posOffset = posOffset start + endOffset }
    return $ spanBetween start end

genUnicodeString :: Gen Text
genUnicodeString = sized $ \n -> do
  let basicChars = [' '..'~']  -- ASCII printable characters
      unicodeChars = basicChars ++ ['\x80'..'\xFF'] ++ ['\x100'..'\x24F']  -- Extended Unicode
  k <- choose (0, n)
  chars <- listOf1 (elements unicodeChars)
  return $ T.pack chars

-- Helper function to generate strings with specific properties
genStringWithNewlines :: Gen String
genStringWithNewlines = do
  parts <- listOf $ oneof
    [ listOf1 (elements ['a'..'z'])
    , return "\n"
    , return "\t"
    , return " "
    ]
  return $ concat parts

genStringWithTabs :: Gen String
genStringWithTabs = do
  parts <- listOf $ oneof
    [ listOf1 (elements ['a'..'z'])
    , return "\t"
    , return " "
    ]
  return $ concat parts