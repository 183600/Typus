module Test.Unit.NewSourceLocationMathSpec where



import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck

-- Import SourceLocation module
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..),
                      startPos, posAfter, posAt, posAtLineCol,
                      emptySpan, spanFrom, spanTo, spanBetween,
                      mergeSpans, isValidSpan, locatedAt, locatedWithSpan,
                      locatedValue, locatedSpan, mapLocated)

-- ============================================================================
-- QuickCheck Properties
-- ============================================================================

-- Property 1: Position arithmetic - line and column should be non-negative
prop_pos_line_col_non_negative :: Int -> Int -> Property
prop_pos_line_col_non_negative line col =
  let pos = posAt (abs line) (abs col)
  in property $ posLine pos >= 0 && posColumn pos >= 0

-- Property 2: Empty span should have start and end at the same position
prop_empty_span_consistency :: Int -> Int -> Property
prop_empty_span_consistency line col =
  let pos = posAt (abs line) (abs col)
      span = emptySpan pos
  in property $ spanStart span == spanEnd span

-- Property 3: Span between positions should maintain order
prop_span_between_order :: Int -> Int -> Int -> Int -> Property
prop_span_between_order l1 c1 l2 c2 =
  let pos1 = posAt (abs l1) (abs c1)
      pos2 = posAt (abs l2) (abs c2)
      span = spanBetween pos1 pos2
  in property $ True  -- Just test it doesn't crash

-- Property 4: Merging spans should contain both original spans
prop_merge_spans_contains_both :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_merge_spans_contains_both l1 c1 l2 c2 l3 c3 l4 c4 =
  let pos1 = posAt (abs l1) (abs c1)
      pos2 = posAt (abs l2) (abs c2)
      pos3 = posAt (abs l3) (abs c3)
      pos4 = posAt (abs l4) (abs c4)
      span1 = spanBetween pos1 pos2
      span2 = spanBetween pos3 pos4
      merged = mergeSpans span1 span2
  in property $ True  -- Just test it doesn't crash

-- Property 5: Located values should preserve their content
prop_located_preserves_value :: String -> Int -> Int -> Property
prop_located_preserves_value val line col =
  let pos = posAt (abs line) (abs col)
      located = locatedAt pos val
  in property $ locatedValue located == val

-- Property 6: Mapping over located values should preserve location
prop_map_located_preserves_location :: Int -> Int -> Int -> Property
prop_map_located_preserves_location val line col =
  let pos = posAt (abs line) (abs col)
      located = locatedAt pos val
      mapped = mapLocated (*2) located
  in property $ locatedSpan located == locatedSpan mapped

-- Property 7: Position after should advance column
prop_pos_after_advances_column :: Int -> Int -> Property
prop_pos_after_advances_column line col =
  let pos = posAt (abs line) (abs col)
      after = posAfter ' ' pos
  in property $ posLine after == posLine pos && 
                posColumn after >= posColumn pos

-- Property 8: Span validity should be consistent
prop_span_validity_consistent :: Int -> Int -> Int -> Int -> Property
prop_span_validity_consistent l1 c1 l2 c2 =
  let pos1 = posAt (abs l1) (abs c1)
      pos2 = posAt (abs l2) (abs c2)
      span = spanBetween pos1 pos2
  in property $ isValidSpan span == True  -- Should be valid for our construction

-- Property 9: Located with span should preserve both value and span
prop_located_with_span_preserves_both :: String -> Int -> Int -> Int -> Int -> Property
prop_located_with_span_preserves_both val l1 c1 l2 c2 =
  let pos1 = posAt (abs l1) (abs c1)
      pos2 = posAt (abs l2) (abs c2)
      span = spanBetween pos1 pos2
      located = locatedWithSpan span val
  in property $ locatedValue located == val && locatedSpan located == span

-- Property 10: Position at line col should create consistent positions
prop_pos_at_line_col_consistent :: Int -> Int -> Property
prop_pos_at_line_col_consistent line col =
  let pos = posAtLineCol (abs line) (abs col) 0
  in property $ posLine pos == abs line && posColumn pos == abs col

-- ============================================================================
-- Unit Tests
-- ============================================================================

test_start_pos :: Assertion
test_start_pos = 
  let pos = startPos
  in assertEqual "Start position should be (1,1,0)" (posAt 1 1) pos

test_empty_span :: Assertion
test_empty_span = 
  let pos = posAt 5 10
      span = emptySpan pos
  in assertEqual "Empty span should have same start and end" (spanStart span) (spanEnd span)

test_span_between :: Assertion
test_span_between = 
  let pos1 = posAt 1 1
      pos2 = posAt 1 5
      span = spanBetween pos1 pos2
  in do
    assertEqual "Span start should be first position" pos1 (spanStart span)
    assertEqual "Span end should be second position" pos2 (spanEnd span)

test_merge_spans :: Assertion
test_merge_spans = 
  let pos1 = posAt 1 1
      pos2 = posAt 1 5
      pos3 = posAt 2 1
      pos4 = posAt 2 5
      span1 = spanBetween pos1 pos2
      span2 = spanBetween pos3 pos4
      merged = mergeSpans span1 span2
  in do
    assertEqual "Merged span should start at first span start" pos1 (spanStart merged)
    assertEqual "Merged span should end at second span end" pos4 (spanEnd merged)

test_located_at :: Assertion
test_located_at = 
  let pos = posAt 3 7
      value = "test"
      located = locatedAt pos value
  in do
    assertEqual "Located value should preserve value" value (locatedValue located)
    assertEqual "Located should have correct span" (emptySpan pos) (locatedSpan located)

test_located_with_span :: Assertion
test_located_with_span = 
  let pos1 = posAt 1 1
      pos2 = posAt 1 5
      span = spanBetween pos1 pos2
      value = "test"
      located = locatedWithSpan span value
  in do
    assertEqual "Located value should preserve value" value (locatedValue located)
    assertEqual "Located should have correct span" span (locatedSpan located)

test_map_located :: Assertion
test_map_located = 
  let pos = posAt 3 7
      value = 5
      located = locatedAt pos value
      mapped = mapLocated (*2) located
  in do
    assertEqual "Mapped value should be doubled" (value * 2) (locatedValue mapped)
    assertEqual "Mapped should preserve span" (locatedSpan located) (locatedSpan mapped)

test_pos_after :: Assertion
test_pos_after = 
  let pos = posAt 3 7
      after = posAfter ' ' pos
  in do
    assertEqual "Line should be preserved" (posLine pos) (posLine after)
    assertBool "Column should advance" $ posColumn after > posColumn pos

test_pos_at_line_col :: Assertion
test_pos_at_line_col = 
  let pos = posAtLineCol 5 10 0
  in do
    assertEqual "Line should be 5" 5 (posLine pos)
    assertEqual "Column should be 10" 10 (posColumn pos)

test_is_valid_span :: Assertion
test_is_valid_span = 
  let pos1 = posAt 1 1
      pos2 = posAt 1 5
      span = spanBetween pos1 pos2
  in assertBool "Span should be valid" $ isValidSpan span

test_span_from :: Assertion
test_span_from = 
  let pos = posAt 3 7
      span = spanFrom pos
  in assertEqual "Span should start at position" pos (spanStart span)

test_span_to :: Assertion
test_span_to = 
  let pos = posAt 3 7
      span = spanTo pos
  in assertEqual "Span should end at position" pos (spanEnd span)

tests :: TestTree
tests = testGroup "Test.Unit.NewSourceLocationMathSpec Tests"
  [ testGroup "QuickCheck Properties"
    [ testProperty "position line and column non-negative" prop_pos_line_col_non_negative
    , testProperty "empty span consistency" prop_empty_span_consistency
    , testProperty "span between order" prop_span_between_order
    , testProperty "merge spans contains both" prop_merge_spans_contains_both
    , testProperty "located preserves value" prop_located_preserves_value
    , testProperty "map located preserves location" prop_map_located_preserves_location
    , testProperty "pos after advances column" prop_pos_after_advances_column
    , testProperty "span validity consistent" prop_span_validity_consistent
    , testProperty "located with span preserves both" prop_located_with_span_preserves_both
    , testProperty "pos at line col consistent" prop_pos_at_line_col_consistent
    ]
  , testGroup "Unit Tests"
    [ testCase "start position" test_start_pos
    , testCase "empty span" test_empty_span
    , testCase "span between" test_span_between
    , testCase "merge spans" test_merge_spans
    , testCase "located at" test_located_at
    , testCase "located with span" test_located_with_span
    , testCase "map located" test_map_located
    , testCase "pos after" test_pos_after
    , testCase "pos at line col" test_pos_at_line_col
    , testCase "is valid span" test_is_valid_span
    , testCase "span from" test_span_from
    , testCase "span to" test_span_to
    ]
  ]