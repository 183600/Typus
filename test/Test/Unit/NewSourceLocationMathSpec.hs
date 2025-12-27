module Test.Unit.NewSourceLocationMathSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Property, (===), Arbitrary(..), Gen, choose, listOf1, elements, forAll, suchThat)

import SourceLocation 
  ( SourcePos(..), SourceSpan(..), Located(..)
  , startPos, posAfter, posAt, posAtLineCol
  , emptySpan, spanFrom, spanTo, spanBetween, mergeSpans, isValidSpan
  , locatedAt, locatedWithSpan, locatedValue, locatedSpan, locatedPos
  , advancePos, advancePosBy, advancePosByText, advancePosByLine
  )
import Data.Text (Text)
import qualified Data.Text as T

-- ============================================================================
-- Test Data Generators
-- ============================================================================

-- Generate valid line numbers (1-1000)
genLineNumber :: Gen Int
genLineNumber = choose (1, 1000)

-- Generate valid column numbers (1-200)
genColumnNumber :: Gen Int
genColumnNumber = choose (1, 200)

-- Generate valid offsets (0-10000)
genOffset :: Gen Int
genOffset = choose (0, 10000)

-- Generate valid source positions
genSourcePos :: Gen SourcePos
genSourcePos = SourcePos <$> genLineNumber <*> genColumnNumber <*> genOffset

-- Generate source positions with valid ordering (start <= end)
genOrderedSourcePosPair :: Gen (SourcePos, SourcePos)
genOrderedSourcePosPair = do
  line1 <- genLineNumber
  line2 <- choose (line1, 1000)
  if line1 == line2
    then do
      col1 <- genColumnNumber
      col2 <- choose (col1, 200)
      off1 <- genOffset
      off2 <- choose (off1, 10000)
      pure (SourcePos line1 col1 off1, SourcePos line2 col2 off2)
    else do
      col1 <- genColumnNumber
      col2 <- genColumnNumber
      off1 <- genOffset
      off2 <- genOffset
      pure (SourcePos line1 col1 off1, SourcePos line2 col2 off2)

-- Generate valid source spans
genSourceSpan :: Gen SourceSpan
genSourceSpan = do
  (start, end) <- genOrderedSourcePosPair
  pure $ SourceSpan start end

-- Generate simple characters for position advancement
genChar :: Gen Char
genChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n.,;:(){}[]"

-- Generate text strings for position advancement
genText :: Gen Text
genText = T.pack <$> listOf1 genChar

-- ============================================================================
-- Property Tests for SourcePos
-- ============================================================================

-- Property: startPos should have line 1, column 1, offset 0
prop_start_pos_properties :: Property
prop_start_pos_properties = 
  let pos = startPos
  in posLine pos === 1 .&&. posColumn pos === 1 .&&. posOffset pos === 0

-- Property: posAfter should correctly handle newline characters
prop_pos_after_newline :: Property
prop_pos_after_newline = 
  forAll genSourcePos $ \pos ->
    let newPos = posAfter '\n' pos
    in posLine newPos === posLine pos + 1 .&&. posColumn newPos === 1

-- Property: posAfter should correctly handle tab characters (8-space alignment)
prop_pos_after_tab :: Property
prop_pos_after_tab = 
  forAll genSourcePos $ \pos ->
    let newPos = posAfter '\t' pos
        expectedCol = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
    in posColumn newPos === expectedCol

-- Property: posAfter should correctly handle regular characters
prop_pos_after_regular_char :: Property
prop_pos_after_regular_char = 
  forAll genSourcePos $ \pos ->
    forAll (elements ['a'..'z']) $ \char ->
      let newPos = posAfter char pos
      in posLine newPos === posLine pos .&&. 
         posColumn newPos === posColumn pos + 1 .&&.
         posOffset newPos === posOffset pos + 1

-- Property: posAt should create positions with correct line and column
prop_pos_at_properties :: Property
prop_pos_at_properties = 
  forAll genLineNumber $ \line ->
    forAll genColumnNumber $ \col ->
      let pos = posAt line col
      in posLine pos === line .&&. posColumn pos === col .&&. posOffset pos === 0

-- ============================================================================
-- Property Tests for SourceSpan
-- ============================================================================

-- Property: emptySpan should have start == end
prop_empty_span_properties :: Property
prop_empty_span_properties = 
  forAll genSourcePos $ \pos ->
    let span = emptySpan pos
    in spanStart span === spanEnd span

-- Property: spanBetween should preserve ordering
prop_span_between_properties :: Property
prop_span_between_properties = 
  forAll genOrderedSourcePosPair $ \(start, end) ->
    let span = spanBetween start end
    in spanStart span === start .&&. spanEnd span === end

-- Property: mergeSpans should produce valid spans
prop_merge_spans_validity :: Property
prop_merge_spans_validity = 
  forAll genSourceSpan $ \span1 ->
    forAll genSourceSpan $ \span2 ->
      let merged = mergeSpans span1 span2
      in isValidSpan merged

-- Property: mergeSpans should be commutative in terms of covered range
prop_merge_spans_commutative :: Property
prop_merge_spans_commutative = 
  forAll genSourceSpan $ \span1 ->
    forAll genSourceSpan $ \span2 ->
      let merged1 = mergeSpans span1 span2
          merged2 = mergeSpans span2 span1
      in spanStart merged1 === spanStart merged2 .&&. 
         spanEnd merged1 === spanEnd merged2

-- Property: mergeSpans should be associative
prop_merge_spans_associative :: Property
prop_merge_spans_associative = 
  forAll genSourceSpan $ \span1 ->
    forAll genSourceSpan $ \span2 ->
      forAll genSourceSpan $ \span3 ->
        let merged12 = mergeSpans span1 span2
            merged23 = mergeSpans span2 span3
            result1 = mergeSpans merged12 span3
            result2 = mergeSpans span1 merged23
        in spanStart result1 === spanStart result2 .&&. 
           spanEnd result1 === spanEnd result2

-- ============================================================================
-- Property Tests for Located Values
-- ============================================================================

-- Property: locatedAt should create values with correct position
prop_located_at_properties :: Property
prop_located_at_properties = 
  forAll genSourcePos $ \pos ->
    forAll (elements [1..100]) $ \value ->
      let located = locatedAt pos value
      in locatedPos located === pos .&&. 
         locatedValue located === value .&&.
         spanStart (locatedSpan located) === pos .&&.
         spanEnd (locatedSpan located) === pos

-- Property: locatedWithSpan should create values with correct span
prop_located_with_span_properties :: Property
prop_located_with_span_properties = 
  forAll genSourceSpan $ \span ->
    forAll (elements [1..100]) $ \value ->
      let located = locatedWithSpan span value
      in locatedSpan located === span .&&.
         locatedValue located === value .&&.
         locatedPos located === spanStart span

-- ============================================================================
-- Property Tests for Position Advancement
-- ============================================================================

-- Property: advancePos should be consistent with posAfter
prop_advance_pos_consistency :: Property
prop_advance_pos_consistency = 
  forAll genSourcePos $ \pos ->
    forAll genChar $ \char ->
      let advanced = advancePos char pos
          expected = posAfter char pos
      in advanced === expected

-- Property: advancing by empty text should not change position
prop_advance_by_empty_text :: Property
prop_advance_by_empty_text = 
  forAll genSourcePos $ \pos ->
    let advanced = advancePosByText T.empty pos
    in advanced === pos

-- Property: advancing by line should increment line count
prop_advance_by_line :: Property
prop_advance_by_line = 
  forAll genSourcePos $ \pos ->
    forAll (choose (1, 10)) $ \numLines ->
      let advanced = advancePosByLine numLines pos
      in posLine advanced === posLine pos + numLines .&&.
         posColumn advanced === posColumn pos

-- ============================================================================
-- Unit Tests
-- ============================================================================

test_source_position_ordering :: IO ()
test_source_position_ordering = do
  let pos1 = SourcePos 1 5 10
      pos2 = SourcePos 2 3 20
      pos3 = SourcePos 1 10 15
  pos1 < pos2 @?= True
  pos1 < pos3 @?= True
  pos3 < pos2 @?= True

test_span_validity :: IO ()
test_span_validity = do
  let validSpan = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
      invalidSpan = SourceSpan (SourcePos 2 1 20) (SourcePos 1 10 9)
  isValidSpan validSpan @?= True
  isValidSpan invalidSpan @?= False

test_located_value_extraction :: IO ()
test_located_value_extraction = do
  let pos = SourcePos 5 10 50
      span = SourceSpan pos (SourcePos 5 15 55)
      located = Located "test" pos span
  locatedValue located @?= "test"
  locatedPos located @?= pos
  locatedSpan located @?= span

test_position_advancement_with_multiline_text :: IO ()
test_position_advancement_with_multiline_text = do
  let startPos' = SourcePos 1 1 0
      text = T.pack "line1\nline2\n"
      finalPos = advancePosByText text startPos'
  posLine finalPos @?= 3
  posColumn finalPos @?= 1

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "New Source Location Math Tests"
  [ -- SourcePos properties
    testProperty "Start position properties" prop_start_pos_properties
  , testProperty "Position after newline" prop_pos_after_newline
  , testProperty "Position after tab" prop_pos_after_tab
  , testProperty "Position after regular character" prop_pos_after_regular_char
  , testProperty "Position at properties" prop_pos_at_properties
  
  -- SourceSpan properties
  , testProperty "Empty span properties" prop_empty_span_properties
  , testProperty "Span between properties" prop_span_between_properties
  , testProperty "Merge spans validity" prop_merge_spans_validity
  , testProperty "Merge spans commutative" prop_merge_spans_commutative
  , testProperty "Merge spans associative" prop_merge_spans_associative
  
  -- Located value properties
  , testProperty "Located at properties" prop_located_at_properties
  , testProperty "Located with span properties" prop_located_with_span_properties
  
  -- Position advancement properties
  , testProperty "Advance position consistency" prop_advance_pos_consistency
  , testProperty "Advance by empty text" prop_advance_by_empty_text
  , testProperty "Advance by line" prop_advance_by_line
  
  -- Unit tests
  , testCase "Source position ordering" test_source_position_ordering
  , testCase "Span validity" test_span_validity
  , testCase "Located value extraction" test_located_value_extraction
  , testCase "Position advancement with multiline text" test_position_advancement_with_multiline_text
  ]