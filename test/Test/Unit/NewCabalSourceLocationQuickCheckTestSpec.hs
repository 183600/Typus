module Test.Unit.NewCabalSourceLocationQuickCheckTestSpec where
import Test.QuickCheck 
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), (.&&.), (.||.), )
  ( SourcePos(..), SourceSpan(..), Located(..)
  , startPos, posAfter, posAt, posAtLineCol
  , emptySpan, spanFrom, spanTo, spanBetween, mergeSpans, isValidSpan
  , locatedAt, locatedWithSpan, locatedValue, locatedSpan, locatedPos, mapLocated
  , runLocationTracker, getCurrentPos, setCurrentPos, markSpanStart, markSpanEnd
  , toErrorLocation, toErrorLocationWithSpan
  , advancePos, advancePosBy, advancePosByText, advancePosByLine
  )
import Data.Text 
              line <- choose (1, 1000)
    column <- choose (1, 1000)
    offset <- choose (0, 1000000)
    return $ SourcePos line column offset
-- Arbitrary instance for SourcePos
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 100)
    column <- choose (1, 100)
    offset <- choose (0, 1000)
    return $ SourcePos line column offset

-- Arbitrary instance for SourceSpan
instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    return $ SourceSpan start end


instance Arbitrary SourceSpan where
                                              arbitrary = do
              start <- arbitrary
    end <- arbitrary
    -- Ensure we have a valid span by sorting positions
    let (realStart, realEnd) = if start <= end then (start, end) else (end, start)
    return $ SourceSpan realStart realEnd

instance Arbitrary                               a => Arbitrary (Located a) where
                                              arbitrary = do
              value <- arbitrary
    pos <- arbitrary
    span <- arbitrary
    return $ Located value pos span

-- Generate valid spans where start <= end
genValidSpan :: Gen SourceSpan
                              genValidSpan = do
              startLine <- choose (1, 100)
  startCol <- choose (1, 100)
  startOffset <- choose (0, 10000)
  
  endLine <- choose (startLine, startLine + 10)
  endCol <- if                               endLine == startLine 
            then choose (startCol, startCol + 100)
            else choose (1, 100)
  endOffset <- choose (startOffset, startOffset + 1000)
  
  return $ SourceSpan (SourcePos startLine startCol startOffset) 
                     (SourcePos endLine endCol endOffset)

-- Generate text with various characters
genText :: Gen Text
                              genText = T.pack <$> listOf (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n")

-- ============================================================================
-- SourcePos QuickCheck Tests
-- ============================================================================

-- Test startPos properties
prop_startPos_valid :: Property
                              prop_startPos_valid = 
  posLine                               startPos === 1 .&&.
  posColumn                               startPos === 1 .&&.
  posOffset                               startPos === 0

-- Test posAfter function properties
prop_posAfter_newline_increments_line :: SourcePos -> Property
prop_posAfter_newline_increments_line                               pos =
  let newPos = posAfter '\n' pos
  in posLine                               newPos === posLine pos + 1 .&&.
     posColumn                               newPos === 1 .&&.
     posOffset                               newPos === posOffset pos + 1

prop_posAfter_tab_aligns_to_8 :: SourcePos -> Property
prop_posAfter_tab_aligns_to_8                               pos =
  let newPos = posAfter '\t' pos
                              expectedCol = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in posColumn                               newPos === expectedCol .&&.
     posOffset                               newPos === posOffset pos + 1

prop_posAfter_regular_char_increments_column :: SourcePos -> Char -> Property
prop_posAfter_regular_char_increments_column pos                               c =
  c /= '\n' && c /= '\t' ==>
  let newPos = posAfter c pos
  in posColumn                               newPos === posColumn pos + 1 .&&.
     posOffset                               newPos === posOffset pos + 1

-- Test posAt function properties
prop_posAt_creates_correct_position :: Int -> Int -> Property
prop_posAt_creates_correct_position line                               col =
  line > 0 && col >                               0 ==>
  let pos = posAt line col
  in posLine                               pos === line .&&.
     posColumn                               pos === col .&&.
     posOffset                               pos === 0

prop_posAtLineCol_creates_correct_position :: Int -> Int -> Int -> Property
prop_posAtLineCol_creates_correct_position line col                               offset =
  line > 0 && col > 0 && offset >=                               0 ==>
  let pos = posAtLineCol line col offset
  in posLine                               pos === line .&&.
     posColumn                               pos === col .&&.
     posOffset                               pos === offset

-- ============================================================================
-- SourceSpan QuickCheck Tests
-- ============================================================================

-- Test emptySpan function properties
prop_emptySpan_start_equals_end :: SourcePos -> Property
prop_emptySpan_start_equals_end                               pos =
  let span = emptySpan pos
  in spanStart                               span === pos .&&. spanEnd                               span === pos
prop_emptySpan_is_valid :: SourcePos -> Property
prop_emptySpan_is_valid                               pos = isValidSpan (emptySpan pos)

-- Test spanFrom function properties
prop_spanFrom_equals_emptySpan :: SourcePos -> Property
prop_spanFrom_equals_emptySpan                               pos = spanFrom                               pos === emptySpan pos

-- Test spanTo function properties
prop_spanTo_start_equals_end :: SourcePos -> Property
prop_spanTo_start_equals_end                               pos =
  let span = spanTo pos
  in spanStart                               span === pos .&&. spanEnd                               span === pos

-- Test spanBetween function properties
prop_spanBetween_orders_positions :: SourcePos -> SourcePos -> Property
prop_spanBetween_orders_positions pos1                               pos2 =
  let span = spanBetween pos1 pos2
  in spanStart span <= spanEnd span

-- Test mergeSpans function properties
prop_mergeSpans_contains_both_spans :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_contains_both_spans span1                               span2 =
  let merged = mergeSpans span1 span2
  in spanStart merged <= spanStart span1 .&&.
     spanEnd merged >= spanEnd span1 .&&.
     spanStart merged <= spanStart span2 .&&.
     spanEnd merged >= spanEnd span2

prop_mergeSpans_commutative :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_commutative span1                               span2 = 
  mergeSpans span1                               span2 === mergeSpans span2 span1

-- Test isValidSpan function properties
prop_isValidSpan_valid_span :: Property
                              prop_isValidSpan_valid_span = forAll genValidSpan isValidSpan

prop_isValidSpan_invalid_span :: SourcePos -> SourcePos -> Property
prop_isValidSpan_invalid_span pos1                               pos2 =
  pos1 >                               pos2 ==> not (isValidSpan (SourceSpan pos1 pos2)

-- ============================================================================
-- Located QuickCheck Tests
-- ============================================================================

-- Test locatedAt function properties
prop_locatedAt_span_is_empty :: SourcePos -> Int -> Property
prop_locatedAt_span_is_empty pos                               value =
  let located = locatedAt pos value
  in locatedSpan                               located === emptySpan pos .&&.
     locatedPos                               located === pos

prop_locatedAt_preserves_value :: SourcePos -> Int -> Property
prop_locatedAt_preserves_value pos                               value =
  locatedValue (locatedAt pos value) === value

-- Test locatedWithSpan function properties
prop_locatedWithSpan_preserves_span :: SourceSpan -> Int -> Property
prop_locatedWithSpan_preserves_span span                               value =
  locatedSpan (locatedWithSpan span value) === span

prop_locatedWithSpan_preserves_value :: SourceSpan -> Int -> Property
prop_locatedWithSpan_preserves_value span                               value =
  locatedValue (locatedWithSpan span value) === value

-- Test mapLocated function properties
prop_mapLocated_preserves_location :: SourceSpan -> Int -> Property
prop_mapLocated_preserves_location span                               value =
  let located = locatedWithSpan span value
                                    mapped = mapLocated (+1) located
  in locatedSpan                               mapped === locatedSpan located .&&.
     locatedPos                               mapped === locatedPos located

prop_mapLocated_applies_function :: SourceSpan -> Int -> Property
prop_mapLocated_applies_function span                               value =
  locatedValue (mapLocated (*2) (locatedWithSpan span value) === value * 2

-- ============================================================================
-- LocationTracker QuickCheck Tests
-- ============================================================================

-- Test runLocationTracker function properties
prop_runLocationTracker_starts_at_startPos :: Property
                              prop_runLocationTracker_starts_at_startPos =
  runLocationTracker                               getCurrentPos === startPos

-- Test setCurrentPos L.and getCurrentPos properties
prop_setCurrentPos_getCurrentPos :: SourcePos -> Property
prop_setCurrentPos_getCurrentPos                               pos =
  runLocationTracker (setCurrentPos pos >> getCurrentPos) === pos

-- Test markSpanStart L.and markSpanEnd properties
prop_markSpan_creates_valid_span :: SourcePos -> Property
prop_markSpan_creates_valid_span                               pos =
  let (span, _) = withLocationTracking pos $ do
              start <- markSpanStart
        setCurrentPos (posAfter 'x' start)
        markSpanEnd start
  in isValidSpan span

-- ============================================================================
-- Position Advancement QuickCheck Tests
-- ============================================================================

-- Test advancePos function properties
prop_advancePos_equals_posAfter :: Char -> SourcePos -> Property
prop_advancePos_equals_posAfter c                               pos = advancePos c                               pos === posAfter c pos

-- Test advancePosBy function properties
prop_advancePosBy_empty_string :: SourcePos -> Property
prop_advancePosBy_empty_string                               pos = advancePosBy ""                               pos === pos

prop_advancePosBy_consistent_with_posAfter :: String -> SourcePos -> Property
prop_advancePosBy_consistent_with_posAfter chars                               pos =
  advancePosBy chars                               pos === L.foldl (flip posAfter) pos chars

-- Test advancePosByText function properties
prop_advancePosByText_equals_advancePosBy :: Text -> SourcePos -> Property
prop_advancePosByText_equals_advancePosBy text                               pos =
  advancePosByText text                               pos === advancePosBy (T.unpack text) pos

-- Test advancePosByLine function properties
prop_advancePosByLine_increments_line :: Int -> SourcePos -> Property
prop_advancePosByLine_increments_line n                               pos =
  n >                               0 ==>
  let newPos = advancePosByLine n pos
  in posLine                               newPos === posLine pos + n .&&.
     posColumn                               newPos === 1

-- ============================================================================
-- Error Location Conversion QuickCheck Tests
-- ============================================================================

-- Test toErrorLocation function properties
prop_toErrorLocation_preserves_line_column :: SourcePos -> Property
prop_toErrorLocation_preserves_line_column                               pos =
  let errLoc = toErrorLocation pos
  in line                               errLoc === posLine pos .&&.
     column                               errLoc === posColumn pos .&&.
     filePath                               errLoc === Nothing .&&.
     endLine                               errLoc === Nothing .&&.
     endColumn                               errLoc === Nothing

-- Test toErrorLocationWithSpan function properties
prop_toErrorLocationWithSpan_preserves_span :: SourceSpan -> Property
prop_toErrorLocationWithSpan_preserves_span                               span =
  let errLoc = toErrorLocationWithSpan span
                                    start = spanStart span
                                    end = spanEnd span
  in line                               errLoc === posLine start .&&.
     column                               errLoc === posColumn start .&&.
     endLine                               errLoc === Just (posLine end) .&&.
     endColumn                               errLoc === Just (posColumn end) .&&.
     filePath                               errLoc === Nothing

-- ============================================================================
-- Additional Property Tests
-- ============================================================================

-- Test position ordering
prop_position_ordering_consistent_with_offset :: SourcePos -> SourcePos -> Property
prop_position_ordering_consistent_with_offset pos1                               pos2 =
  (pos1 <= pos2) === (posOffset pos1 <= posOffset pos2)

-- Test span merging idempotency
prop_merge_spans_idempotent :: SourceSpan -> Property
prop_merge_spans_idempotent                               span = mergeSpans span                               span === span

-- Test located value extraction
prop_located_value_roundtrip :: SourceSpan -> String -> Property
prop_located_value_roundtrip span                               value =
  locatedValue (locatedWithSpan span value) === value

-- Test span coverage
prop_span_between_covers_both_positions :: SourcePos -> SourcePos -> Property
prop_span_between_covers_both_positions pos1                               pos2 =
  let span = spanBetween pos1 pos2
  in property $ (spanStart span <= pos1 && pos1 <= spanEnd span) .&&.
     (spanStart span <= pos2 && pos2 <= spanEnd span)

tests :: TestTree
tests =   testGroup "New Cabal SourceLocation QuickCheck Tests"
  [ testGroup "SourcePos tests"
      [             testProperty "startPos has correct values" prop_startPos_valid
      ,             testProperty "posAfter increments line for newline" prop_posAfter_newline_increments_line
      ,             testProperty "posAfter aligns tab to 8 columns" prop_posAfter_tab_aligns_to_8
      ,             testProperty "posAfter increments column for regular chars" prop_posAfter_regular_char_increments_column
      ,             testProperty "posAt creates correct position" prop_posAt_creates_correct_position
      ,             testProperty "posAtLineCol creates correct position" prop_posAtLineCol_creates_correct_position
      ]
  , testGroup "SourceSpan tests"
      [             testProperty "emptySpan has start equals end" prop_emptySpan_start_equals_end
      ,             testProperty "emptySpan is valid" prop_emptySpan_is_valid
      ,             testProperty "spanFrom equals emptySpan" prop_spanFrom_equals_emptySpan
      ,             testProperty "spanTo has start equals end" prop_spanTo_start_equals_end
      ,             testProperty "spanBetween orders positions" prop_spanBetween_orders_positions
      ,             testProperty "mergeSpans contains both spans" prop_mergeSpans_contains_both_spans
      ,             testProperty "mergeSpans is commutative" prop_mergeSpans_commutative
      ,             testProperty "isValidSpan validates valid spans" prop_isValidSpan_valid_span
      ,             testProperty "isValidSpan rejects invalid spans" prop_isValidSpan_invalid_span
      ]
  , testGroup "Located tests"
      [             testProperty "locatedAt creates empty span" prop_locatedAt_span_is_empty
      ,             testProperty "locatedAt preserves value" prop_locatedAt_preserves_value
      ,             testProperty "locatedWithSpan preserves span" prop_locatedWithSpan_preserves_span
      ,             testProperty "locatedWithSpan preserves value" prop_locatedWithSpan_preserves_value
      ,             testProperty "mapLocated preserves location" prop_mapLocated_preserves_location
      ,             testProperty "mapLocated applies function" prop_mapLocated_applies_function
      ]
  , testGroup "LocationTracker tests"
      [             testProperty "runLocationTracker starts at startPos" prop_runLocationTracker_starts_at_startPos
      ,             testProperty "setCurrentPos/getCurrentPos roundtrip" prop_setCurrentPos_getCurrentPos
      ,             testProperty "markSpan creates valid span" prop_markSpan_creates_valid_span
      ]
  , testGroup "Position advancement tests"
      [             testProperty "advancePos equals posAfter" prop_advancePos_equals_posAfter
      ,             testProperty "advancePosBy empty string" prop_advancePosBy_empty_string
      ,             testProperty "advancePosBy consistent with posAfter" prop_advancePosBy_consistent_with_posAfter
      ,             testProperty "advancePosByText equals advancePosBy" prop_advancePosByText_equals_advancePosBy
      ,             testProperty "advancePosByLine increments line" prop_advancePosByLine_increments_line
      ]
  , testGroup "Error location conversion tests"
      [             testProperty "toErrorLocation preserves line L.and column" prop_toErrorLocation_preserves_line_column
      ,             testProperty "toErrorLocationWithSpan preserves span" prop_toErrorLocationWithSpan_preserves_span
      ]
  , testGroup "Additional property tests"
      [             testProperty "position ordering consistent with offset" prop_position_ordering_consistent_with_offset
      ,             testProperty "merge spans idempotent" prop_merge_spans_idempotent
      ,             testProperty "located value roundtrip" prop_located_value_roundtrip
      ,             testProperty "span between covers both positions" prop_span_between_covers_both_positions
      ]
  ]