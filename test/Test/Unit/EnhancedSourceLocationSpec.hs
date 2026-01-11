module Test.Unit.EnhancedSourceLocationSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), 
                       spanStart, spanEnd, locatedWithSpan)

-- | Test SourcePos properties
prop_source_pos_ordering :: Int -> Int -> Int -> Int -> Property
prop_source_pos_ordering line1 col1 line2 col2 =
  let pos1 = SourcePos line1 col1
      pos2 = SourcePos line2 col2
  in property $ 
    if line1 < line2 || (line1 == line2 && col1 <= col2)
    then pos1 <= pos2
    else pos1 > pos2

prop_source_pos_equality :: Int -> Int -> Property
prop_source_pos_equality line col = 
  let pos1 = SourcePos line col
      pos2 = SourcePos line col
  in property $ pos1 == pos2

-- | Test SourceSpan properties
prop_span_start_end_consistency :: Int -> Int -> Int -> Int -> Property
prop_span_start_end_consistency startLine startCol endLine endCol =
  let start = SourcePos startLine startCol
      end = SourcePos endLine endCol
      span = SourceSpan start end
  in property $ 
    (spanStart span == start) && (spanEnd span == end)

prop_span_ordering :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_span_ordering sLine1 sCol1 eLine1 eCol1 sLine2 sCol2 eCol2 =
  let start1 = SourcePos sLine1 sCol1
      end1 = SourcePos eLine1 eCol1
      start2 = SourcePos sLine2 sCol2
      end2 = SourcePos (sLine2 + eCol2) (sCol2 + eCol2)  -- Ensure end2 >= start2
      span1 = SourceSpan start1 end1
      span2 = SourceSpan start2 end2
  in property $ 
    (span1 `compare` span2) === (start1 `compare` start2)

-- | Test Located properties
prop_located_value_extraction :: Int -> Int -> Int -> Int -> String -> Property
prop_located_value_extraction line1 col1 line2 col2 value =
  let start = SourcePos line1 col1
      end = SourcePos line2 col2
      located = Located { locatedValue = value, locatedSpan = SourceSpan start end }
  in property $ locatedValue located == value

prop_located_span_consistency :: Int -> Int -> Int -> Int -> String -> Property
prop_located_span_consistency line1 col1 line2 col2 value =
  let start = SourcePos line1 col1
      end = SourcePos line2 col2
      span = SourceSpan start end
      located = locatedWithSpan span value
  in property $ locatedSpan located == span

-- | Test span creation and manipulation
prop_span_creation :: Int -> Int -> Property
prop_span_creation line col =
  let pos = SourcePos line col
      span = SourceSpan pos pos
  in property $ spanStart span == pos && spanEnd span == pos

prop_span_contains_itself :: Int -> Int -> Int -> Int -> Property
prop_span_contains_itself sLine sCol eLine eCol =
  let start = SourcePos sLine sCol
      end = SourcePos (max sLine eLine) (max sCol eCol)  -- Ensure end >= start
      span = SourceSpan start end
  in property $ spanStart span <= spanEnd span

-- | Test position arithmetic
prop_position_arithmetic :: Int -> Int -> Int -> Property
prop_position_arithmetic line col offset =
  let pos = SourcePos line col
      newPos = SourcePos line (col + offset)
  in property $ offset >= 0 ==> newPos >= pos

prop_line_advancement :: Int -> Int -> Int -> Property
prop_line_advancement line col lineInc =
  let pos = SourcePos line col
      newPos = SourcePos (line + lineInc) col
  in property $ lineInc > 0 ==> newPos > pos

-- | Test span properties
prop_span_length :: Int -> Int -> Int -> Int -> Property
prop_span_length sLine sCol eLine eCol =
  let start = SourcePos sLine sCol
      end = SourcePos (max sLine eLine) (max sCol eCol)  -- Ensure end >= start
      span = SourceSpan start end
  in property $ 
    if sLine == eLine 
    then (eCol - sCol) >= 0
    else (eLine - sLine) >= 0

-- | Test located values
prop_located_preserves_value :: Int -> Int -> Int -> Int -> String -> Property
prop_located_preserves_value sLine sCol eLine eCol value =
  let start = SourcePos sLine sCol
      end = SourcePos (max sLine eLine) (max sCol eCol)
      located = Located { locatedValue = value, locatedSpan = SourceSpan start end }
  in property $ locatedValue located == value

-- | Test span ordering transitivity
prop_span_ordering_transitive :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_span_ordering_transitive sLine1 sCol1 eLine1 eCol2 sLine2 sCol2 eLine3 eCol3 =
  let start1 = SourcePos sLine1 sCol1
      end1 = SourcePos (max sLine1 eLine1) (max sCol1 eCol2)
      start2 = SourcePos sLine2 sCol2
      end2 = SourcePos (max sLine2 eLine3) (max sCol2 eCol3)
      start3 = SourcePos (sLine2 + eLine3) (sCol2 + eCol3)
      end3 = SourcePos (sLine2 + eLine3 + eLine3) (sCol2 + eCol3 + eCol3)
      span1 = SourceSpan start1 end1
      span2 = SourceSpan start2 end2
      span3 = SourceSpan start3 end3
  in property $ 
    (span1 <= span2 && span2 <= span3) ==> span1 <= span3

tests :: TestTree
tests = testGroup "Enhanced SourceLocation Tests"
  [ testGroup "SourcePos tests"
    [ testProperty "source pos ordering" prop_source_pos_ordering
    , testProperty "source pos equality" prop_source_pos_equality
    ]
  , testGroup "SourceSpan tests"
    [ testProperty "span start end consistency" prop_span_start_end_consistency
    , testProperty "span ordering" prop_span_ordering
    , testProperty "span creation" prop_span_creation
    , testProperty "span contains itself" prop_span_contains_itself
    , testProperty "span length" prop_span_length
    , testProperty "span ordering transitive" prop_span_ordering_transitive
    ]
  , testGroup "Located tests"
    [ testProperty "located value extraction" prop_located_value_extraction
    , testProperty "located span consistency" prop_located_span_consistency
    , testProperty "located preserves value" prop_located_preserves_value
    ]
  , testGroup "Position arithmetic"
    [ testProperty "position arithmetic" prop_position_arithmetic
    , testProperty "line advancement" prop_line_advancement
    ]
  ]