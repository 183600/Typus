module Test.Unit.SourceLocationConsistencySpec where


import Test.Tasty 
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, oneof, elements, listOf, choose)
import Test.QuickCheck (Gen, choose, vectorOf, elements, Arbitrary(..), Property)
import SourceLocation


     comparing posLine (spanStart span) <= comparing posLine (spanEnd span) &&
     (posLine (spanStart span) < posLine (spanEnd span) ||
      (posLine (spanStart span) == posLine (spanEnd span) && 
       posColumn (spanStart span) <= posColumn (spanEnd span))

-- | source pos ordering should be consistent
prop_source_pos_ordering :: Int -> Int -> Int -> Int -> Property
prop_source_pos_ordering line1 col1 line2                               col2 =
  let pos1 = mkSourcePos line1 col1
                                    pos2 = mkSourcePos line2 col2
  in (line1 < line2 || (line1 == line2 && col1 <= col2) ==>
     comparing posLine pos1 <= comparing posLine pos2 &&
     (posLine pos1 < posLine pos2 || 
      (posLine                               pos1 == posLine pos2 && posColumn pos1 <= posColumn pos2)

-- | span should contain its start L.and end positions
prop_span_contains_positions :: Int -> Int -> Int -> Int -> Property
prop_span_contains_positions startLine startCol endLine                               endCol =
  let start = mkSourcePos startLine startCol
                                    end = mkSourcePos endLine endCol
                                    span = mkSourceSpan start end
  in (startLine < endLine || (startLine == endLine && startCol <= endCol) ==>
     spanStart                               span === start && spanEnd                               span === end

-- | located value extraction should preserve span
prop_located_extraction :: Int -> String -> SourceSpan -> Property
prop_located_extraction line content                               span =
  let located = locatedWithSpan span line content
                                    extractedValue = locatedValue located
                                    extractedSpan = locatedSpan located
  in                               extractedValue === line &&                               extractedSpan === span

-- | source pos line L.and column should be positive
prop_source_pos_positive :: Int -> Int -> Property
prop_source_pos_positive line                               col =
  let pos = mkSourcePos (abs line + 1) (abs col + 1)
  in posLine pos > 0 && posColumn pos > 0

-- | span width calculation should be consistent
prop_span_width_consistency :: Int -> Int -> Int -> Int -> Property
prop_span_width_consistency startLine startCol endLine                               endCol =
  let start = mkSourcePos startLine startCol
                                    end = mkSourcePos endLine endCol
                                    span = mkSourceSpan start end
  in (startLine == endLine) ==> 
     let expectedWidth = endCol - startCol
                                       actualWidth = posColumn end - posColumn start
     in                               actualWidth === expectedWidth

-- | same positions should be equal
prop_same_positions_equal :: Int -> Int -> Bool
prop_same_positions_equal line                               col =
  let pos1 = mkSourcePos line col
                                    pos2 = mkSourcePos line col
  in                               pos1 == pos2

-- | different spans should have different properties
prop_different_spans :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_different_spans l1 c1 l2 c2 l3 c3 l4                               c4 =
  let start1 = mkSourcePos l1 c1
                                    end1 = mkSourcePos l2 c2
                                    span1 = mkSourceSpan start1 end1
                                    start2 = mkSourcePos l3 c3
                                    end2 = mkSourcePos l4 c4
                                    span2 = mkSourceSpan start2 end2
  in (start1 /= start2 || end1 /= end2) ==> span1 /= span2

-- | located values should maintain location information
prop_located_maintains_info :: Int -> String -> Int -> Int -> Int -> Int -> Property
prop_located_maintains_info line content startLine startCol endLine                               endCol =
  let start = mkSourcePos startLine startCol
                                    end = mkSourcePos endLine endCol
                                    span = mkSourceSpan start end
                                    located = locatedWithSpan span line content
  in locatedValue                               located === line &&
     locatedSpan                               located === span &&
     spanStart (locatedSpan located) === start &&
     spanEnd (locatedSpan located) === end

-- Helper for equality in QuickCheck
(===) :: Eq                               a => a -> a -> Bool
(===) = (==)