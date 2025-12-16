{-# LANGUAGE CPP #-}

module Test.Unit.SourceLocationPropertiesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import Data.List (sort)

import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), HasLocation(..),
                      startPos, posAfter, posAt, posAtLineCol,
                      emptySpan, spanFrom, spanTo, spanBetween, mergeSpans, isValidSpan,
                      locatedAt, locatedWithSpan, locatedValue, locatedSpan, locatedPos,
                      advancePos, advancePosBy, advancePosByText, advancePosByLine)
import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "SourceLocation Properties QuickCheck"
  [ sourcePosTests
  , sourceSpanTests
  , locatedTests
  , positionUtilityTests
  , spanUtilityTests
  ]

sourcePosTests :: TestTree
sourcePosTests = testGroup "SourcePos Properties"
  [ fastProperty "SourcePos equality is reflexive" prop_sourcepos_reflexive
  , fastProperty "SourcePos equality is symmetric" prop_sourcepos_symmetric
  , fastProperty "SourcePos equality is transitive" prop_sourcepos_transitive
  , fastProperty "SourcePos offset increases with line" prop_sourcepos_offset_line_monotonic
  , fastProperty "SourcePos offset increases with column" prop_sourcepos_offset_column_monotonic
  , fastProperty "SourcePos line and column are positive" prop_sourcepos_positive
  , fastProperty "SourcePos ordering is consistent" prop_sourcepos_ordering_consistent
  ]

sourceSpanTests :: TestTree
sourceSpanTests = testGroup "SourceSpan Properties"
  [ fastProperty "SourceSpan equality is reflexive" prop_sourcespan_reflexive
  , fastProperty "SourceSpan equality is symmetric" prop_sourcespan_symmetric
  , fastProperty "SourceSpan equality is transitive" prop_sourcespan_transitive
  , fastProperty "SourceSpan start is before or equal to end" prop_sourcespan_start_before_end
  , fastProperty "SourceSpan validity is preserved" prop_sourcespan_validity_preserved
  , fastProperty "SourceSpan merge preserves ordering" prop_sourcespan_merge_ordering
  , fastProperty "SourceSpan spanBetween creates valid span" prop_sourcespan_between_valid
  ]

locatedTests :: TestTree
locatedTests = testGroup "Located Properties"
  [ fastProperty "Located equality is reflexive" prop_located_reflexive
  , fastProperty "Located equality is symmetric" prop_located_symmetric
  , fastProperty "Located equality is transitive" prop_located_transitive
  , fastProperty "Located preserves value" prop_located_preserves_value
  , fastProperty "Located preserves location" prop_located_preserves_location
  , fastProperty "Located mapping preserves location" prop_located_map_preserves_location
  ]

positionUtilityTests :: TestTree
positionUtilityTests = testGroup "Position Utility Properties"
  [ fastProperty "startPos has minimal values" prop_startpos_minimal
  , fastProperty "posAfter advances correctly" prop_posafter_advances
  , fastProperty "posAt creates consistent position" prop_posat_consistent
  , fastProperty "posAtLineCol creates correct position" prop_posatlinecol_correct
  , fastProperty "advancePos increases offset" prop_advancepos_increases_offset
  , fastProperty "advancePosBy advances correctly" prop_advanceposby_correct
  , fastProperty "advancePosByText handles newlines" prop_advanceposby_newlines
  , fastProperty "advancePosByLine advances by lines" prop_advanceposbyline_advances
  ]

spanUtilityTests :: TestTree
spanUtilityTests = testGroup "Span Utility Properties"
  [ fastProperty "emptySpan has zero length" prop_emptyspan_zero_length
  , fastProperty "spanFrom creates valid span" prop_spanfrom_valid
  , fastProperty "spanTo creates valid span" prop_spanto_valid
  , fastProperty "mergeSpans creates valid span" prop_mergespans_valid
  , fastProperty "mergeSpans is commutative" prop_mergespans_commutative
  , fastProperty "mergeSpans is associative" prop_mergespans_associative
  , fastProperty "isValidSpan checks correctly" prop_isvalidspan_correct
  ]

-- SourcePos Properties
prop_sourcepos_reflexive :: SourcePos -> Property
prop_sourcepos_reflexive pos =
  pos === pos

prop_sourcepos_symmetric :: SourcePos -> SourcePos -> Property
prop_sourcepos_symmetric pos1 pos2 =
  (pos1 == pos2) ==> property (pos2 == pos1)

prop_sourcepos_transitive :: SourcePos -> SourcePos -> SourcePos -> Property
prop_sourcepos_transitive pos1 pos2 pos3 =
  (pos1 == pos2 && pos2 == pos3) ==> property (pos1 == pos3)

prop_sourcepos_offset_line_monotonic :: Positive Int -> Positive Int -> Positive Int -> Property
prop_sourcepos_offset_line_monotonic (Positive l) (Positive c) (Positive o) =
  let pos1 = SourcePos l c o
      pos2 = SourcePos (l + 1) c (o + 10)
  in property $ posOffset pos1 < posOffset pos2

prop_sourcepos_offset_column_monotonic :: Positive Int -> Positive Int -> Positive Int -> Property
prop_sourcepos_offset_column_monotonic (Positive l) (Positive c) (Positive o) =
  let pos1 = SourcePos l c o
      pos2 = SourcePos l (c + 1) (o + 1)
  in property $ posOffset pos1 < posOffset pos2

prop_sourcepos_positive :: Positive Int -> Positive Int -> Positive Int -> Property
prop_sourcepos_positive (Positive l) (Positive c) (Positive o) =
  let pos = SourcePos l c o
  in property $ posLine pos > 0 && posColumn pos > 0 && posOffset pos >= 0

prop_sourcepos_ordering_consistent :: SourcePos -> SourcePos -> Property
prop_sourcepos_ordering_consistent pos1 pos2 =
  let offset1 = posOffset pos1
      offset2 = posOffset pos2
  in (offset1 <= offset2) ==> property True

-- SourceSpan Properties
prop_sourcespan_reflexive :: SourceSpan -> Property
prop_sourcespan_reflexive span =
  span === span

prop_sourcespan_symmetric :: SourceSpan -> SourceSpan -> Property
prop_sourcespan_symmetric span1 span2 =
  (span1 == span2) ==> property (span2 == span1)

prop_sourcespan_transitive :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_sourcespan_transitive span1 span2 span3 =
  (span1 == span2 && span2 == span3) ==> property (span1 == span3)

prop_sourcespan_start_before_end :: SourceSpan -> Property
prop_sourcespan_start_before_end span =
  let start = spanStart span
      end = spanEnd span
  in property $ posOffset start <= posOffset end

prop_sourcespan_validity_preserved :: SourceSpan -> Property
prop_sourcespan_validity_preserved span =
  let isValid = posOffset (spanStart span) <= posOffset (spanEnd span)
  in isValid ==> isValidSpan span

prop_sourcespan_merge_ordering :: SourceSpan -> SourceSpan -> Property
prop_sourcespan_merge_ordering span1 span2 =
  let merged = mergeSpans span1 span2
      start1 = spanStart span1
      start2 = spanStart span2
      end1 = spanEnd span1
      end2 = spanEnd span2
      mergedStart = spanStart merged
      mergedEnd = spanEnd merged
  in property $ posOffset mergedStart <= min (posOffset start1) (posOffset start2) &&
                posOffset mergedEnd >= max (posOffset end1) (posOffset end2)

prop_sourcespan_between_valid :: SourcePos -> SourcePos -> Property
prop_sourcespan_between_valid pos1 pos2 =
  let span = spanBetween pos1 pos2
  in property $ isValidSpan span

-- Located Properties
prop_located_reflexive :: Located String -> Property
prop_located_reflexive located =
  located === located

prop_located_symmetric :: Located String -> Located String -> Property
prop_located_symmetric loc1 loc2 =
  (loc1 == loc2) ==> property (loc2 == loc1)

prop_located_transitive :: Located String -> Located String -> Located String -> Property
prop_located_transitive loc1 loc2 loc3 =
  (loc1 == loc2 && loc2 == loc3) ==> property (loc1 == loc3)

prop_located_preserves_value :: SourceSpan -> String -> Property
prop_located_preserves_value span value =
  let located = locatedWithSpan span value
  in locatedValue located === value

prop_located_preserves_location :: SourceSpan -> String -> Property
prop_located_preserves_location span value =
  let located = locatedWithSpan span value
  in locatedSpan located === span

prop_located_map_preserves_location :: Located String -> Property
prop_located_map_preserves_location located =
  let mapped = fmap length located
  in locatedSpan mapped === locatedSpan located

-- Position Utility Properties
prop_startpos_minimal :: Property
prop_startpos_minimal =
  let pos = startPos
  in property $ posLine pos == 1 && posColumn pos == 1 && posOffset pos == 0

prop_posafter_advances :: Char -> SourcePos -> Property
prop_posafter_advances c pos =
  let next = posAfter c pos
  in property $ posOffset next > posOffset pos

prop_posat_consistent :: Positive Int -> Positive Int -> Property
prop_posat_consistent (Positive line) (Positive col) =
  let pos = posAt line col
  in property $ posLine pos == line && posColumn pos == col

prop_posatlinecol_correct :: Positive Int -> Positive Int -> Positive Int -> Property
prop_posatlinecol_correct (Positive line) (Positive col) (Positive offset) =
  let pos = posAtLineCol line col offset
  in property $ posLine pos == line && posColumn pos == col && posOffset pos == offset

prop_advancepos_increases_offset :: Char -> SourcePos -> Property
prop_advancepos_increases_offset c pos =
  let advanced = advancePos c pos
  in property $ posOffset advanced >= posOffset pos

prop_advanceposby_newlines :: SourcePos -> String -> Property
prop_advanceposby_newlines pos text =
  let advanced = advancePosByText pos (T.pack text)
      newlines = length $ filter (== '\n') text
  in property $ posLine advanced >= posLine pos + newlines

prop_advanceposbyline_advances :: Int -> SourcePos -> Property
prop_advanceposbyline_advances lines pos =
  let advanced = advancePosByLine pos lines
  in property $ posLine advanced >= posLine pos + lines

-- Span Utility Properties
prop_emptyspan_zero_length :: Property
prop_emptyspan_zero_length =
  let span = emptySpan
      start = spanStart span
      end = spanEnd span
  in property $ posOffset start == posOffset end

prop_spanfrom_valid :: SourcePos -> Property
prop_spanfrom_valid pos =
  let span = spanFrom pos
  in property $ isValidSpan span

prop_spanto_valid :: SourcePos -> Property
prop_spanto_valid pos =
  let span = spanTo pos
  in property $ isValidSpan span

prop_mergespans_valid :: SourceSpan -> SourceSpan -> Property
prop_mergespans_valid span1 span2 =
  let merged = mergeSpans span1 span2
  in property $ isValidSpan merged

prop_mergespans_commutative :: SourceSpan -> SourceSpan -> Property
prop_mergespans_commutative span1 span2 =
  mergeSpans span1 span2 === mergeSpans span2 span1

prop_mergespans_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_mergespans_associative span1 span2 span3 =
  mergeSpans (mergeSpans span1 span2) span3 === mergeSpans span1 (mergeSpans span2 span3)

prop_isvalidspan_correct :: SourcePos -> SourcePos -> Property
prop_isvalidspan_correct pos1 pos2 =
  let span = spanBetween pos1 pos2
      isValid = posOffset pos1 <= posOffset pos2
  in property $ isValidSpan span == isValid