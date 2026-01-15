{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.Unit.ConciseSourceLocationQuickCheckSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperties, Arbitrary(..), Gen, choose, listOf, elements, oneof, vectorOf, property, (===), forAll, counterexample)
import Test.QuickCheck (Gen, Property, (==>))
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), HasLocation(..),
                      startPos, posAfter, posAt, posAtLineCol,
                      emptySpan, spanFrom, spanTo, spanBetween, spanBetweenOrdered,
                      mergeSpans, isValidSpan, isValidBlockSpan,
                      locatedAt, locatedWithSpan, locatedValue, locatedSpan, locatedPos,
                      mapLocated, toErrorLocation, toErrorLocationWithSpan)

-- Helper generators for SourceLocation tests
genSourcePos :: Gen SourcePos
genSourcePos = do
  line <- choose (1, 100)
  col <- choose (1, 100)
  return $ SourcePos line col

genSourceSpan :: Gen SourceSpan
genSourceSpan = do
  start <- genSourcePos
  end <- genSourcePos
  return $ SourceSpan start end

genLocated :: Gen (Located String)
genLocated = do
  value <- elements ["test", "value", "data", "item"]
  span <- genSourceSpan
  return $ Located value span

-- Test properties for SourceLocation module

-- SourcePos tests
prop_source_pos_line_positive :: SourcePos -> Property
prop_source_pos_line_positive pos = sourceLine pos > 0

prop_source_pos_col_positive :: SourcePos -> Property
prop_source_pos_col_positive pos = sourceColumn pos > 0

prop_start_pos_first_line_col :: Property
prop_start_pos_first_line_col = 
  let pos = startPos
  in sourceLine pos === 1 && sourceColumn pos === 1

prop_pos_after_increments_column :: SourcePos -> Property
prop_pos_after_increments_column pos = 
  sourceColumn (posAfter pos) === sourceColumn pos + 1

-- SourceSpan tests
prop_empty_span_valid :: Property
prop_empty_span_valid = isValidSpan emptySpan

prop_span_from_same_pos :: SourcePos -> Property
prop_span_from_same_pos pos = 
  let span = spanFrom pos
  in spanStart span === pos && spanEnd span === pos

prop_span_to_same_pos :: SourcePos -> Property
prop_span_to_same_pos pos = 
  let span = spanTo pos
  in spanStart span === pos && spanEnd pos === pos

prop_span_between_ordered :: SourcePos -> SourcePos -> Property
prop_span_between_ordered pos1 pos2 = 
  let span = spanBetweenOrdered pos1 pos2
      start = spanStart span
      end = spanEnd span
  in (sourceLine start <= sourceLine end || (sourceLine start == sourceLine end && sourceColumn start <= sourceColumn end))

prop_merge_spans_contains_originals :: SourceSpan -> SourceSpan -> Property
prop_merge_spans_contains_originals span1 span2 = 
  let merged = mergeSpans span1 span2
      start1 = spanStart span1
      end1 = spanEnd span1
      start2 = spanStart span2
      end2 = spanEnd span2
      mergedStart = spanStart merged
      mergedEnd = spanEnd merged
  in (sourceLine mergedStart <= sourceLine start1 || (sourceLine mergedStart == sourceLine start1 && sourceColumn mergedStart <= sourceColumn start1)) &&
     (sourceLine mergedEnd >= sourceLine end1 || (sourceLine mergedEnd == sourceLine end1 && sourceColumn mergedEnd >= sourceColumn end1)) &&
     (sourceLine mergedStart <= sourceLine start2 || (sourceLine mergedStart == sourceLine start2 && sourceColumn mergedStart <= sourceColumn start2)) &&
     (sourceLine mergedEnd >= sourceLine end2 || (sourceLine mergedEnd == sourceLine end2 && sourceColumn mergedEnd >= sourceColumn end2))

-- Located tests
prop_located_at_correct_span :: String -> SourceSpan -> Property
prop_located_at_correct_span value span = 
  let located = locatedAt value span
  in locatedValue located === value && locatedSpan located === span

prop_located_with_span_correct :: String -> SourcePos -> SourcePos -> Property
prop_located_with_span_correct value start end = 
  let span = SourceSpan start end
      located = locatedWithSpan value start end
  in locatedValue located === value && locatedSpan located === span

prop_map_located_preserves_span :: Located String -> Property
prop_map_located_preserves_span located = 
  let mapped = mapLocated (reverse) located
  in locatedSpan mapped === locatedSpan located

tests :: TestTree
tests = testGroup "Concise SourceLocation QuickCheck Tests"
  [ testProperties "SourcePos Tests"
    [ ("source pos line positive", prop_source_pos_line_positive)
    , ("source pos col positive", prop_source_pos_col_positive)
    , ("start pos first line col", prop_start_pos_first_line_col)
    , ("pos after increments column", prop_pos_after_increments_column)
    ]
  , testProperties "SourceSpan Tests"
    [ ("empty span valid", prop_empty_span_valid)
    , ("span from same pos", prop_span_from_same_pos)
    , ("span to same pos", prop_span_to_same_pos)
    , ("span between ordered", prop_span_between_ordered)
    , ("merge spans contains originals", prop_merge_spans_contains_originals)
    ]
  , testProperties "Located Tests"
    [ ("located at correct span", prop_located_at_correct_span)
    , ("located with span correct", prop_located_with_span_correct)
    , ("map located preserves span", prop_map_located_preserves_span)
    ]
  ]