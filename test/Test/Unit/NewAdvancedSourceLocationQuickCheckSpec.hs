{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.NewAdvancedSourceLocationQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.TH
import SourceLocation
  ( SourcePos(..), SourceSpan(..), Located(..)
  , startPos, posAfter, posAt, posAtLineCol
  , emptySpan, spanFrom, spanTo, spanBetween, mergeSpans, isValidSpan
  , locatedAt, locatedWithSpan, locatedValue, locatedSpan, mapLocated
  , advancePos, advancePosBy, advancePosByText, advancePosByLine
  )
import Data.Text (Text)
import qualified Data.Text as T
import Control.DeepSeq (NFData, rnf)

-- Test SourcePos properties
prop_start_pos_valid :: Bool
prop_start_pos_valid = 
  posLine startPos > 0 && posColumn startPos > 0 && posOffset startPos >= 0

prop_pos_after_newline_increments_line :: Int -> Property
prop_pos_after_newline_increments_line lineNum = 
  lineNum >= 0 ==> 
  let pos = posAt lineNum 1
      newPos = posAfter '\n' pos
  in posLine newPos == lineNum + 1 && posColumn newPos == 1

prop_pos_after_tab_advances_to_next_tab_stop :: Int -> Property
prop_pos_after_tab_advances_to_next_tab_stop col = 
  col > 0 && col <= 8 ==> 
  let pos = posAt 1 col
      newPos = posAfter '\t' pos
      expectedCol = ((col - 1) `div` 8 + 1) * 8 + 1
  in posColumn newPos == expectedCol

prop_pos_after_regular_char_increments_column :: Char -> Int -> Property
prop_pos_after_regular_char_increments_column c col = 
  c /= '\n' && c /= '\t' && col > 0 ==> 
  let pos = posAt 1 col
      newPos = posAfter c pos
  in posColumn newPos == col + 1 && posLine newPos == 1

-- Test SourceSpan properties
prop_empty_span_valid :: Bool
prop_empty_span_valid = 
  let span = emptySpan startPos
  in isValidSpan span && spanStart span == spanEnd span

prop_span_between_valid :: Int -> Int -> Int -> Int -> Property
prop_span_between_valid line1 col1 line2 col2 = 
  line1 > 0 && col1 > 0 && line2 > 0 && col2 > 0 ==> 
  let start = posAt line1 col1
      end = posAt line2 col2
      span = spanBetween start end
  in spanStart span == start && spanEnd span == end

prop_merge_spans_contains_originals :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_merge_spans_contains_originals l1 c1 l2 c2 l3 c3 l4 c4 = 
  all (>0) [l1, c1, l2, c2, l3, c3, l4, c4] ==> 
  let span1 = spanBetween (posAt l1 c1) (posAt l2 c2)
      span2 = spanBetween (posAt l3 c3) (posAt l4 c4)
      merged = mergeSpans span1 span2
  in spanStart merged <= spanStart span1 && spanEnd merged >= spanEnd span1 &&
     spanStart merged <= spanStart span2 && spanEnd merged >= spanEnd span2

-- Test Located properties
prop_located_at_preserves_position :: Int -> Int -> String -> Property
prop_located_at_preserves_position line col value = 
  line > 0 && col > 0 ==> 
  let pos = posAt line col
      located = locatedAt pos value
  in locatedPos located == pos && locatedValue located == value

prop_located_with_span_preserves_span :: Int -> Int -> Int -> Int -> String -> Property
prop_located_with_span_preserves_span l1 c1 l2 c2 value = 
  all (>0) [l1, c1, l2, c2] ==> 
  let span = spanBetween (posAt l1 c1) (posAt l2 c2)
      located = locatedWithSpan span value
  in locatedSpan located == span && locatedValue located == value

prop_map_located_preserves_location :: Int -> Int -> Int -> Property
prop_map_located_preserves_location line col n = 
  line > 0 && col > 0 ==> 
  let pos = posAt line col
      located = locatedAt pos n
      doubled = mapLocated (*2) located
  in locatedPos doubled == pos && locatedSpan doubled == locatedSpan located &&
     locatedValue doubled == n * 2

-- Test position advancement properties
prop_advance_pos_by_empty_string :: SourcePos -> Bool
prop_advance_pos_by_empty_string pos = advancePosBy "" pos == pos

prop_advance_pos_by_consistency :: String -> SourcePos -> Bool
prop_advance_pos_by_consistency s pos = 
  let advanced = advancePosBy s pos
      charAdvanced = foldl (flip advancePos) pos s
  in advanced == charAdvanced

prop_advance_pos_by_text_consistency :: String -> SourcePos -> Bool
prop_advance_pos_by_text_consistency s pos = 
  let text = T.pack s
      advancedByText = advancePosByText text pos
      advancedByString = advancePosBy s pos
  in advancedByText == advancedByString

prop_advance_pos_by_line_increments_line :: Int -> Int -> Int -> Property
prop_advance_pos_by_line_increments_line line numLines col = 
  line > 0 && numLines >= 0 && col > 0 ==> 
  let pos = posAt line col
      newPos = advancePosByLine numLines pos
  in posLine newPos == line + numLines && posColumn newPos == 1

-- Test NFData instances
prop_sourcepos_nfdata :: SourcePos -> Bool
prop_sourcepos_nfdata pos = rnf pos == ()

prop_sourcespan_nfdata :: SourceSpan -> Bool
prop_sourcespan_nfdata span = rnf span == ()

prop_located_nfdata :: Located String -> Bool
prop_located_nfdata located = rnf located == ()

-- Arbitrary instances for QuickCheck
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 1000)
    col <- choose (1, 1000)
    offset <- choose (0, 1000000)
    return $ SourcePos line col offset

instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    endOffset <- choose (0, 1000)
    let end = SourcePos (posLine start) (posColumn start + endOffset) (posOffset start + endOffset)
    return $ SourceSpan start end

instance Arbitrary a => Arbitrary (Located a) where
  arbitrary = do
    value <- arbitrary
    span <- arbitrary
    return $ Located value (spanStart span) span

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain tests