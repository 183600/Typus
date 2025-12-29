{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.NewSourceLocationQuickCheckTestSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , startPos
  , posAfter
  , emptySpan
  , spanFrom
  , mergeSpans
  , isValidSpan
  , advancePos
  )

-- 测试SourcePos的性质
prop_pos_after_advances :: Positive Int -> Positive Int -> Char -> Bool
prop_pos_after_advances (Positive line) (Positive col) c = 
  let pos = SourcePos line col
      newPos = posAfter pos c
  in case c of
    '\n' -> sourceLine newPos == line + 1 && sourceColumn newPos == 1
    '\t' -> sourceLine newPos == line && sourceColumn newPos >= col + 1
    _ -> sourceLine newPos == line && sourceColumn newPos == col + 1

prop_start_pos_properties :: Bool
prop_start_pos_properties = 
  let pos = startPos
  in sourceLine pos == 1 && sourceColumn pos == 1

-- 测试SourceSpan的性质  
prop_empty_span_invalid :: Bool
prop_empty_span_invalid = not (isValidSpan emptySpan)

prop_span_from_single_pos :: Positive Int -> Positive Int -> Bool
prop_span_from_single_pos (Positive line) (Positive col) =
  let pos = SourcePos line col
      span = spanFrom pos
  in spanStart span == pos && spanEnd span == pos

prop_merge_spans_properties :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> Bool
prop_merge_spans_properties (Positive line1) (Positive col1) (Positive line2) (Positive col2) =
  let pos1 = SourcePos line1 col1
      pos2 = SourcePos line2 col2
      span1 = spanFrom pos1
      span2 = spanFrom pos2
      merged = mergeSpans span1 span2
  in spanStart merged == min pos1 pos2 && spanEnd merged == max pos1 pos2

-- 测试位置推进的性质
prop_advance_pos_newline :: Positive Int -> Positive Int -> Bool
prop_advance_pos_newline (Positive line) (Positive col) =
  let pos = SourcePos line col
      newPos = advancePos pos '\n'
  in sourceLine newPos == line + 1 && sourceColumn newPos == 1

prop_advance_pos_regular_char :: Positive Int -> Positive Int -> Char -> Bool
prop_advance_pos_regular_char (Positive line) (Positive col) c = 
  c `notElem` "\n\r" ==>
  let pos = SourcePos line col
      newPos = advancePos pos c
  in sourceLine newPos == line && sourceColumn newPos == col + 1

-- 测试span有效性的性质
prop_valid_span_consistency :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> Bool
prop_valid_span_consistency (Positive line1) (Positive col1) (Positive line2) (Positive col2) =
  let pos1 = SourcePos line1 col1
      pos2 = SourcePos line2 col2
      span1 = spanBetween pos1 pos2
  in isValidSpan span1 == (pos1 <= pos2)

-- 生成测试套件
tests :: TestTree
tests = testGroup "SourceLocation QuickCheck Tests"
  [ testProperty "posAfter advances correctly" prop_pos_after_advances
  , testProperty "startPos has correct coordinates" prop_start_pos_properties
  , testProperty "emptySpan is invalid" prop_empty_span_invalid
  , testProperty "spanFrom creates span from single position" prop_span_from_single_pos
  , testProperty "mergeSpans combines spans correctly" prop_merge_spans_properties
  , testProperty "advancePos handles newline correctly" prop_advance_pos_newline
  , testProperty "advancePos handles regular characters" prop_advance_pos_regular_char
  , testProperty "validSpan consistency" prop_valid_span_consistency
  ]