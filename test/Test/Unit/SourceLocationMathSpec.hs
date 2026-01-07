module Test.Unit.SourceLocationMathSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import SourceLocation

-- Test SourcePos arithmetic
prop_pos_addition :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> Property
prop_pos_addition (Positive line1) (Positive col1) (Positive line2) (Positive col2) =
  let pos1 = posAt line1 col1
      pos2 = posAt line2 col2
      posSum = posAt (line1 + line2) (col1 + col2)
  in property $ pos1 `posAfter` pos2 === posSum

-- Test span merging
prop_span_merge_associative :: Positive Int -> Positive Int -> Positive Int -> 
                               Positive Int -> Positive Int -> Positive Int -> Property
prop_span_merge_associative (Positive l1) (Positive c1) (Positive l2) (Positive c2) 
                             (Positive l3) (Positive c3) =
  let pos1 = posAt l1 c1
      pos2 = posAt l2 c2
      pos3 = posAt l3 c3
      span12 = spanBetween pos1 pos2
      span23 = spanBetween pos2 pos3
      span123 = spanBetween pos1 pos3
      merged1 = mergeSpans span12 span23
  in property $ span123 === merged1

-- Test span validity
prop_span_validity :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> Property
prop_span_validity (Positive l1) (Positive c1) (Positive l2) (Positive c2) =
  let pos1 = posAt l1 c1
      pos2 = posAt l2 c2
      span = spanBetween pos1 pos2
  in property $ isValidSpan span

-- Test position advancement
prop_advance_pos_newline :: Positive Int -> Positive Int -> String -> Property
prop_advance_pos_newline (Positive line) (Positive col) s =
  let pos = posAt line col
      posWithNewlines = foldl advancePos pos s
      newlineCount = length $ filter (== '\n') s
  in property $ posLine posWithNewlines === line + newlineCount

tests :: TestTree
tests = testGroup "SourceLocation Math Tests"
  [ testProperty "position addition" prop_pos_addition
  , testProperty "span merging is associative" prop_span_merge_associative
  , testProperty "span validity" prop_span_validity
  , testProperty "position advancement with newlines" prop_advance_pos_newline
  ]