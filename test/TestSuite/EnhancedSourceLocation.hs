{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module TestSuite.EnhancedSourceLocation where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import SourceLocation (SourcePos(..), SourceSpan(..), spanStart, spanEnd, locatedWithSpan, locatedValue, locatedSpan)

-- | Test properties for SourceLocation module

-- | SourcePos should be ordered correctly by line, then column, then offset
prop_sourcepos_ordering :: SourcePos -> SourcePos -> Property
prop_sourcepos_ordering pos1 pos2 =
  let line1 = posLine pos1
      col1 = posColumn pos1
      line2 = posLine pos2
      col2 = posColumn pos2
      _ = (posOffset pos1, posOffset pos2)  -- offsets not used in comparison
  in property $ 
    if line1 < line2 then pos1 < pos2
    else if line1 > line2 then pos1 > pos2
    else if col1 < col2 then pos1 < pos2
    else if col1 > col2 then pos1 > pos2
    else pos1 <= pos2 && pos2 <= pos1

-- | SourceSpan should have start <= end in terms of position
prop_sourcespan_consistency :: Positive Int -> Positive Int -> Positive Int -> Property
prop_sourcespan_consistency (Positive line) (Positive col) (Positive len) =
  let start = SourcePos line col 0
      endCol = col + len
      end = SourcePos line endCol (0 :: Int)
      testSpan = SourceSpan start end
  in property $ spanStart testSpan <= spanEnd testSpan

-- | locatedWithSpan should preserve the value and set the span
prop_located_with_span_preserves_value :: String -> SourceSpan -> Property
prop_located_with_span_preserves_value val testSpan =
  let located = locatedWithSpan testSpan val
  in property $ locatedValue located == val && locatedSpan located == testSpan

-- Unit tests
test_sourcepos_creation :: Assertion
test_sourcepos_creation = do
  let pos = SourcePos 10 20 100
  assertEqual "source line" 10 (posLine pos)
  assertEqual "source column" 20 (posColumn pos)
  assertEqual "source offset" 100 (posOffset pos)

test_sourcespan_single_line :: Assertion
test_sourcespan_single_line = do
  let start = SourcePos 5 10 0
      end = SourcePos 5 15 0
      testSpan = SourceSpan start end
  assertEqual "start line" 5 (posLine $ spanStart testSpan)
  assertEqual "end line" 5 (posLine $ spanEnd testSpan)
  assertEqual "start column" 10 (posColumn $ spanStart testSpan)
  assertEqual "end column" 15 (posColumn $ spanEnd testSpan)

test_located_with_span_preserves_value :: Assertion
test_located_with_span_preserves_value = do
  let testSpan = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 0)
      val = "test value"
      located = locatedWithSpan testSpan val
  assertEqual "located value" val (locatedValue located)
  assertEqual "located span" testSpan (locatedSpan located)

-- | Test suite for SourceLocation module
tests :: TestTree
tests = testGroup "Enhanced SourceLocation Tests"
  [ testCase "SourcePos creation" test_sourcepos_creation
  , testCase "SourceSpan single line" test_sourcespan_single_line
  , testCase "locatedWithSpan preserves value" test_located_with_span_preserves_value
  ]