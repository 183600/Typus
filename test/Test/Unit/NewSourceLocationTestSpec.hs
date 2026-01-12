{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewSourceLocationTestSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import SourceLocation
import qualified Data.Text as T

-- | 测试SourcePos的基本属性
prop_sourcePos_monotonic :: Positive Int -> Positive Int -> Positive Int -> Property
prop_sourcePos_monotonic (Positive line) (Positive col) (Positive n) =
  let pos = SourcePos line col
      posAfter = advancePos '\n' pos
  in if line /= 0 && col /= 0
     then sourceLine posAfter >= sourceLine pos
     else property True

-- | 测试SourceSpan的合并操作
prop_mergeSpans_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_mergeSpans_associative span1 span2 span3 =
  let merge12 = mergeSpans span1 span2
      merge23 = mergeSpans span2 span3
      merge123_1 = mergeSpans merge12 span3
      merge123_2 = mergeSpans span1 merge23
  in if isValidSpan span1 && isValidSpan span2 && isValidSpan span3
     then merge123_1 === merge123_2
     else property True

-- | 测试Located值的位置追踪
prop_locatedAt_preserves_value :: String -> SourcePos -> Bool
prop_locatedAt_preserves_value val pos =
  locatedValue (locatedAt pos val) == val

-- | 测试位置计算
prop_posAfter_newline_increments_line :: Positive Int -> Positive Int -> Property
prop_posAfter_newline_increments_line (Positive line) (Positive col) =
  let pos = SourcePos line col
      posAfter = advancePos '\n' pos
  in if line > 0 && col > 0
     then sourceLine posAfter === sourceLine pos + 1 .&&. sourceColumn posAfter === 1
     else property True

-- | 测试spanBetween的正确性
prop_spanBetween_order_independent :: SourcePos -> SourcePos -> Property
prop_spanBetween_order_independent pos1 pos2 =
  if pos1 /= pos2
  then let span1 = spanBetween pos1 pos2
           span2 = spanBetween pos2 pos1
       in spanStart span1 === min pos1 pos2 .&&. 
          spanEnd span1 === max pos1 pos2 .&&.
          spanStart span2 === min pos1 pos2 .&&.
          spanEnd span2 === max pos1 pos2
  else property True

-- | 测试LocationTracker的基本功能
test_locationTracker_basic :: Assertion
test_locationTracker_basic = do
  let initialPos = startPos
      (result, finalState) = runLocationTracker $ do
        setCurrentPos (SourcePos 2 5)
        getCurrentPos
  assertEqual "Position should be updated" (SourcePos 2 5) result
  assertEqual "Final state should match" (SourcePos 2 5) finalState

-- | 测试错误位置转换
test_errorLocation_conversion :: Assertion
test_errorLocation_conversion = do
  let pos = SourcePos 10 20
      span = spanFrom pos "test string"
      errorLoc = toErrorLocationWithSpan span
  assertEqual "Error line should match" 10 (errorLine errorLoc)
  assertEqual "Error column should match" 20 (errorColumn errorLoc)

-- | 测试空span的行为
test_emptySpan_properties :: Assertion
test_emptySpan_properties = do
  let empty = emptySpan
  assertEqual "Empty span should start at startPos" startPos (spanStart empty)
  assertEqual "Empty span should end at startPos" startPos (spanEnd empty)
  assertBool "Empty span should be valid" (isValidSpan empty)

-- | 生成任意SourcePos用于QuickCheck测试
instance Arbitrary SourcePos where
  arbitrary = SourcePos <$> arbitraryPositive <*> arbitraryPositive
    where
      arbitraryPositive = getPositive <$> arbitrary

-- | 生成任意SourceSpan用于QuickCheck测试
instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    if sourcePosLe start end
      then return $ SourceSpan start end
      else return $ SourceSpan end start

-- | 生成任意Located值用于QuickCheck测试
instance Arbitrary a => Arbitrary (Located a) where
  arbitrary = Located <$> arbitrary <*> arbitrary

-- | 辅助函数：检查SourcePos的顺序
sourcePosLe :: SourcePos -> SourcePos -> Bool
sourcePosLe (SourcePos l1 c1) (SourcePos l2 c2) = 
  l1 < l2 || (l1 == l2 && c1 <= c2)

-- | 测试套件
tests :: TestTree
tests = testGroup "New SourceLocation Tests"
  [ testProperty "SourcePos monotonic property" prop_sourcePos_monotonic
  , testProperty "Merge spans is associative" prop_mergeSpans_associative
  , testProperty "LocatedAt preserves value" prop_locatedAt_preserves_value
  , testProperty "PosAfter newline increments line" prop_posAfter_newline_increments_line
  , testProperty "SpanBetween is order independent" prop_spanBetween_order_independent
  , testCase "LocationTracker basic functionality" test_locationTracker_basic
  , testCase "Error location conversion" test_errorLocation_conversion
  , testCase "Empty span properties" test_emptySpan_properties
  ]