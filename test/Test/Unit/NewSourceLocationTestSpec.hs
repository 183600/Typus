{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewSourceLocationTestSpec where


import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck



import Test.Tasty
import Test.Tasty.QuickCheck

import SourceLocation
import qualified Data.Text as T
import Compiler.Errors.Core (ErrorLocation(..))

-- | 测试SourcePos的基本属性
prop_sourcePos_monotonic :: Positive Int -> Positive Int -> Positive Int -> Property
prop_sourcePos_monotonic (Positive line) (Positive col) (Positive n) =
  let pos = SourcePos line col 0
      posAfter' = advancePos '\n' pos
  in if line /= 0 && col /= 0
     then property (posLine posAfter' >= posLine pos)
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
  locValue (locatedAt pos val) == val

-- | 测试位置计算
prop_posAfter_newline_increments_line :: Positive Int -> Positive Int -> Property
prop_posAfter_newline_increments_line (Positive line) (Positive col) =
  let pos = SourcePos line col 0
      posAfter' = advancePos '\n' pos
  in if line > 0 && col > 0
     then posLine posAfter' === posLine pos + 1 .&&. posColumn posAfter' === 1
     else property True

-- | 测试spanBetween的正确性
prop_spanBetween_order_independent :: SourcePos -> SourcePos -> Property
prop_spanBetween_order_independent pos1 pos2 =
  if pos1 /= pos2
  then let span1 = spanBetweenOrdered pos1 pos2
           span2 = spanBetweenOrdered pos2 pos1
           minPos = if pos1 <= pos2 then pos1 else pos2
           maxPos = if pos1 >= pos2 then pos1 else pos2
       in spanStart span1 === minPos .&&. 
          spanEnd span1 === maxPos .&&.
          spanStart span2 === minPos .&&.
          spanEnd span2 === maxPos
  else property True

-- | 测试LocationTracker的基本功能
test_locationTracker_basic :: Assertion
test_locationTracker_basic = do
  let initialPos = startPos
      result = runLocationTracker $ do
        setCurrentPos (SourcePos 2 5 0)
        getCurrentPos
  assertEqual "Position should be updated" (SourcePos 2 5 0) result

-- | 测试错误位置转换
test_errorLocation_conversion :: Assertion
test_errorLocation_conversion = do
  let pos = SourcePos 10 20 0
      span = spanFrom pos
      errorLoc = toErrorLocationWithSpan span
  assertEqual "Error line should match" 10 (line errorLoc)
  assertEqual "Error column should match" 20 (column errorLoc)

-- | 测试空span的行为
test_emptySpan_properties :: Assertion
test_emptySpan_properties = do
  let empty = emptySpan startPos
  assertEqual "Empty span should start at startPos" startPos (spanStart empty)
  assertEqual "Empty span should end at startPos" startPos (spanEnd empty)
  assertBool "Empty span should be valid" (isValidSpan empty)

-- | 生成任意SourcePos用于QuickCheck测试
-- Arbitrary instance for SourcePos is now defined in SourceLocation module


-- | 生成任意SourceSpan用于QuickCheck测试
-- Arbitrary instance for SourceSpan is now defined in SourceLocation module


-- | 生成任意Located值用于QuickCheck测试
instance Arbitrary a => Arbitrary (Located a) where
  arbitrary = Located <$> arbitrary <*> arbitrary <*> arbitrary

-- | 辅助函数：检查SourcePos的顺序
sourcePosLe :: SourcePos -> SourcePos -> Bool
sourcePosLe (SourcePos l1 c1 _) (SourcePos l2 c2 _) = 
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