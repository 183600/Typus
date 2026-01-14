{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.SourceLocationAdvancedQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import SourceLocation
import Compiler.Errors.Core (ErrorLocation(..), ErrorSeverity(..), getErrorLine, getErrorColumn)
import qualified Data.Text as T
import Data.List (isPrefixOf, isSuffixOf)
import Control.Monad (replicateM)

-- | 测试SourcePos的偏移量计算一致性
prop_sourcePos_offset_consistent :: Positive Int -> Positive Int -> Property
prop_sourcePos_offset_consistent (Positive line) (Positive col) =
  let pos = SourcePos line col 0
      text = T.pack $ replicate (col - 1) ' ' ++ "x"
      pos' = advancePosByText text startPos
  in line > 0 && col > 0 ==> 
     posLine pos' === 1 .&&.
     posColumn pos' === col .&&.
     posOffset pos' === col - 1

-- | 测试多行文本的位置追踪
prop_multiline_position_tracking :: Positive Int -> Positive Int -> Property
prop_multiline_position_tracking (Positive lines) (Positive cols) =
  let lineContent = T.pack $ replicate cols 'x' ++ "\n"
      multiLineText = T.concat $ replicate lines lineContent
      finalPos = advancePosByText multiLineText startPos
  in lines > 0 && cols > 0 ==>
     posLine finalPos === lines + 1 .&&.
     posColumn finalPos === 1

-- | 测试SourceSpan的包含关系
prop_span_contains_monotonic :: SourcePos -> SourcePos -> SourcePos -> Property
prop_span_contains_monotonic start middle end =
  let span = spanBetweenOrdered start end
  in sourcePosLe start middle && sourcePosLe middle end ==> 
     spanContains span middle

-- | 测试span合并的包含性
prop_mergeSpans_contains_originals :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_contains_originals span1 span2 =
  let merged = mergeSpans span1 span2
  in isValidSpan span1 && isValidSpan span2 ==>
     spanContains merged (spanStart span1) .&&.
     spanContains merged (spanEnd span1) .&&.
     spanContains merged (spanStart span2) .&&.
     spanContains merged (spanEnd span2)

-- | 测试Located值的映射保持位置信息
prop_located_map_preserves_span :: String -> String -> SourceSpan -> Property
prop_located_map_preserves_span val1 val2 span =
  let located1 = locatedWithSpan span val1
      located2 = fmap reverse located1
  in locatedSpan located1 === locatedSpan located2 .&&.
     locatedSpan located2 === span

-- | 测试位置比较的传递性
prop_pos_comparison_transitive :: SourcePos -> SourcePos -> SourcePos -> Property
prop_pos_comparison_transitive pos1 pos2 pos3 =
  let le x y = x <= y
  in (le pos1 pos2 && le pos2 pos3) ==> le pos1 pos3

-- | 测试制表符位置计算
prop_tab_position_calculation :: Positive Int -> Property
prop_tab_position_calculation (Positive col) =
  let pos = SourcePos 1 col 0
      posAfterTab = advancePos '\t' pos
      expectedCol = ((col - 1) `div` 8 + 1) * 8 + 1
  in col > 0 ==> posColumn posAfterTab === expectedCol

-- | 测试空字符串的位置处理
prop_empty_string_position :: Property
prop_empty_string_position =
  let pos = advancePosByText "" startPos
  in pos === startPos

-- | 测试位置追踪的单步性
prop_position_tracking_step_by_step :: String -> Property
prop_position_tracking_step_by_step text =
  let pos1 = advancePosByText (T.pack text) startPos
      pos2 = foldl (flip advancePos) startPos text
  in pos1 === pos2

-- | 测试span的零长度特性
prop_zero_length_span :: SourcePos -> Property
prop_zero_length_span pos =
  let span = spanBetween pos pos
  in spanStart span === spanEnd span .&&.
     spanStart span === pos

-- | 测试LocationTracker的状态一致性
test_locationTracker_state_consistency :: Assertion
test_locationTracker_state_consistency = do
  let initialPos = SourcePos 1 1 0
      testPos = SourcePos 5 10 100
      actions = runLocationTracker $ do
        setCurrentPos testPos
        getCurrentPos
  assertEqual "Position should be set correctly" testPos actions

-- | 测试错误位置转换的完整性
test_errorLocation_completeness :: Assertion
test_errorLocation_completeness = do
  let start = SourcePos 10 5 50
      end = SourcePos 10 15 60
      span = SourceSpan start end
      errorLoc = toErrorLocationWithSpan span
  assertEqual "Error line should match span start" 10 (getErrorLine errorLoc)
  assertEqual "Error column should match span start" 5 (getErrorColumn errorLoc)

-- | 测试复杂文本的位置追踪
test_complex_text_position_tracking :: Assertion
test_complex_text_position_tracking = do
  let text = T.pack "hello\nworld\t\n\ttest"
      finalPos = advancePosByText text startPos
      expectedPos = SourcePos 3 6 20  -- 计算预期位置
  assertEqual "Complex text position should be tracked correctly" expectedPos finalPos

-- | 生成任意非空文本用于QuickCheck测试
instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary `suchThat` (/= "")

-- | 辅助函数：检查span是否包含位置
spanContains :: SourceSpan -> SourcePos -> Bool
spanContains span pos = 
  let start = spanStart span
      end = spanEnd span
  in sourcePosLe start pos && sourcePosLe pos end

-- | 辅助函数：检查SourcePos的顺序
sourcePosLe :: SourcePos -> SourcePos -> Bool
sourcePosLe (SourcePos l1 c1 _) (SourcePos l2 c2 _) = 
  l1 < l2 || (l1 == l2 && c1 <= c2)

-- | 测试套件
tests :: TestTree
tests = testGroup "SourceLocation Advanced QuickCheck Tests"
  [ testProperty "SourcePos offset consistent" prop_sourcePos_offset_consistent
  , testProperty "Multiline position tracking" prop_multiline_position_tracking
  , testProperty "Span contains monotonic" prop_span_contains_monotonic
  , testProperty "MergeSpans contains originals" prop_mergeSpans_contains_originals
  , testProperty "Located map preserves span" prop_located_map_preserves_span
  , testProperty "Position comparison transitive" prop_pos_comparison_transitive
  , testProperty "Tab position calculation" prop_tab_position_calculation
  , testProperty "Empty string position" prop_empty_string_position
  , testProperty "Position tracking step by step" prop_position_tracking_step_by_step
  , testProperty "Zero length span" prop_zero_length_span
  , testCase "LocationTracker state consistency" test_locationTracker_state_consistency
  , testCase "Error location completeness" test_errorLocation_completeness
  , testCase "Complex text position tracking" test_complex_text_position_tracking
  ]

-- | 为SourcePos添加Arbitrary实例
instance Arbitrary SourcePos where
  arbitrary = SourcePos <$> arbitrary <*> arbitrary <*> arbitrary

-- | 为SourceSpan添加Arbitrary实例
instance Arbitrary SourceSpan where
  arbitrary = SourceSpan <$> arbitrary <*> arbitrary