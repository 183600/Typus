{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Test.Unit.NewSourceLocationMathPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), counterexample)

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , startPos
  , posAfter
  , posAt
  , emptySpan
  , spanFrom
  , spanTo
  , mergeSpans
  , isValidSpan
  , spanStart
  , spanEnd
  )

-- | 生成任意的 SourcePos 用于测试
instance Arbitrary SourcePos where
  arbitrary = SourcePos <$> arbitrary <*> arbitrary <*> arbitrary

-- | 生成任意的 SourceSpan 用于测试
instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    -- 确保 end 的位置 >= start 的位置
    let end' = if posLine end < posLine start || 
                   (posLine end == posLine start && posColumn end < posColumn start)
                 then start { posColumn = posColumn start + 1 }
                 else end
    return $ SourceSpan start end'

-- | 生成小的非负整数用于行列位置
smallNat :: Gen Int
smallNat = getSmall <$> arbitrary

tests :: TestTree
tests = testGroup "New SourceLocation Math Properties Tests"
  [ -- 单元测试
    testPositionMath
  , testSpanMath
  , testSpanValidation
  , testEdgeCases
    -- QuickCheck 属性测试
  , testPositionProperties
  , testSpanProperties
  , testMergeProperties
  ]

-- | 测试位置计算的单元测试
testPositionMath :: TestTree
testPositionMath = testCase "position math unit tests" $ do
  let start = startPos "test.txt"
  
  -- 测试 posAfter
  let pos1 = posAfter start 'a'
  assertEqual "posAfter moves to next column" 
    (start { posColumn = 2 }) pos1
  
  let pos2 = posAfter pos1 '\n'
  assertEqual "posAfter newline moves to next line"
    (SourcePos "test.txt" 2 1) pos2
  
  -- 测试 posAt
  let pos3 = posAt "test.txt" 5 10
  assertEqual "posAt creates correct position"
    (SourcePos "test.txt" 5 10) pos3

-- | 测试范围计算的单元测试
testSpanMath :: TestTree
testSpanMath = testCase "span math unit tests" $ do
  let start = posAt "test.txt" 1 1
  let middle = posAt "test.txt" 1 5
  let end = posAt "test.txt" 2 3
  
  -- 测试 spanFrom
  let span1 = spanFrom start
  assertEqual "spanFrom creates zero-length span"
    (SourceSpan start start) span1
  
  -- 测试 spanTo
  let span2 = spanTo end
  assertEqual "spanTo creates span from startPos"
    (SourceSpan (startPos "test.txt") end) span2
  
  -- 测试 mergeSpans
  let spanA = SourceSpan start middle
  let spanB = SourceSpan middle end
  let merged = mergeSpans spanA spanB
  assertEqual "mergeSpans combines spans correctly"
    (SourceSpan start end) merged

-- | 测试范围验证
testSpanValidation :: TestTree
testSpanValidation = testCase "span validation tests" $ do
  let start = posAt "test.txt" 1 1
  let end = posAt "test.txt" 1 5
  let validSpan = SourceSpan start end
  
  assertBool "valid span passes validation" (isValidSpan validSpan)
  
  -- 测试无效范围（end < start）
  let invalidSpan = SourceSpan end start
  assertBool "invalid span fails validation" (not $ isValidSpan invalidSpan)
  
  -- 测试空范围
  let empty = emptySpan
  assertBool "empty span is valid" (isValidSpan empty)

-- | 测试边界情况
testEdgeCases :: TestTree
testEdgeCases = testCase "edge cases" $ do
  -- 测试相同位置的span
  let pos = posAt "test.txt" 1 1
  let samePosSpan = SourceSpan pos pos
  assertBool "same position span is valid" (isValidSpan samePosSpan)
  assertEqual "same position span has same start and end" pos (spanStart samePosSpan)
  assertEqual "same position span has same start and end" pos (spanEnd samePosSpan)
  
  -- 测试跨行span
  let line1 = posAt "test.txt" 1 10
  let line2 = posAt "test.txt" 2 1
  let crossLineSpan = SourceSpan line1 line2
  assertBool "cross-line span is valid" (isValidSpan crossLineSpan)
  
  -- 测试大行列数
  let bigPos = posAt "test.txt" 999999 999999
  let bigSpan = SourceSpan pos bigPos
  assertBool "large position span is valid" (isValidSpan bigSpan)

-- | QuickCheck 属性：位置递增应该保持单调性
testPositionProperties :: TestTree
testPositionProperties = testGroup "Position Properties"
  [ testProperty "posAfter preserves file" $ \pos char ->
      let newPos = posAfter pos char
      in posFile pos === posFile newPos
      
  , testProperty "posAfter newline resets column" $ \pos ->
      let posAfterNewline = posAfter pos '\n'
      in posColumn posAfterNewline === 1
      
  , testProperty "posAfter newline increments line" $ \pos ->
      let posAfterNewline = posAfter pos '\n'
      in posLine posAfterNewline === posLine pos + 1
      
  , testProperty "posAfter non-newline increments column" $ \pos char ->
      let newPos = posAfter pos char
      in if char /= '\n'
         then posColumn newPos === posColumn pos + 1
         else property True
  ]

-- | QuickCheck 属性：范围合并的交换律
testSpanProperties :: TestTree
testSpanProperties = testGroup "Span Properties"
  [ testProperty "mergeSpans is commutative" $ \span1 span2 ->
      let merged1 = mergeSpans span1 span2
          merged2 = mergeSpans span2 span1
      in merged1 === merged2
      
  , testProperty "mergeSpans is associative" $ \span1 span2 span3 ->
      let merged1 = mergeSpans (mergeSpans span1 span2) span3
          merged2 = mergeSpans span1 (mergeSpans span2 span3)
      in merged1 === merged2
      
  , testProperty "mergeSpans contains both spans" $ \span1 span2 ->
      let merged = mergeSpans span1 span2
          start1 = spanStart span1
          end1 = spanEnd span1
          start2 = spanStart span2
          end2 = spanEnd span2
          mergedStart = spanStart merged
          mergedEnd = spanEnd merged
      in (mergedStart `isBeforeOrEqual` start1 && end1 `isBeforeOrEqual` mergedEnd &&
          mergedStart `isBeforeOrEqual` start2 && end2 `isBeforeOrEqual` mergedEnd)
  ]

-- | QuickCheck 属性：范围合并的边界情况
testMergeProperties :: TestTree
testMergeProperties = testGroup "Merge Properties"
  [ testProperty "merge with empty span" $ \span ->
      let empty = emptySpan
          merged1 = mergeSpans span empty
          merged2 = mergeSpans empty span
      in merged1 === span && merged2 === span
      
  , testProperty "merge with self returns self" $ \span ->
      let merged = mergeSpans span span
      in merged === span
  ]

-- | 辅助函数：检查位置顺序
isBeforeOrEqual :: SourcePos -> SourcePos -> Bool
isBeforeOrEqual pos1 pos2 =
  let line1 = posLine pos1
      line2 = posLine pos2
      col1 = posColumn pos1
      col2 = posColumn pos2
  in if line1 < line2
     then True
     else if line1 > line2
          then False
          else col1 <= col2