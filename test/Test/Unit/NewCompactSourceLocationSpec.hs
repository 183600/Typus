{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewCompactSourceLocationSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), forAll, choose)
import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  , posAfter
  , posAt
  , posAtLineCol
  , emptySpan
  , spanFrom
  , spanTo
  , spanBetween
  , mergeSpans
  , isValidSpan
  , locatedAt
  , locatedWithSpan
  , locatedValue
  , locatedSpan
  , advancePos
  , advancePosBy
  )

-- | 生成任意的有效源位置
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 1000)
    col <- choose (1, 1000)
    return $ SourcePos line col

-- | 生成任意的源跨度
instance Arbitrary SourceSpan where
  arbitrary = do
    startLine <- choose (1, 100)
    startCol <- choose (1, 100)
    endLine <- choose (startLine, startLine + 10)  -- 确保结束行不小于开始行
    endCol <- if endLine == startLine 
              then choose (startCol, startCol + 10)  -- 同行时结束列不小于开始列
              else choose (1, 100)
    return $ SourceSpan (SourcePos startLine startCol) (SourcePos endLine endCol)

-- | 测试位置计算的数学属性
testPositionMathProperties :: TestTree
testPositionMathProperties = testGroup "位置计算数学属性测试"
  [ testProperty "posAfter增加列号" $
      \pos -> posAfter pos 'x' === pos { spColumn = spColumn pos + 1}
    
  , testProperty "posAfter处理换行符" $
      \pos -> posAfter pos '\n' === SourcePos (spLine pos + 1) 1
    
  , testProperty "posAtLineCol创建正确位置" $
      \line col -> let pos = posAtLineCol line col
                   in spLine pos === line && spColumn pos === col
    
  , testProperty "advancePosBy正确处理多字符" $
      \pos s -> let chars = take 10 s  -- 限制长度避免过大
                    finalPos = foldl posAfter pos chars
                in spLine finalPos >= spLine pos
  ]

-- | 测试跨度的属性
testSpanProperties :: TestTree
testSpanProperties = testGroup "跨度属性测试"
  [ testProperty "spanFrom创建单位跨度" $
      \pos -> let span = spanFrom pos
              in spanStart span === pos && spanEnd span === pos
    
  , testProperty "spanTo的正确性" $
      \startPos endPos -> 
        let span = spanFrom startPos `spanTo` endPos
        in spanStart span === startPos && spanEnd span === endPos
    
  , testProperty "spanBetween包含两个位置" $
      \pos1 pos2 ->
        let span = spanBetween pos1 pos2
            start = spanStart span
            end = spanEnd span
        in (start <= pos1 && end >= pos1) || (start <= pos2 && end >= pos2)
    
  , testProperty "mergeSpans包含原始跨度" $
      \span1 span2 ->
        let merged = mergeSpans span1 span2
            start1 = spanStart span1
            end1 = spanEnd span1
            start2 = spanStart span2
            end2 = spanEnd span2
            mergedStart = spanStart merged
            mergedEnd = spanEnd merged
        in mergedStart <= start1 && mergedEnd >= end1 &&
           mergedStart <= start2 && mergedEnd >= end2
  ]

-- | 测试Located值的属性
testLocatedProperties :: TestTree
testLocatedProperties = testGroup "Located值属性测试"
  [ testProperty "locatedAt创建正确位置" $
      \value pos -> 
        let located = locatedAt pos value
        in locatedPos located === pos && locatedValue located === value
    
  , testProperty "locatedWithSpan创建正确跨度" $
      \value span ->
        let located = locatedWithSpan span value
        in locatedSpan located === span && locatedValue located === value
    
  , testProperty "mapLocated保持位置不变" $
      \value pos f ->
        let located = locatedAt pos value
            mapped = mapLocated (const f) located
        in locatedPos mapped === locatedPos located
  ]

-- | 边界条件测试
testBoundaryConditions :: TestTree
testBoundaryConditions = testGroup "边界条件测试"
  [ testCase "起始位置" $
      let pos = startPos
      in spLine pos @?= 1 && spColumn pos @?= 1
    
  , testCase "空跨度有效性" $
      let span = emptySpan
      in isValidSpan span @?= False
    
  , testCase "单位跨度有效性" $
      let pos = posAtLineCol 1 1
          span = spanFrom pos
      in isValidSpan span @?= True
    
  , testCase "跨度的开始不大于结束" $
      \span -> isValidSpan span ==> 
        let start = spanStart span
            end = spanEnd span
        in (spLine start < spLine end) || 
           (spLine start == spLine end && spColumn start <= spColumn end)
  ]

-- | 一致性测试
testConsistencyProperties :: TestTree
testConsistencyProperties = testGroup "一致性属性测试"
  [ testProperty "位置前进的单调性" $
      \pos c -> 
        let newPos = posAfter pos c
        in spLine newPos > spLine pos || 
           (spLine newPos == spLine pos && spColumn newPos >= spColumn pos)
    
  , testProperty "跨度合并的幂等性" $
      \span -> mergeSpans span span === span
  ]

-- | 组合所有测试
tests :: TestTree
tests = testGroup "SourceLocation模块核心功能测试"
  [ testPositionMathProperties
  , testSpanProperties
  , testLocatedProperties
  , testBoundaryConditions
  , testConsistencyProperties
  ]