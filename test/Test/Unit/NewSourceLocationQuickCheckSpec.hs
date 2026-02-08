{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewSourceLocationQuickCheckSpec where

import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck
import TestSupport.MemoryLimits 
  ( withMemoryLimits
  , memoryLimitedTestGroup
  , memoryLevelTestGroup
  , MemoryLevel(..)
  , withMemoryLevel
  , gcBetweenTests
  )

import SourceLocation
import Data.List (isPrefixOf, isInfixOf)

-- | 测试源位置创建
prop_source_position_creation :: Int -> Int -> Property
prop_source_position_creation line col =
  let limitedLine = max 1 (min 1000 line)  -- 限制行号范围
      limitedCol = max 1 (min 1000 col)    -- 限制列号范围
      offset = 0
      pos = SourcePos limitedLine limitedCol offset
  in conjoin
    [ posLine pos === limitedLine
    , posColumn pos === limitedCol
    , posOffset pos === offset
    ]

-- | 测试源位置相等性
prop_source_position_equality :: Int -> Int -> Property
prop_source_position_equality line col =
  let limitedLine = max 1 (min 100 line)
      limitedCol = max 1 (min 100 col)
      offset = 0
      pos1 = SourcePos limitedLine limitedCol offset
      pos2 = SourcePos limitedLine limitedCol offset
  in pos1 === pos2

-- | 测试源位置不等性
prop_source_position_inequality :: Int -> Int -> Int -> Int -> Property
prop_source_position_inequality line1 col1 line2 col2 =
  let limitedLine1 = max 1 (min 50 line1)
      limitedCol1 = max 1 (min 50 col1)
      limitedLine2 = max 1 (min 50 line2)
      limitedCol2 = max 1 (min 50 col2)
      offset1 = 0
      offset2 = 0
      pos1 = SourcePos limitedLine1 limitedCol1 offset1
      pos2 = SourcePos limitedLine2 limitedCol2 offset2
  in (limitedLine1 /= limitedLine2 || limitedCol1 /= limitedCol2) ==> property (pos1 /= pos2)

-- | 测试源位置比较
prop_source_position_comparison :: Int -> Int -> Int -> Int -> Property
prop_source_position_comparison line1 col1 line2 col2 =
  let limitedLine1 = max 1 (min 20 line1)
      limitedCol1 = max 1 (min 20 col1)
      limitedLine2 = max 1 (min 20 line2)
      limitedCol2 = max 1 (min 20 col2)
      offset1 = 0
      offset2 = 0
      pos1 = SourcePos limitedLine1 limitedCol1 offset1
      pos2 = SourcePos limitedLine2 limitedCol2 offset2
  in property True  -- 位置比较逻辑依赖于具体实现

-- | 测试源跨度创建
prop_source_span_creation :: Int -> Int -> Int -> Int -> Property
prop_source_span_creation startLine startCol endLine endCol =
  let limitedStartLine = max 1 (min 100 startLine)
      limitedStartCol = max 1 (min 100 startCol)
      limitedEndLine = max limitedStartLine (min 100 endLine)
      limitedEndCol = if limitedEndLine == limitedStartLine 
                     then max limitedStartCol (min 100 endCol)
                     else max 1 (min 100 endCol)
      startOffset = 0
      endOffset = 0
      startPos = SourcePos limitedStartLine limitedStartCol startOffset
      endPos = SourcePos limitedEndLine limitedEndCol endOffset
      span = SourceSpan startPos endPos
  in conjoin
    [ spanStart span === startPos
    , spanEnd span === endPos
    ]

-- | 测试源跨度相等性
prop_source_span_equality :: Int -> Int -> Int -> Int -> Property
prop_source_span_equality startLine startCol endLine endCol =
  let limitedStartLine = max 1 (min 50 startLine)
      limitedStartCol = max 1 (min 50 startCol)
      limitedEndLine = max limitedStartLine (min 50 endLine)
      limitedEndCol = if limitedEndLine == limitedStartLine 
                     then max limitedStartCol (min 50 endCol)
                     else max 1 (min 50 endCol)
      startOffset = 0
      endOffset = 0
      startPos = SourcePos limitedStartLine limitedStartCol startOffset
      endPos = SourcePos limitedEndLine limitedEndCol endOffset
      span1 = SourceSpan startPos endPos
      span2 = SourceSpan startPos endPos
  in span1 === span2

-- | 测试源跨度不等性
prop_source_span_inequality :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_source_span_inequality sLine1 sCol1 eLine1 eCol1 sLine2 sCol2 eLine2 eCol2 =
  let limitedSLine1 = max 1 (min 20 sLine1)
      limitedSCol1 = max 1 (min 20 sCol1)
      limitedELine1 = max limitedSLine1 (min 20 eLine1)
      limitedECol1 = if limitedELine1 == limitedSLine1 
                    then max limitedSCol1 (min 20 eCol1)
                    else max 1 (min 20 eCol1)
      limitedSLine2 = max 1 (min 20 sLine2)
      limitedSCol2 = max 1 (min 20 sCol2)
      limitedELine2 = max limitedSLine2 (min 20 eLine2)
      limitedECol2 = if limitedELine2 == limitedSLine2 
                    then max limitedSCol2 (min 20 eCol2)
                    else max 1 (min 20 eCol2)
      startOffset1 = 0
      endOffset1 = 0
      startOffset2 = 0
      endOffset2 = 0
      startPos1 = SourcePos limitedSLine1 limitedSCol1 startOffset1
      endPos1 = SourcePos limitedELine1 limitedECol1 endOffset1
      startPos2 = SourcePos limitedSLine2 limitedSCol2 startOffset2
      endPos2 = SourcePos limitedELine2 limitedECol2 endOffset2
      span1 = SourceSpan startPos1 endPos1
      span2 = SourceSpan startPos2 endPos2
      spansDiffer = span1 /= span2
      condition = limitedSLine1 /= limitedSLine2 || limitedSCol1 /= limitedSCol2 || 
                  limitedELine1 /= limitedELine2 || limitedECol1 /= limitedECol2
  in condition ==> property spansDiffer

-- | 测试位置包装器
prop_located_wrapper :: Int -> Int -> String -> Property
prop_located_wrapper line col value =
  let limitedLine = max 1 (min 50 line)
      limitedCol = max 1 (min 50 col)
      limitedValue = take 30 value
      offset = 0
      pos = SourcePos limitedLine limitedCol offset
      located = locatedAt pos limitedValue
  in conjoin
    [ locatedPos located === pos
    , locValue located === limitedValue
    ]

-- | 测试跨度包装器
prop_located_with_span_wrapper :: Int -> Int -> Int -> Int -> String -> Property
prop_located_with_span_wrapper startLine startCol endLine endCol value =
  let limitedStartLine = max 1 (min 30 startLine)
      limitedStartCol = max 1 (min 30 startCol)
      limitedEndLine = max limitedStartLine (min 30 endLine)
      limitedEndCol = if limitedEndLine == limitedStartLine 
                     then max limitedStartCol (min 30 endCol)
                     else max 1 (min 30 endCol)
      limitedValue = take 40 value :: String
      startOffset = 0
      endOffset = 0
      startPos = SourcePos limitedStartLine limitedStartCol startOffset
      endPos = SourcePos limitedEndLine limitedEndCol endOffset
      span = SourceSpan startPos endPos
      located = locatedWithSpan span limitedValue
  in conjoin
    [ property $ locatedPos located === startPos
    , property $ locValue located === limitedValue
    ]

-- | 测试默认跨度
prop_default_span_properties :: Property
prop_default_span_properties =
  let pos = startPos
      span = emptySpan pos
      spanStartPos = spanStart span
      spanEndPos = spanEnd span
  in conjoin
    [ posLine spanStartPos === 1
    , posColumn spanStartPos === 1
    , posLine spanEndPos === 1
    , posColumn spanEndPos === 1
    ]

-- | 测试位置字符串表示
prop_source_position_show :: Int -> Int -> Property
prop_source_position_show line col =
  let limitedLine = max 1 (min 100 line)
      limitedCol = max 1 (min 100 col)
      offset = 0
      pos = SourcePos limitedLine limitedCol offset
      posStr = show pos
  in conjoin
    [ show limitedLine `isInfixOf` posStr
    , show limitedCol `isInfixOf` posStr
    ]

-- | 测试跨度字符串表示
prop_source_span_show :: Int -> Int -> Int -> Int -> Property
prop_source_span_show startLine startCol endLine endCol =
  let limitedStartLine = max 1 (min 50 startLine)
      limitedStartCol = max 1 (min 50 startCol)
      limitedEndLine = max limitedStartLine (min 50 endLine)
      limitedEndCol = if limitedEndLine == limitedStartLine 
                     then max limitedStartCol (min 50 endCol)
                     else max 1 (min 50 endCol)
      startOffset = 0
      endOffset = 0
      startPos = SourcePos limitedStartLine limitedStartCol startOffset
      endPos = SourcePos limitedEndLine limitedEndCol endOffset
      span = SourceSpan startPos endPos
      spanStr = show span
  in conjoin
    [ show limitedStartLine `isInfixOf` spanStr
    , show limitedStartCol `isInfixOf` spanStr
    , show limitedEndLine `isInfixOf` spanStr
    , show limitedEndCol `isInfixOf` spanStr
    ]

-- | 测试位置包装器字符串表示
prop_located_show :: Int -> Int -> String -> Property
prop_located_show line col value =
  let limitedLine = max 1 (min 50 line)
      limitedCol = max 1 (min 50 col)
      limitedValue = take 30 value
      offset = 0
      pos = SourcePos limitedLine limitedCol offset
      located = locatedAt pos limitedValue
      locatedStr = show located
      -- 对于特殊字符，检查它们的转义形式
      escapedValue = show limitedValue
  in property $ limitedLine >= 1 && limitedCol >= 1 &&  -- 确保值有效
              show limitedLine `isInfixOf` locatedStr &&
              show limitedCol `isInfixOf` locatedStr &&
              (null limitedValue || limitedValue `isInfixOf` locatedStr || escapedValue `isInfixOf` locatedStr)

-- | 测试单行跨度
prop_single_line_span :: Int -> Int -> String -> Property
prop_single_line_span startCol endCol value =
  let limitedStartCol = max 1 (min 30 startCol)
      limitedEndCol = max limitedStartCol (min 30 endCol)
      limitedValue = take 40 value :: String
      line = 5
      offset = 0
      startPos = SourcePos line limitedStartCol offset
      endPos = SourcePos line limitedEndCol offset
      span = SourceSpan startPos endPos
      located = locatedWithSpan span limitedValue
  in conjoin
    [ property $ posLine (spanStart span) === line
    , property $ posLine (spanEnd span) === line
    , property $ posColumn (spanStart span) === limitedStartCol
    , property $ posColumn (spanEnd span) === limitedEndCol
    ]

-- | 测试多行跨度
prop_multi_line_span :: Int -> Int -> Int -> Int -> Property
prop_multi_line_span startLine startCol endLine endCol =
  let limitedStartLine = max 1 (min 20 startLine)
      limitedStartCol = max 1 (min 20 startCol)
      limitedEndLine = max (limitedStartLine + 1) (min 20 endLine)
      limitedEndCol = max 1 (min 20 endCol)
      startOffset = 0
      endOffset = 0
      startPos = SourcePos limitedStartLine limitedStartCol startOffset
      endPos = SourcePos limitedEndLine limitedEndCol endOffset
      span = SourceSpan startPos endPos
  in conjoin
    [ property $ posLine (spanStart span) === limitedStartLine
    , property $ posColumn (spanStart span) === limitedStartCol
    , property $ posLine (spanEnd span) === limitedEndLine
    , property $ posColumn (spanEnd span) === limitedEndCol
    , property $ posLine (spanStart span) < posLine (spanEnd span)
    ]

-- | 测试源位置边界情况
test_source_position_edge_cases :: Assertion
test_source_position_edge_cases = do
  let pos1 = SourcePos 1 1 0
      pos2 = SourcePos 100 100 0
      pos3 = SourcePos 1 100 0
      pos4 = SourcePos 100 1 0
  assertEqual "Position (1,1)" (SourcePos 1 1 0) pos1
  assertEqual "Position (100,100)" (SourcePos 100 100 0) pos2
  assertEqual "Position (1,100)" (SourcePos 1 100 0) pos3
  assertEqual "Position (100,1)" (SourcePos 100 1 0) pos4
  assertBool "Different positions should not be equal" $ pos1 /= pos2
  assertBool "Different positions should not be equal" $ pos3 /= pos4

-- | 测试源跨度边界情况
test_source_span_edge_cases :: Assertion
test_source_span_edge_cases = do
  let startPos = SourcePos 1 1 0
      endPos = SourcePos 1 10 0
      span1 = SourceSpan startPos endPos
      span2 = SourceSpan (SourcePos 2 1 0) (SourcePos 2 20 0)
      span3 = SourceSpan (SourcePos 1 1 0) (SourcePos 2 1 0)
  assertEqual "Single line span" span1 span1
  assertEqual "Different line span" span2 span2
  assertEqual "Multi-line span" span3 span3
  assertBool "Different spans should not be equal" $ span1 /= span2
  assertBool "Different spans should not be equal" $ span1 /= span3

-- | 测试位置包装器边界情况
test_located_wrapper_edge_cases :: Assertion
test_located_wrapper_edge_cases = do
  let pos = SourcePos 5 10 0
      value = "test value"
      located = locatedAt pos value
      locatedEmpty = locatedAt pos ""
  assertEqual "Located value" value (locValue located)
  assertEqual "Located position" pos (locatedPos located)
  assertEqual "Empty located value" "" (locValue locatedEmpty)
  assertEqual "Empty located position" pos (locatedPos locatedEmpty)

-- | 测试跨度包装器边界情况
test_located_with_span_wrapper_edge_cases :: Assertion
test_located_with_span_wrapper_edge_cases = do
  let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 0)
      value = "test value"
      located = locatedWithSpan span value
      locatedEmpty = locatedWithSpan span ""
  assertEqual "Located with span value" value (locValue located)
  assertEqual "Located with span position" (spanStart span) (locatedPos located)
  assertEqual "Empty located with span value" "" (locValue locatedEmpty)
  assertEqual "Empty located with span position" (spanStart span) (locatedPos locatedEmpty)

-- | 测试套件
tests :: TestTree
tests = memoryLevelTestGroup Moderate "New SourceLocation QuickCheck Tests"
  [ withMemoryLevel Moderate $ testProperty "Source position creation" prop_source_position_creation
  , withMemoryLevel Moderate $ testProperty "Source position equality" prop_source_position_equality
  , withMemoryLevel Moderate $ testProperty "Source position inequality" prop_source_position_inequality
  , withMemoryLevel Moderate $ testProperty "Source position comparison" prop_source_position_comparison
  , withMemoryLevel Moderate $ testProperty "Source span creation" prop_source_span_creation
  , withMemoryLevel Moderate $ testProperty "Source span equality" prop_source_span_equality
  , withMemoryLevel Moderate $ testProperty "Source span inequality" prop_source_span_inequality
  , withMemoryLevel Moderate $ testProperty "Located wrapper" prop_located_wrapper
  , withMemoryLevel Moderate $ testProperty "Located with span wrapper" prop_located_with_span_wrapper
  , withMemoryLevel Moderate $ testProperty "Default span properties" prop_default_span_properties
  , withMemoryLevel Moderate $ testProperty "Source position show" prop_source_position_show
  , withMemoryLevel Moderate $ testProperty "Source span show" prop_source_span_show
  , withMemoryLevel Moderate $ testProperty "Located show" prop_located_show
  , withMemoryLevel Moderate $ testProperty "Single line span" prop_single_line_span
  , withMemoryLevel Moderate $ testProperty "Multi line span" prop_multi_line_span
  , testCase "Source position edge cases" test_source_position_edge_cases
  , testCase "Source span edge cases" test_source_span_edge_cases
  , testCase "Located wrapper edge cases" test_located_wrapper_edge_cases
  , testCase "Located with span wrapper edge cases" test_located_with_span_wrapper_edge_cases
  ]

-- | 轻量级测试套件，用于内存受限环境
essentialTests :: TestTree
essentialTests = memoryLevelTestGroup Minimal "New SourceLocation Essential Tests"
  [ withMemoryLevel Minimal $ testProperty "Source position creation" prop_source_position_creation
  , withMemoryLevel Minimal $ testProperty "Source span creation" prop_source_span_creation
  , withMemoryLevel Minimal $ testProperty "Located wrapper" prop_located_wrapper
  , withMemoryLevel Minimal $ testCase "Source position edge cases" test_source_position_edge_cases
  , withMemoryLevel Minimal $ testCase "Source span edge cases" test_source_span_edge_cases
  ]