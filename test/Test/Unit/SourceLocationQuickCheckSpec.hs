{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.SourceLocationQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck (property, testProperty, Property)
import Test.Tasty.HUnit
import TestSupport.MemoryLimits 
  ( withMemoryLimits
  , memoryLimitedTestGroup
  , memoryLevelTestGroup
  , MemoryLevel(..)
  , withMemoryLevel
  , gcBetweenTests
  )
import TestSupport.EnhancedMemoryOptimization 
  ( enhancedMemoryCleanup
  , strategicMemoryCleanup
  , cleanupBetweenTests
  , withEnhancedMemoryControl
  , withStrictMemoryLimits
  , applyMemoryOptimizations
  )
import TestSupport.OptimizedStringOperations 
  ( genMinimalString
  , genUltraMinimalString
  , safeTake
  , safeLength
  , efficientTrim
  , efficientIsEmpty
  , withUltraStringLimit
  , minimizeStringUsage
  , optimizeStringProperty
  )
import TestSupport.TestPropertyMemoryCleanup 
  ( testGroupWithCleanup
  , testGroupWithStrategicCleanup
  , memoryAwareProperty
  , memoryOptimizedProperty
  , withPropertyMemoryCleanup
  )

import SourceLocation
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.Char (isSpace, isDigit)
import Data.Either (isLeft, isRight)
import Data.Maybe (isJust, isNothing)

-- | 使用SourceLocation模块中的SourcePos类型
type SourcePosType = SourcePos

-- | 简单的错误类型，用于测试
data ErrorWithLocation = ErrorWithLocation String SourcePosType
  deriving (Show, Eq)

-- | 源码位置范围数据类型（基于SourceSpan）
type SourceLocationRange = SourceSpan

-- | 测试源码位置的基本属性
prop_source_location_basic :: Int -> Int -> Property
prop_source_location_basic line col =
  let loc = posAt line col
  in property $ 
    (sourceLine loc == line) && 
    (sourceColumn loc == col)

-- | 测试源码位置的行号准确性
prop_source_location_line_accuracy :: Int -> Property
prop_source_location_line_accuracy line =
  let loc = posAt line 1
  in property $ sourceLine loc == line

-- | 测试源码位置的列号准确性
prop_source_location_column_accuracy :: Int -> Property
prop_source_location_column_accuracy col =
  let loc = posAt 1 col
  in property $ sourceColumn loc == col

-- | 测试源码位置范围的有效性
prop_source_location_range_validity :: Int -> Int -> Int -> Int -> Property
prop_source_location_range_validity line1 col1 line2 col2 =
  let loc1 = posAt line1 col1
      loc2 = posAt line2 col2
      range = spanBetween loc1 loc2
  in property $ isValidSpan range

-- | 测试源码位置的字符串表示
prop_source_location_string_representation :: Int -> Int -> Property
prop_source_location_string_representation line col =
  let loc = posAt line col
      str = show loc
  in property $ (show line) `isInfixOf` str && (show col) `isInfixOf` str

-- | 测试源码位置的比较
prop_source_location_comparison :: Int -> Int -> Int -> Int -> Property
prop_source_location_comparison line1 col1 line2 col2 =
  let loc1 = posAt line1 col1
      loc2 = posAt line2 col2
  in property $ comparePos loc1 loc2 == 
    if line1 < line2 || (line1 == line2 && col1 < col2)
    then LT
    else if line1 == line2 && col1 == col2
         then EQ
         else GT

-- | 测试源码位置的合并
prop_source_location_merge :: Int -> Int -> Int -> Int -> Property
prop_source_location_merge line1 col1 line2 col2 =
  let loc1 = posAt line1 col1
      loc2 = posAt line2 col2
      span1 = spanFrom loc1
      span2 = spanFrom loc2
      merged = mergeSpans span1 span2
  in property $ isValidSpan merged

-- | 测试源码位置的扩展
prop_source_location_extend :: Int -> Int -> Int -> Int -> Property
prop_source_location_extend line1 col1 line2 col2 =
  let loc1 = posAt line1 col1
      loc2 = posAt line2 col2
      span1 = spanFrom loc1
      span2 = spanFrom loc2
      extended = mergeSpans span1 span2
      startLoc = spanStart extended
      endLoc = spanEnd extended
  in property $ 
    (sourceLine startLoc == min line1 line2) && 
    (sourceColumn startLoc == min col1 col2) &&
    (sourceLine endLoc == max line1 line2) && 
    (sourceColumn endLoc == max col1 col2)

-- | 测试源码位置的偏移
prop_source_location_offset :: Int -> Int -> Int -> Int -> Property
prop_source_location_offset line col lineOffset colOffset =
  let newLine = line + lineOffset
      newCol = col + colOffset
      -- 确保行号和列号为正数
      validLine = max 1 newLine
      validCol = max 1 newCol
      loc = posAt line col
      offsetLoc = posAt validLine validCol
  in property $ 
    (sourceLine offsetLoc == validLine) && 
    (sourceColumn offsetLoc == validCol)

-- | 测试源码位置与错误的关联
prop_source_location_error_association :: String -> Int -> Int -> Property
prop_source_location_error_association errMsg line col =
  let loc :: SourcePosType
      loc = posAt line col
      errorWithLoc = ErrorWithLocation errMsg loc
  in property $ 
    (errorMessage errorWithLoc == errMsg) && 
    (errorLocation errorWithLoc == loc)
  where
    errorMessage (ErrorWithLocation msg _) = msg
    errorLocation (ErrorWithLocation _ loc) = loc

-- | 单元测试：源码位置的边界情况
test_source_location_edge_cases :: Assertion
test_source_location_edge_cases = do
  let start :: SourcePos
      start = startPos
  assertEqual "Start position" start start
  assertEqual "Position at 1,1" (posAt 1 1) (posAt 1 1)
  assertEqual "Position at 1000,1000" (posAt 1000 1000) (posAt 1000 1000)

-- | 单元测试：复杂表达式的源码位置
test_source_location_complex_expressions :: Assertion
test_source_location_complex_expressions = do
  let loc1 = posAt 10 20
      loc2 = posAt 15 25
      range = spanBetween loc1 loc2
  assertEqual "Range start location" loc1 (spanStart range)
  assertEqual "Range end location" loc2 (spanEnd range)

-- | IR类型定义
data IR = IR String deriving (Eq, Show)

-- | 源码位置测试套件
tests :: TestTree
tests = testGroupWithStrategicCleanup "Source Location QuickCheck Tests"
  [ -- 基本位置测试
    memoryOptimizedProperty "Source location basic" (property prop_source_location_basic)
  , memoryOptimizedProperty "Source location line accuracy" (property prop_source_location_line_accuracy)
  , memoryOptimizedProperty "Source location column accuracy" (property prop_source_location_column_accuracy)
  
  -- 位置范围测试
  , memoryOptimizedProperty "Source location range validity" (property prop_source_location_range_validity)
  , memoryOptimizedProperty "Source location string representation" (property prop_source_location_string_representation)
  
  -- 位置操作测试
  , memoryOptimizedProperty "Source location comparison" (property prop_source_location_comparison)
  , memoryOptimizedProperty "Source location merge" (property prop_source_location_merge)
  , memoryOptimizedProperty "Source location extend" (property prop_source_location_extend)
  , memoryOptimizedProperty "Source location offset" (property prop_source_location_offset)
  
  -- 错误关联测试
  , memoryOptimizedProperty "Source location error association" (property prop_source_location_error_association)
  
  -- 单元测试
  , testCase "Source location edge cases" test_source_location_edge_cases
  , testCase "Source location complex expressions" test_source_location_complex_expressions
  ]

-- | 内存优化的测试套件
memoryOptimizedTests :: TestTree
memoryOptimizedTests = memoryLevelTestGroup Minimal "Source Location Memory Optimized Tests"
  [ testProperty "Source location basic" prop_source_location_basic
  , testProperty "Source location line accuracy" prop_source_location_line_accuracy
  , testProperty "Source location column accuracy" prop_source_location_column_accuracy
  , testProperty "Source location range validity" prop_source_location_range_validity
  , testProperty "Source location error association" prop_source_location_error_association
  ]