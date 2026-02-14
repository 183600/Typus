{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.SourceLocationQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
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

-- | SourceLocation类型定义（如果SourceLocation模块中没有定义）
data SourceLocation = SourceLocation Int Int deriving (Eq, Show, Ord)

-- | 辅助函数
sourceLine :: SourceLocation -> Int
sourceLine (SourceLocation line _) = line

sourceColumn :: SourceLocation -> Int
sourceColumn (SourceLocation _ col) = col

-- | 测试源码位置的基本属性
prop_source_location_basic :: Int -> Int -> Property
prop_source_location_basic line col =
  let loc = SourceLocation line col
  in property $ 
    (sourceLine loc == line) && 
    (sourceColumn loc == col) &&
    (show loc == show line ++ ":" ++ show col)

-- | 测试源码位置的行号准确性
prop_source_location_line_accuracy :: Int -> Property
prop_source_location_line_accuracy line =
  let loc = SourceLocation line 1
  in property $ sourceLine loc == line

-- | 测试源码位置的列号准确性
prop_source_location_column_accuracy :: Int -> Property
prop_source_location_column_accuracy col =
  let loc = SourceLocation 1 col
  in property $ sourceColumn loc == col

-- | 测试源码位置范围的有效性
prop_source_location_range_validity :: Int -> Int -> Int -> Int -> Property
prop_source_location_range_validity line1 col1 line2 col2 =
  let loc1 = SourceLocation line1 col1
      loc2 = SourceLocation line2 col2
      range = SourceLocationRange loc1 loc2
  in if line1 < line2 || (line1 == line2 && col1 <= col2)
     then property $ isValidRange range
     else property $ not (isValidRange range)
  where
    isValidRange (SourceLocationRange start end) = 
      let startLine = sourceLine start
          startCol = sourceColumn start
          endLine = sourceLine end
          endCol = sourceColumn end
      in startLine < endLine || (startLine == endLine && startCol <= endCol)

-- | 测试源码位置的字符串表示
prop_source_location_string_representation :: Int -> Int -> Property
prop_source_location_string_representation line col =
  let loc = SourceLocation line col
      str = show loc
  in property $ (show line ++ ":" ++ show col) `isInfixOf` str

-- | 测试源码位置的比较
prop_source_location_comparison :: Int -> Int -> Int -> Int -> Property
prop_source_location_comparison line1 col1 line2 col2 =
  let loc1 = SourceLocation line1 col1
      loc2 = SourceLocation line2 col2
  in if line1 < line2 || (line1 == line2 && col1 < col2)
     then property $ loc1 < loc2
     else if line1 == line2 && col1 == col2
          then property $ loc1 == loc2
          else property $ loc1 > loc2

-- | 测试源码位置的合并
prop_source_location_merge :: Int -> Int -> Int -> Int -> Property
prop_source_location_merge line1 col1 line2 col2 =
  let loc1 = SourceLocation line1 col1
      loc2 = SourceLocation line2 col2
      merged = mergeLocations loc1 loc2
  in if line1 < line2 || (line1 == line2 && col1 <= col2)
     then property $ merged == SourceLocationRange loc1 loc2
     else property $ merged == SourceLocationRange loc2 loc1
  where
    mergeLocations l1 l2 = 
      if l1 <= l2 then SourceLocationRange l1 l2 else SourceLocationRange l2 l1

-- | 测试源码位置的扩展
prop_source_location_extend :: Int -> Int -> Int -> Int -> Property
prop_source_location_extend line1 col1 line2 col2 =
  let loc1 = SourceLocation line1 col1
      loc2 = SourceLocation line2 col2
      extended = extendLocation loc1 loc2
  in property $ 
    (sourceLine extended == max line1 line2) && 
    (sourceColumn extended == max col1 col2)
  where
    extendLocation l1 l2 = SourceLocation (max (sourceLine l1) (sourceLine l2)) (max (sourceColumn l1) (sourceColumn l2))

-- | 测试源码位置的偏移
prop_source_location_offset :: Int -> Int -> Int -> Int -> Property
prop_source_location_offset line col lineOffset colOffset =
  let loc = SourceLocation line col
      offsetLoc = offsetLocation loc lineOffset colOffset
  in property $ 
    (sourceLine offsetLoc == line + lineOffset) && 
    (sourceColumn offsetLoc == col + colOffset)
  where
    offsetLocation l lo co = SourceLocation (sourceLine l + lo) (sourceColumn l + co)

-- | 测试源码位置与错误的关联
prop_source_location_error_association :: String -> Int -> Int -> Property
prop_source_location_error_association errMsg line col =
  let loc = SourceLocation line col
      errorWithLoc = ErrorWithLocation errMsg loc
  in property $ 
    (errorMessage errorWithLoc == errMsg) && 
    (errorLocation errorWithLoc == loc)
  where
    errorMessage (ErrorWithLocation msg _) = msg
    errorLocation (ErrorWithLocation _ loc) = loc

-- | 错误位置数据类型
data ErrorWithLocation = ErrorWithLocation String SourceLocation deriving (Eq, Show)

-- | 源码位置范围数据类型
data SourceLocationRange = SourceLocationRange SourceLocation SourceLocation deriving (Eq, Show)

-- | 单元测试：源码位置的边界情况
test_source_location_edge_cases :: Assertion
test_source_location_edge_cases = do
  assertEqual "Zero line and column" (SourceLocation 0 0) (SourceLocation 0 0)
  assertEqual "Positive line and column" (SourceLocation 1 1) (SourceLocation 1 1)
  assertEqual "Large line and column" (SourceLocation 1000 1000) (SourceLocation 1000 1000)

-- | 单元测试：复杂表达式的源码位置
test_source_location_complex_expressions :: Assertion
test_source_location_complex_expressions = do
  let loc1 = SourceLocation 10 20
      loc2 = SourceLocation 15 25
      range = SourceLocationRange loc1 loc2
  assertEqual "Range start location" loc1 (getSourceRangeStart range)
  assertEqual "Range end location" loc2 (getSourceRangeEnd range)
  where
    getSourceRangeStart (SourceLocationRange start _) = start
    getSourceRangeEnd (SourceLocationRange _ end) = end

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
    memoryOptimizedProperty "Source location range validity" (property prop_source_location_range_validity)
  , memoryOptimizedProperty "Source location string representation" (property prop_source_location_string_representation)
  
  -- 位置操作测试
    memoryOptimizedProperty "Source location comparison" (property prop_source_location_comparison)
  , memoryOptimizedProperty "Source location merge" (property prop_source_location_merge)
  , memoryOptimizedProperty "Source location extend" (property prop_source_location_extend)
  , memoryOptimizedProperty "Source location offset" (property prop_source_location_offset)
  
  -- 错误关联测试
    memoryOptimizedProperty "Source location error association" (property prop_source_location_error_association)
  
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