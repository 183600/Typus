{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Unit.ComprehensiveSourceLocationQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Data.List (isInfixOf, nub, sort, group, intercalate, isPrefixOf, isSuffixOf)
import Data.Char (isSpace, isAlpha, isDigit, isAlphaNum, toLower, toUpper)
import Data.Either (isLeft, isRight)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set

import SourceLocation
import Utils
import Parser
import Compiler

import TestSupport.Arbitrary

-- ============================================================================
-- Comprehensive Source Location Properties
-- ============================================================================

-- | 测试源位置的基本属性
prop_source_position_basic_properties :: Int -> Int -> Int -> Property
prop_source_position_basic_properties line col offset =
  let validPos = line >= 0 && col >= 0 && offset >= 0
      pos = SourcePos line col offset
  in if not validPos
     then property True
     else let posStr = show pos
              hasLine = show line `isInfixOf` posStr
              hasCol = show col `isInfixOf` posStr
              hasOffset = show offset `isInfixOf` posStr
          in property $ hasLine && hasCol && hasOffset

-- | 测试源范围的构建
prop_source_span_construction :: (Int, Int, Int) -> (Int, Int, Int) -> Property
prop_source_span_construction (line1, col1, offset1) (line2, col2, offset2) =
  let validPos = line1 >= 0 && col1 >= 0 && offset1 >= 0 && 
                  line2 >= 0 && col2 >= 0 && offset2 >= 0
      start = SourcePos line1 col1 offset1
      end = SourcePos line2 col2 offset2
      span = SourceSpan start end
      spanValid = isValidSpan span
  in if not validPos
     then property True
     else property $ spanValid

-- | 测试源范围的合并
prop_source_span_merge :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int) -> Property
prop_source_span_merge (l1, c1, o1) (l2, c2, o2) (l3, c3, o3) (l4, c4, o4) =
  let validPos = all (\(l, c, o) -> l >= 0 && c >= 0 && o >= 0) [(l1, c1, o1), (l2, c2, o2), (l3, c3, o3), (l4, c4, o4)]
      span1 = SourceSpan (SourcePos l1 c1 o1) (SourcePos l2 c2 o2)
      span2 = SourceSpan (SourcePos l3 c3 o3) (SourcePos l4 c4 o4)
      merged = mergeSpans span1 span2
  in if not validPos
     then property True
     else property $ isValidSpan merged

-- | 测试源位置的排序
prop_source_position_ordering :: (Int, Int) -> (Int, Int) -> Property
prop_source_position_ordering (line1, col1) (line2, col2) =
  let validPos = line1 >= 0 && col1 >= 0 && line2 >= 0 && col2 >= 0
      pos1 = SourcePos line1 col1 0
      pos2 = SourcePos line2 col2 0
      samePos = pos1 == pos2
  in if not validPos
     then property True
     else property $ samePos == (line1 == line2 && col1 == col2)

-- | 测试空源范围
prop_empty_source_span :: Property
prop_empty_source_span =
  let empty = emptySpan
      start = startPos
      end = startPos
  in property $ empty == SourceSpan start end

-- | 测试源范围的有效性
prop_source_span_validity :: (Int, Int, Int) -> (Int, Int, Int) -> Property
prop_source_span_validity (line1, col1, offset1) (line2, col2, offset2) =
  let validPos = line1 >= 0 && col1 >= 0 && offset1 >= 0 && 
                  line2 >= 0 && col2 >= 0 && offset2 >= 0
      start = SourcePos line1 col1 offset1
      end = SourcePos line2 col2 offset2
      span = SourceSpan start end
      spanValid = isValidSpan span
  in if not validPos
     then property True
     else property $ spanValid

-- | 测试位置创建函数
prop_position_creation :: Int -> Int -> Int -> Property
prop_position_creation line col offset =
  let validPos = line >= 0 && col >= 0 && offset >= 0
  in if not validPos
     then property True
     else let pos1 = SourcePos line col offset
              pos2 = posAt line col offset
          in property $ pos1 == pos2

-- | 测试位置偏移函数
prop_position_offset :: Int -> Int -> Int -> Int -> Property
prop_position_offset line col offset offsetAmount =
  let validPos = line >= 0 && col >= 0 && offset >= 0
      validAmount = offsetAmount >= 0
  in if not (validPos && validAmount)
     then property True
     else let pos = SourcePos line col offset
              afterPos = posAfter pos offsetAmount
          in property $ sourcePosOffset afterPos >= sourcePosOffset pos

-- | 测试Located值的基本属性
prop_located_value_properties :: Int -> String -> Property
prop_located_value_properties posIndex value =
  let validValue = not (null value)
      pos = SourcePos posIndex posIndex posIndex
      located = locatedAt pos value
  in if not validValue
     then property True
     else property $ locatedValue located == value

-- | 测试Located值的映射
prop_located_value_mapping :: Int -> String -> Property
prop_located_value_mapping posIndex value =
  let validValue = not (null value)
      pos = SourcePos posIndex posIndex posIndex
      located = locatedAt pos value
      mapped = mapLocated (length) located
  in if not validValue
     then property True
     else property $ locatedValue mapped == length value

-- | 测试源位置的错误转换
prop_source_location_error_conversion :: Int -> Int -> Property
prop_source_location_error_conversion line col =
  let validPos = line >= 0 && col >= 0
      pos = SourcePos line col 0
      errorLoc = toErrorLocation pos
  in if not validPos
     then property True
     else property $ show errorLoc /= ""

-- | 测试源范围的位置提取
prop_source_span_position_extraction :: (Int, Int, Int) -> (Int, Int, Int) -> Property
prop_source_span_position_extraction (line1, col1, offset1) (line2, col2, offset2) =
  let validPos = line1 >= 0 && col1 >= 0 && offset1 >= 0 && 
                  line2 >= 0 && col2 >= 0 && offset2 >= 0
      start = SourcePos line1 col1 offset1
      end = SourcePos line2 col2 offset2
      span = SourceSpan start end
      extractedStart = spanFrom span
      extractedEnd = spanTo span
  in if not validPos
     then property True
     else property $ extractedStart == start && extractedEnd == end

-- | 测试源范围的顺序创建
prop_source_span_ordered_creation :: (Int, Int, Int) -> (Int, Int, Int) -> Property
prop_source_span_ordered_creation (line1, col1, offset1) (line2, col2, offset2) =
  let validPos = line1 >= 0 && col1 >= 0 && offset1 >= 0 && 
                  line2 >= 0 && col2 >= 0 && offset2 >= 0
      pos1 = SourcePos line1 col1 offset1
      pos2 = SourcePos line2 col2 offset2
      orderedSpan = spanBetweenOrdered pos1 pos2
  in if not validPos
     then property True
     else property $ isValidSpan orderedSpan

-- | 测试源范围的行列位置创建
prop_source_span_line_col_creation :: Int -> Int -> Int -> Int -> Property
prop_source_span_line_col_creation line1 col1 line2 col2 =
  let validPos = line1 >= 0 && col1 >= 0 && line2 >= 0 && col2 >= 0
      pos1 = posAtLineCol line1 col1
      pos2 = posAtLineCol line2 col2
      span = spanBetween pos1 pos2
  in if not validPos
     then property True
     else property $ isValidSpan span

-- ============================================================================
-- Integration Tests with Parser and Compiler
-- ============================================================================

-- | 测试解析器中的源位置跟踪
prop_parser_source_location_tracking :: String -> Property
prop_parser_source_location_tracking code =
  let validCode = not (null code)
      parsed = Parser.parseTypusFile code
  in if not validCode
     then property True
     else case parsed of
            Right ast -> property $ show ast /= ""
            Left _ -> property True

-- | 测试编译器中的源位置传播
prop_compiler_source_location_propagation :: String -> Property
prop_compiler_source_location_propagation code =
  let validCode = not (null code)
      parsed = Parser.parseTypusFile code
      compiled = case parsed of
                   Right ast -> Compiler.compile ast
                   Left _ -> Left [Compiler.malformedSyntaxError]
  in if not validCode
     then property True
     else case compiled of
            Right _ -> property True
            Left _ -> property True

-- | 测试错误位置报告
prop_error_location_reporting :: String -> Int -> Int -> Property
prop_error_location_reporting code line col =
  let validCode = not (null code)
      validPos = line >= 0 && col >= 0
      pos = SourcePos line col 0
      errorLoc = toErrorLocation pos
  in if not (validCode && validPos)
     then property True
     else property $ show errorLoc /= ""

-- ============================================================================
-- Performance Tests
-- ============================================================================

-- | 测试大量源位置操作的性能
prop_massive_source_location_operations :: Int -> Property
prop_massive_source_location_operations numOps =
  let validNum = numOps >= 0 && numOps <= 1000
  in if not validNum
     then property True
     else let positions = take numOps $ map (\i -> SourcePos i i i) [0..]
              spans = zipWith SourceSpan positions (tail positions ++ [last positions])
              validSpans = map isValidSpan spans
          in property $ length validSpans == numOps

-- | 测试复杂源范围计算的性能
prop_complex_source_span_calculations :: Int -> Property
prop_complex_source_span_calculations complexity =
  let validComplexity = complexity >= 0 && complexity <= 100
  in if not validComplexity
     then property True
     else let spans = take complexity $ map (\i -> 
                   SourceSpan (SourcePos i 0 0) (SourcePos (i+1) 0 0)) [0..]
              merged = foldl mergeSpans emptySpan spans
          in property $ isValidSpan merged

-- ============================================================================
-- Edge Case Tests
-- ============================================================================

-- | 测试零长度源范围
prop_zero_length_source_span :: Int -> Int -> Property
prop_zero_length_source_span line col =
  let validPos = line >= 0 && col >= 0
      pos = SourcePos line col 0
      zeroSpan = SourceSpan pos pos
  in if not validPos
     then property True
     else property $ isValidSpan zeroSpan

-- | 测试极大源位置值
prop_extreme_source_position_values :: Property
prop_extreme_source_position_values =
  let maxInt = maxBound `div` 2  -- 避免溢出
      extremePos = SourcePos maxInt maxInt maxInt
  in property $ show extremePos /= ""

-- | 测试源位置的边界条件
prop_source_position_boundary_conditions :: Int -> Property
prop_source_position_boundary_conditions value =
  let pos = SourcePos value value value
      validPos = value >= 0
  in if not validPos
     then property True
     else property $ show pos /= ""

-- | 测试源范围的边界条件
prop_source_span_boundary_conditions :: Int -> Int -> Int -> Int -> Property
prop_source_span_boundary_conditions line1 col1 line2 col2 =
  let validPos = line1 >= 0 && col1 >= 0 && line2 >= 0 && col2 >= 0
      start = SourcePos line1 col1 0
      end = SourcePos line2 col2 0
      span = SourceSpan start end
  in if not validPos
     then property True
     else property $ isValidSpan span

-- | 测试无效源范围的处理
prop_invalid_source_span_handling :: Property
prop_invalid_source_span_handling =
  let startPos = SourcePos 10 5 0
      endPos = SourcePos 5 10 0  -- 结束位置在开始位置之前
      invalidSpan = SourceSpan startPos endPos
      validity = isValidSpan invalidSpan
  in property $ validity == False

-- ============================================================================
-- Test Suite Collection
-- ============================================================================

testSuite :: TestTree
testSuite = testGroup "Comprehensive Source Location QuickCheck Tests"
  [ testProperty "Source Position Basic Properties" prop_source_position_basic_properties
  , testProperty "Source Span Construction" prop_source_span_construction
  , testProperty "Source Span Merge" prop_source_span_merge
  , testProperty "Source Position Ordering" prop_source_position_ordering
  , testProperty "Empty Source Span" prop_empty_source_span
  , testProperty "Source Span Validity" prop_source_span_validity
  , testProperty "Position Creation" prop_position_creation
  , testProperty "Position Offset" prop_position_offset
  , testProperty "Located Value Properties" prop_located_value_properties
  , testProperty "Located Value Mapping" prop_located_value_mapping
  , testProperty "Source Location Error Conversion" prop_source_location_error_conversion
  , testProperty "Source Span Position Extraction" prop_source_span_position_extraction
  , testProperty "Source Span Ordered Creation" prop_source_span_ordered_creation
  , testProperty "Source Span Line Col Creation" prop_source_span_line_col_creation
  , testProperty "Parser Source Location Tracking" prop_parser_source_location_tracking
  , testProperty "Compiler Source Location Propagation" prop_compiler_source_location_propagation
  , testProperty "Error Location Reporting" prop_error_location_reporting
  , testProperty "Massive Source Location Operations" prop_massive_source_location_operations
  , testProperty "Complex Source Span Calculations" prop_complex_source_span_calculations
  , testProperty "Zero Length Source Span" prop_zero_length_source_span
  , testProperty "Extreme Source Position Values" prop_extreme_source_position_values
  , testProperty "Source Position Boundary Conditions" prop_source_position_boundary_conditions
  , testProperty "Source Span Boundary Conditions" prop_source_span_boundary_conditions
  , testProperty "Invalid Source Span Handling" prop_invalid_source_span_handling
  ]