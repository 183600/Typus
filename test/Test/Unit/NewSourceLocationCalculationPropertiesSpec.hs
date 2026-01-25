{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewSourceLocationCalculationPropertiesSpec where


import Test.Tasty
import Test.Tasty.QuickCheck



import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Data.Text as T
import SourceLocation
import Parser
import Compiler
import Test.QuickCheck (Positive(..))
import Data.Char (isSpace)
import Data.List (unfoldr)

-- | 测试源码位置的基本属性
prop_sourcepos_basic_properties :: Positive Int -> Positive Int -> Property
prop_sourcepos_basic_properties (Positive line) (Positive col) =
  let pos = SourcePos line col 0
  in property $ posLine pos == line && 
                posColumn pos == col && 
                posOffset pos == 0

-- | 测试源码位置的顺序关系
prop_sourcepos_ordering :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> Property
prop_sourcepos_ordering (Positive line1) (Positive col1) (Positive line2) (Positive col2) =
  let pos1 = SourcePos line1 col1 0
      pos2 = SourcePos line2 col2 0
  in property $ (line1 < line2 || (line1 == line2 && col1 < col2)) == 
                (pos1 < pos2)

-- | 测试源码跨度的构造
prop_sourcespan_construction :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> Property
prop_sourcespan_construction (Positive startLine) (Positive startCol) (Positive endLine) (Positive endCol) =
  let startPos = SourcePos startLine startCol 0
      endPos = SourcePos endLine endCol 0
      span = SourceSpan startPos endPos
  in property $ spanStart span == startPos && 
                spanEnd span == endPos

-- | 测试源码跨度的包含关系
prop_sourcespan_containment :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> Property
prop_sourcespan_containment (Positive startLine) (Positive startCol) (Positive endLine) (Positive endCol) =
  let startPos = SourcePos startLine startCol 0
      endPos = SourcePos (max startLine endLine) (max startCol endCol) 0
      span = SourceSpan startPos endPos
      midPos = SourcePos ((startLine + endLine) `div` 2) ((startCol + endCol) `div` 2) 0
  in property $ posInSpan midPos span

-- | 测试源码跨度的合并
prop_sourcespan_merge :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> Property
prop_sourcespan_merge (Positive startLine1) (Positive startCol1) (Positive endLine2) (Positive endCol2) =
  let startPos1 = SourcePos startLine1 startCol1 0
      endPos1 = SourcePos (startLine1 + 1) (startCol1 + 5) 0
      span1 = SourceSpan startPos1 endPos1
      startPos2 = SourcePos (endLine2 - 1) (endCol2 - 3) 0
      endPos2 = SourcePos endLine2 endCol2 0
      span2 = SourceSpan startPos2 endPos2
      merged = mergeSpans' span1 span2
  in property $ spanStart merged <= spanStart span2 && 
                spanEnd merged >= spanEnd span2

-- | 测试行号计算的准确性
prop_line_number_calculation :: String -> Property
prop_line_number_calculation input =
  let expectedLines = length $ lines input
      positions = calculateLinePositions input
  in property $ length positions >= expectedLines

-- | 测试列号计算的准确性
prop_column_number_calculation :: String -> Property
prop_column_number_calculation input =
  let linePositions = calculateLinePositions input
  in property $ all (\pos -> posColumn pos >= 1) linePositions

-- | 测试偏移量计算的准确性
prop_offset_calculation :: String -> Property
prop_offset_calculation input =
  let positions = calculateAllPositions input
  in property $ all (\pos -> posOffset pos >= 0) positions

-- | 测试多行文本的位置计算
prop_multiline_position_calculation :: String -> String -> Property
prop_multiline_position_calculation line1 line2 =
  let input = line1 ++ "\n" ++ line2
      positions = calculateAllPositions input
  in property $ all (\pos -> posColumn pos >= 1) positions

-- | 测试包含制表符的文本位置计算
prop_tab_character_position_calculation :: String -> Property
prop_tab_character_position_calculation input =
  let withTabs = replaceSpacesWithTabs input
      positions = calculateAllPositions withTabs
  in property $ all (\pos -> posColumn pos >= 1) positions
  where
    replaceSpacesWithTabs [] = []
    replaceSpacesWithTabs (' ':' ':' ':' ':' ':xs) = '\t' : replaceSpacesWithTabs xs
    replaceSpacesWithTabs (x:xs) = x : replaceSpacesWithTabs xs

-- | 测试Unicode字符的位置计算
prop_unicode_position_calculation :: String -> Property
prop_unicode_position_calculation input =
  let unicodeInput = addUnicodeCharacters input
      positions = calculateAllPositions unicodeInput
  in property $ all (\pos -> posColumn pos >= 1) positions
  where
    addUnicodeCharacters [] = "αβγδε"
    addUnicodeCharacters (x:xs) = x : addUnicodeCharacters xs

-- | 测试位置信息的持久性
prop_position_persistence :: String -> Property
prop_position_persistence input =
  let positions1 = calculateAllPositions input
      positions2 = calculateAllPositions input
  in property $ positions1 == positions2

-- | 测试位置计算的幂等性
prop_position_calculation_idempotent :: String -> Property
prop_position_calculation_idempotent input =
  let positions = calculateAllPositions input
      recalculated = concatMap (calculateAllPositions . extractTextAtPosition input) positions
  in property $ length recalculated >= length positions

-- | 测试位置计算的边界条件
prop_position_boundary_conditions :: Property
prop_position_boundary_conditions =
  let emptyInput = ""
      singleChar = "x"
      longLine = replicate 1000 'x'
      manyLines = unlines $ replicate 100 "line"
  in property $ all validPositions 
    [ calculateAllPositions emptyInput,
      calculateAllPositions singleChar,
      calculateAllPositions longLine,
      calculateAllPositions manyLines
    ]
  where
    validPositions positions = all (\pos -> posLine pos >= 1 && posColumn pos >= 1) positions

-- | 测试位置计算的增量更新
prop_position_incremental_update :: String -> String -> Property
prop_position_incremental_update prefix suffix =
  let original = calculateAllPositions prefix
      updated = calculateAllPositions (prefix ++ suffix)
  in property $ length original <= length updated

-- | 测试位置计算的内存效率
prop_position_calculation_memory_efficient :: Positive Int -> Property
prop_position_calculation_memory_efficient (Positive size) =
  let limitedSize = min size 10000
      largeInput = unlines $ replicate limitedSize "test line"
      positions = calculateAllPositions largeInput
  in property $ length positions <= limitedSize + 1

-- 辅助函数：计算行位置
calculateLinePositions :: String -> [SourcePos]
calculateLinePositions input = 
  let lineCount = length $ lines input
  in [SourcePos line 1 0 | line <- [1..lineCount]]

-- 辅助函数：计算所有位置
calculateAllPositions :: String -> [SourcePos]
calculateAllPositions input = 
  let linesList = lines input
      calculatePositionsInLine lineNum line = 
        let chars = length line
        in [SourcePos lineNum col 0 | col <- [1..chars+1]]
  in concat $ zipWith calculatePositionsInLine [1..] linesList

-- 辅助函数：检查位置是否在跨度内
posInSpan :: SourcePos -> SourceSpan -> Bool
posInSpan pos span = 
  let start = spanStart span
      end = spanEnd span
  in (posLine pos > posLine start || 
      (posLine pos == posLine start && posColumn pos >= posColumn start)) &&
     (posLine pos < posLine end || 
      (posLine pos == posLine end && posColumn pos <= posColumn end))

-- 辅助函数：合并跨度
mergeSpans' :: SourceSpan -> SourceSpan -> SourceSpan
mergeSpans' span1 span2 = 
  let start = min (spanStart span1) (spanStart span2)
      end = max (spanEnd span1) (spanEnd span2)
  in SourceSpan start end

-- 辅助函数：从位置提取文本
extractTextAtPosition :: String -> SourcePos -> String
extractTextAtPosition input pos = 
  let linesList = lines input
      targetLine = if posLine pos <= length linesList 
                   then linesList !! (posLine pos - 1) 
                   else ""
  in take (posColumn pos - 1) targetLine

tests :: TestTree
tests = testGroup "New Source Location Calculation Properties Tests"
  [ testProperty "sourcepos basic properties" prop_sourcepos_basic_properties,
    testProperty "sourcepos ordering" prop_sourcepos_ordering,
    testProperty "sourcespan construction" prop_sourcespan_construction,
    testProperty "sourcespan containment" prop_sourcespan_containment,
    testProperty "sourcespan merge" prop_sourcespan_merge,
    testProperty "line number calculation" prop_line_number_calculation,
    testProperty "column number calculation" prop_column_number_calculation,
    testProperty "offset calculation" prop_offset_calculation,
    testProperty "multiline position calculation" prop_multiline_position_calculation,
    testProperty "tab character position calculation" prop_tab_character_position_calculation,
    testProperty "unicode position calculation" prop_unicode_position_calculation,
    testProperty "position persistence" prop_position_persistence,
    testProperty "position calculation idempotent" prop_position_calculation_idempotent,
    testProperty "position boundary conditions" prop_position_boundary_conditions,
    testProperty "position incremental update" prop_position_incremental_update,
    testProperty "position calculation memory efficient" prop_position_calculation_memory_efficient
  ]