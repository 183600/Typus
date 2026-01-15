{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.EnhancedSourceLocationQuickCheckPropertiesSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.QuickCheck (conjoin, (===), Property, property, forAll, choose, listOf1, elements, oneof, suchThat)

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , HasLocation(..)
  , startPos
  , posAfter
  , posAt
  , posAtLineCol
  , emptySpan
  , spanFrom
  , spanTo
  , spanBetween
  , spanBetweenOrdered
  , mergeSpans
  , isValidSpan
  , isValidBlockSpan
  , locatedAt
  , locatedWithSpan
  , locatedValue
  , locatedSpan
  , locatedPos
  , mapLocated
  , advancePos
  , advancePosBy
  , advancePosByText
  , advancePosByLine
  , comparePos
  , toErrorLocation
  , toErrorLocationWithSpan
  )

import Compiler.Errors.Core (ErrorLocation(..))
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Char (isAlphaNum, isAlpha, isSpace, isControl)
import Data.Either (isLeft, isRight)
import Control.Monad (replicateM)

-- 生成有效的行号
genLineNumber :: Gen Int
genLineNumber = choose (1, 1000)

-- 生成有效的列号
genColumnNumber :: Gen Int
genColumnNumber = choose (1, 1000)

-- 生成有效的偏移量
genOffset :: Gen Int
genOffset = choose (0, 10000)

-- 生成源位置
genSourcePos :: Gen SourcePos
genSourcePos = do
  line <- genLineNumber
  column <- genColumnNumber
  offset <- genOffset
  return $ SourcePos line column offset

-- 生成源跨度
genSourceSpan :: Gen SourceSpan
genSourceSpan = do
  startLine <- genLineNumber
  startColumn <- genColumnNumber
  startOffset <- genOffset
  endLine <- choose (startLine, startLine + 100)  -- 确保结束行不小于开始行
  endColumn <- if endLine == startLine 
               then choose (startColumn, startColumn + 100)  -- 同一行时，结束列不小于开始列
               else genColumnNumber
  endOffset <- choose (startOffset, startOffset + 1000)
  return $ SourceSpan (SourcePos startLine startColumn startOffset) 
                      (SourcePos endLine endColumn endOffset)

-- 生成字符
genChar :: Gen Char
genChar = elements ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n\r.,!?-;:()[]{}\"'"

-- 生成字符串
genString :: Gen String
genString = listOf genChar

-- 生成定位值
genLocatedValue :: Gen (Located String)
genLocatedValue = do
  value <- genString
  span <- genSourceSpan
  return $ locatedWithSpan value span

-- 属性1: startPos应该是(1, 1, 0)
prop_start_pos_is_correct :: Property
prop_start_pos_is_correct =
  property $ startPos === SourcePos 1 1 0

-- 属性2: posAfter处理换行符应该增加行号并重置列号
prop_pos_after_newline :: Property
prop_pos_after_newline = forAll genSourcePos $ \pos ->
  let newPos = posAfter '\n' pos
  in property $ posLine newPos === posLine pos + 1 && posColumn newPos === 1

-- 属性3: posAfter处理制表符应该正确对齐列号
prop_pos_after_tab :: Property
prop_pos_after_tab = forAll genSourcePos $ \pos ->
  let newPos = posAfter '\t' pos
      expectedCol = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in property $ posColumn newPos === expectedCol

-- 属性4: posAfter处理普通字符应该只增加列号和偏移量
prop_pos_after_normal_char :: Property
prop_pos_after_normal_char = forAll genSourcePos $ \pos ->
  forAll (suchThat genChar (`notElem` "\n\t")) $ \char ->
    let newPos = posAfter char pos
    in property $ posLine newPos === posLine pos && 
                  posColumn newPos === posColumn pos + 1 &&
                  posOffset newPos === posOffset pos + 1

-- 属性5: posAt应该创建指定位置
prop_pos_at_creates_correct_position :: Property
prop_pos_at_creates_correct_position = 
  forAll genLineNumber $ \line ->
  forAll genColumnNumber $ \column ->
  forAll genOffset $ \offset ->
  let pos = posAt line column offset
  in property $ posLine pos === line && 
                posColumn pos === column && 
                posOffset pos === offset

-- 属性6: posAtLineCol应该创建指定行列的位置
prop_pos_at_line_col_creates_correct_position :: Property
prop_pos_at_line_col_creates_correct_position = 
  forAll genLineNumber $ \line ->
  forAll genColumnNumber $ \column ->
  let pos = posAtLineCol line column
  in property $ posLine pos === line && posColumn pos === column

-- 属性7: emptySpan应该有相同的开始和结束位置
prop_empty_span_has_same_positions :: Property
prop_empty_span_has_same_positions = forAll genSourcePos $ \pos ->
  let span = emptySpan pos
  in property $ spanStart span === pos && spanEnd span === pos

-- 属性8: spanFrom应该创建从指定位置开始的跨度
prop_span_from_creates_correct_span :: Property
prop_span_from_creates_correct_span = forAll genSourcePos $ \pos ->
  let span = spanFrom pos
  in property $ spanStart span === pos

-- 属性9: spanTo应该创建到指定位置结束的跨度
prop_span_to_creates_correct_span :: Property
prop_span_to_creates_correct_span = forAll genSourcePos $ \pos ->
  let span = spanTo pos
  in property $ spanEnd span === pos

-- 属性10: spanBetween应该创建两个位置之间的跨度
prop_span_between_creates_correct_span :: Property
prop_span_between_creates_correct_span = 
  forAll genSourcePos $ \start ->
  forAll genSourcePos $ \end ->
  let span = spanBetween start end
  in property $ spanStart span === start && spanEnd span === end

-- 属性11: mergeSpans应该包含两个原始跨度
prop_merge_spans_contains_originals :: Property
prop_merge_spans_contains_originals = 
  forAll genSourceSpan $ \span1 ->
  forAll genSourceSpan $ \span2 ->
  let merged = mergeSpans span1 span2
      start1 = spanStart span1
      end1 = spanEnd span1
      start2 = spanStart span2
      end2 = spanEnd span2
      mergedStart = spanStart merged
      mergedEnd = spanEnd merged
  in property $ comparePos mergedStart start1 /= GT &&  -- 合并后的开始不晚于span1的开始
                comparePos mergedStart start2 /= GT &&  -- 合并后的开始不晚于span2的开始
                comparePos mergedEnd end1 /= LT &&      -- 合并后的结束不早于span1的结束
                comparePos mergedEnd end2 /= LT         -- 合并后的结束不早于span2的结束

-- 属性12: isValidSpan应该验证跨度的有效性
prop_is_valid_span_checks_validity :: Property
prop_is_valid_span_checks_validity = forAll genSourceSpan $ \span ->
  let start = spanStart span
      end = spanEnd span
      valid = comparePos start end /= GT  -- 开始不晚于结束
  in property $ isValidSpan span === valid

-- 属性13: locatedAt应该创建指定位置的定位值
prop_located_at_creates_correct_located :: Property
prop_located_at_creates_correct_located = 
  forAll genString $ \value ->
  forAll genSourcePos $ \pos ->
  let located = locatedAt value pos
  in property $ locatedValue located === value && 
                locatedPos located === pos

-- 属性14: locatedWithSpan应该创建指定跨度的定位值
prop_located_with_span_creates_correct_located :: Property
prop_located_with_span_creates_correct_located = do
  value <- genString
  span <- genSourceSpan
  let located = locatedWithSpan value span
  in property $ locatedValue located === value && 
                locatedSpan located === span

-- 属性15: mapLocated应该应用函数到定位值
prop_map_located_applies_function :: Property
prop_map_located_applies_function = do
  value <- genString
  span <- genSourceSpan
  let located = locatedWithSpan value span
      mapped = mapLocated (length) located
  in property $ locatedValue mapped === length value &&
                locatedSpan mapped === span

-- 属性16: advancePosByText应该正确处理文本
prop_advance_pos_by_text :: Property
prop_advance_pos_by_text = forAll genString $ \text ->
  let start = startPos
      end = advancePosByText start text
  in property $ posOffset end >= posOffset start

-- 属性17: advancePosByLine应该增加行号
prop_advance_pos_by_line :: Property
prop_advance_pos_by_line = do
  pos <- genSourcePos
  lines <- choose (1, 10)
  let newPos = advancePosByLine pos lines
  in property $ posLine newPos === posLine pos + lines

-- 属性18: comparePos应该正确比较位置
prop_compare_pos_correct_comparison :: Property
prop_compare_pos_correct_comparison = do
  pos1 <- genSourcePos
  pos2 <- genSourcePos
  let result = comparePos pos1 pos2
  in property $ (result == EQ) === (pos1 == pos2) &&
                (result == LT) === (posLine pos1 < posLine pos2 || 
                                   (posLine pos1 == posLine pos2 && posColumn pos1 < posColumn pos2) ||
                                   (posLine pos1 == posLine pos2 && posColumn pos1 == posColumn pos2 && posOffset pos1 < posOffset pos2))

-- 属性19: toErrorLocation应该转换源位置为错误位置
prop_to_error_location_converts_correctly :: Property
prop_to_error_location_converts_correctly = forAll genSourcePos $ \pos ->
  let errorLoc = toErrorLocation pos
  in property $ errorLine errorLoc === posLine pos &&
                errorColumn errorLoc === posColumn pos

-- 属性20: toErrorLocationWithSpan应该转换源跨度为错误位置
prop_to_error_location_with_span_converts_correctly :: Property
prop_to_error_location_with_span_converts_correctly = forAll genSourceSpan $ \span ->
  let errorLoc = toErrorLocationWithSpan span
      start = spanStart span
  in property $ errorLine errorLoc === posLine start &&
                errorColumn errorLoc === posColumn start

-- 测试套件
tests :: TestTree
tests = testGroup "SourceLocation QuickCheck Properties Tests"
  [ testProperty "Start pos is correct" prop_start_pos_is_correct
  , testProperty "Pos after newline" prop_pos_after_newline
  , testProperty "Pos after tab" prop_pos_after_tab
  , testProperty "Pos after normal char" prop_pos_after_normal_char
  , testProperty "Pos at creates correct position" prop_pos_at_creates_correct_position
  , testProperty "Pos at line col creates correct position" prop_pos_at_line_col_creates_correct_position
  , testProperty "Empty span has same positions" prop_empty_span_has_same_positions
  , testProperty "Span from creates correct span" prop_span_from_creates_correct_span
  , testProperty "Span to creates correct span" prop_span_to_creates_correct_span
  , testProperty "Span between creates correct span" prop_span_between_creates_correct_span
  , testProperty "Merge spans contains originals" prop_merge_spans_contains_originals
  , testProperty "Is valid span checks validity" prop_is_valid_span_checks_validity
  , testProperty "Located at creates correct located" prop_located_at_creates_correct_located
  , testProperty "Located with span creates correct located" prop_located_with_span_creates_correct_located
  , testProperty "Map located applies function" prop_map_located_applies_function
  , testProperty "Advance pos by text" prop_advance_pos_by_text
  , testProperty "Advance pos by line" prop_advance_pos_by_line
  , testProperty "Compare pos correct comparison" prop_compare_pos_correct_comparison
  , testProperty "To error location converts correctly" prop_to_error_location_converts_correctly
  , testProperty "To error location with span converts correctly" prop_to_error_location_with_span_converts_correctly
  ]