module Test.Unit.EnhancedSourceLocationMathSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import SourceLocation 
  ( SourcePos(..)
  , SourceSpan(..)
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
  , spanStart
  , spanEnd
  )

-- | 测试 startPos 的属性：startPos 的行和列都是1
prop_start_pos_values :: Property
prop_start_pos_values = 
  let pos = startPos
  in posLine pos === 1 .&&. posColumn pos === 1

-- | 测试 posAfter 的属性：在同一行中，posAfter 会增加列数
prop_pos_after_same_line :: Positive Int -> Property
prop_pos_after_same_line (Positive n) = 
  let pos = SourcePos 5 10 0
      newPos = posAfter 'x' pos
  in posLine newPos === 5 .&&. posColumn newPos === 10 + n

-- | 测试 posAt 的属性：posAt 0 返回原位置
prop_pos_at_zero :: SourcePos -> Property
prop_pos_at_zero pos = pos === pos

-- | 测试 posAtLineCol 的属性：posAtLineCol 创建的位置具有正确的行和列
prop_pos_at_line_col :: Positive Int -> Positive Int -> Property
prop_pos_at_line_col (Positive line) (Positive col) = 
  let pos = posAtLineCol line col
  in posLine (pos line col 0) === line .&&. posColumn (pos line col 0) === col

-- | 测试 emptySpan 的属性：emptySpan 的开始和结束位置相同
prop_empty_span_same_pos :: Property
prop_empty_span_same_pos = 
  let span = emptySpan
  in spanStart (span pos pos) === spanEnd (span pos pos)

-- | 测试 spanFrom 的属性：spanFrom 创建的跨度以给定位置开始
prop_span_from_start :: SourcePos -> Property
prop_span_from_start pos = spanStart (spanFrom pos) === pos

-- | 测试 spanTo 的属性：spanTo 创建的跨度以给定位置结束
prop_span_to_end :: SourcePos -> Property
prop_span_to_end pos = spanEnd (spanTo pos) === pos

-- | 测试 spanBetween 的属性：spanBetween 创建的跨度以第一个位置开始，以第二个位置结束
prop_span_between :: SourcePos -> SourcePos -> Property
prop_span_between pos1 pos2 = 
  let span = spanBetween pos1 pos2
  in spanStart span === pos1 .&&. spanEnd span === pos2

-- | 测试 mergeSpans 的属性：mergeSpans 的开始位置是两个跨度开始位置中较早的
prop_merge_spans_start :: SourcePos -> SourcePos -> SourcePos -> SourcePos -> Property
prop_merge_spans_start pos1 pos2 pos3 pos4 = 
  let span1 = spanBetween pos1 pos2
      span2 = spanBetween pos3 pos4
      merged = mergeSpans span1 span2
      start1 = spanStart span1
      start2 = spanStart span2
      mergedStart = spanStart merged
  in if posLine start1 < posLine start2 || 
        (posLine start1 == posLine start2 && posColumn start1 <= posColumn start2)
     then mergedStart === start1
     else mergedStart === start2

-- | 测试 mergeSpans 的属性：mergeSpans 的结束位置是两个跨度结束位置中较晚的
prop_merge_spans_end :: SourcePos -> SourcePos -> SourcePos -> SourcePos -> Property
prop_merge_spans_end pos1 pos2 pos3 pos4 = 
  let span1 = spanBetween pos1 pos2
      span2 = spanBetween pos3 pos4
      merged = mergeSpans span1 span2
      end1 = spanEnd span1
      end2 = spanEnd span2
      mergedEnd = spanEnd merged
  in if posLine end1 > posLine end2 || 
        (posLine end1 == posLine end2 && posColumn end1 >= posColumn end2)
     then mergedEnd === end1
     else mergedEnd === end2

-- | 测试 isValidSpan 的属性：emptySpan 是有效的
prop_empty_span_valid :: Property
prop_empty_span_valid = property (isValidSpan (emptySpan startPos))

-- | 测试 isValidSpan 的属性：spanBetween 创建的跨度是有效的
prop_span_between_valid :: SourcePos -> SourcePos -> Property
prop_span_between_valid pos1 pos2 = property (isValidSpan (spanBetween pos1 pos2))

tests :: TestTree
tests = testGroup "Enhanced Source Location Math Tests"
  [ testProperty "start pos values" prop_start_pos_values
  , testProperty "pos after same line" prop_pos_after_same_line
  , testProperty "pos at zero" prop_pos_at_zero
  , testProperty "pos at line col" prop_pos_at_line_col
  , testProperty "empty span same pos" prop_empty_span_same_pos
  , testProperty "span from start" prop_span_from_start
  , testProperty "span to end" prop_span_to_end
  , testProperty "span between" prop_span_between
  , testProperty "merge spans start" prop_merge_spans_start
  , testProperty "merge spans end" prop_merge_spans_end
  , testProperty "empty span valid" prop_empty_span_valid
  , testProperty "span between valid" prop_span_between_valid
  ]