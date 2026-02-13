{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.SourceLocationComprehensiveQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified SourceLocation as SL
import Data.Char (isAlphaNum, isLetter, isDigit)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Maybe (isJust, isNothing)
import Data.Either (isLeft, isRight)
import qualified Data.Text as T

-- ============================================================================
-- SourceLocation模块的QuickCheck测试 (25个测试)
-- ============================================================================

-- | 测试SourcePos的创建
prop_source_pos_creation :: Int -> Int -> Int -> Property
prop_source_pos_creation line column offset =
  let pos = SL.SourcePos line column offset
  in property $ SL.posLine pos === line .&.
                SL.posColumn pos === column .&.
                SL.posOffset pos === offset

-- | 测试startPos
prop_start_pos :: Property
prop_start_pos = 
  let pos = SL.startPos
  in property $ SL.posLine pos === 1 .&.
                SL.posColumn pos === 1 .&.
                SL.posOffset pos === 0

-- | 测试empty SourcePos
prop_empty_source_pos :: Property
prop_empty_source_pos = 
  let pos = SL.empty
  in property $ SL.posLine pos === 0 .&.
                SL.posColumn pos === 0 .&.
                SL.posOffset pos === 0

-- | 测试posAfter
prop_pos_after :: Int -> Int -> Int -> Property
prop_pos_after line column offset =
  let pos = SL.SourcePos line column offset
      after = SL.posAfter pos
  in property $ SL.posOffset after === offset + 1

-- | 测试posAt
prop_pos_at :: Int -> Int -> Property
prop_pos_at line column =
  let pos = SL.posAt line column
  in property $ SL.posLine pos === line .&.
                SL.posColumn pos === column

-- | 测试posAtLineCol
prop_pos_at_line_col :: Int -> Int -> Property
prop_pos_at_line_col line column =
  let pos = SL.posAtLineCol line column
  in property $ SL.posLine pos === line .&.
                SL.posColumn pos === column

-- | 测试posAdvanceBy
prop_pos_advance_by :: Int -> Int -> Int -> Property
prop_pos_advance_by line column offset =
  let pos = SL.SourcePos line column offset
      advanced = SL.posAdvanceBy pos 5
  in property $ SL.posOffset advanced === offset + 5

-- | 测试SourceSpan的创建
prop_source_span_creation :: Int -> Int -> Int -> Int -> Property
prop_source_span_creation line1 col1 line2 col2 =
  let start = SL.SourcePos line1 col1 0
      end = SL.SourcePos line2 col2 10
      span = SL.SourceSpan start end
  in property $ SL.spanStart span === start .&.
                SL.spanEnd span === end

-- | 测试emptySpan
prop_empty_span :: Property
prop_empty_span = 
  let span = SL.emptySpan
      emptyPos = SL.empty
  in property $ SL.spanStart span === emptyPos .&.
                SL.spanEnd span === emptyPos

-- | 测试spanFrom
prop_span_from :: Int -> Int -> Property
prop_span_from line column =
  let pos = SL.SourcePos line column 0
      span = SL.spanFrom pos
  in property $ SL.spanStart span === pos .&.
                SL.spanEnd span === pos

-- | 测试spanTo
prop_span_to :: Int -> Int -> Property
prop_span_to line column =
  let pos = SL.SourcePos line column 0
      span = SL.spanTo pos
  in property $ SL.spanStart span === pos .&.
                SL.spanEnd span === pos

-- | 测试spanBetween
prop_span_between :: Int -> Int -> Int -> Int -> Property
prop_span_between line1 col1 line2 col2 =
  let start = SL.SourcePos line1 col1 0
      end = SL.SourcePos line2 col2 10
      span = SL.spanBetween start end
  in property $ SL.spanStart span === start .&.
                SL.spanEnd span === end

-- | 测试spanStartPos
prop_span_start_pos :: Int -> Int -> Int -> Int -> Property
prop_span_start_pos line1 col1 line2 col2 =
  let start = SL.SourcePos line1 col1 0
      end = SL.SourcePos line2 col2 10
      span = SL.SourceSpan start end
      startPos = SL.spanStartPos span
  in property $ startPos === start

-- | 测试spanEndPos
prop_span_end_pos :: Int -> Int -> Int -> Int -> Property
prop_span_end_pos line1 col1 line2 col2 =
  let start = SL.SourcePos line1 col1 0
      end = SL.SourcePos line2 col2 10
      span = SL.SourceSpan start end
      endPos = SL.spanEndPos span
  in property $ endPos === end

-- | 测试mergeSpans
prop_merge_spans :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_merge_spans line1 col1 line2 col2 line3 col3 line4 col4 =
  let start1 = SL.SourcePos line1 col1 0
      end1 = SL.SourcePos line2 col2 10
      start2 = SL.SourcePos line3 col3 20
      end2 = SL.SourcePos line4 col4 30
      span1 = SL.SourceSpan start1 end1
      span2 = SL.SourceSpan start2 end2
      merged = SL.mergeSpans span1 span2
  in property $ SL.spanStart merged === start1 .&.
                SL.spanEnd merged === end2

-- | 测试isValidSpan
prop_is_valid_span :: Int -> Int -> Int -> Int -> Property
prop_is_valid_span line1 col1 line2 col2 =
  let start = SL.SourcePos line1 col1 0
      end = SL.SourcePos line2 col2 10
      span = SL.SourceSpan start end
      valid = SL.isValidSpan span
  in property $ valid === (line1 <= line2 || (line1 == line2 && col1 <= col2))

-- | 测试isValidBlockSpan
prop_is_valid_block_span :: Int -> Int -> Int -> Int -> Property
prop_is_valid_block_span line1 col1 line2 col2 =
  let start = SL.SourcePos line1 col1 0
      end = SL.SourcePos line2 col2 10
      span = SL.SourceSpan start end
      valid = SL.isValidBlockSpan span
  in property $ valid === (line1 < line2 || (line1 == line2 && col1 < col2))

-- | 测试locatedAt
prop_located_at :: Int -> Int -> String -> Property
prop_located_at line column value =
  let pos = SL.SourcePos line column 0
      located = SL.locatedAt pos value
  in property $ SL.locatedPos located === pos .&.
                SL.locatedValue located === value

-- | 测试locatedWithSpan
prop_located_with_span :: Int -> Int -> Int -> Int -> String -> Property
prop_located_with_span line1 col1 line2 col2 value =
  let start = SL.SourcePos line1 col1 0
      end = SL.SourcePos line2 col2 10
      span = SL.SourceSpan start end
      located = SL.locatedWithSpan span value
  in property $ SL.locatedSpan located === span .&.
                SL.locatedValue located === value

-- | 测试mapLocated
prop_map_located :: Int -> Int -> String -> Property
prop_map_located line column value =
  let pos = SL.SourcePos line column 0
      located = SL.locatedAt pos value
      mapped = SL.mapLocated reverse located
  in property $ SL.locatedPos mapped === pos .&.
                SL.locatedValue mapped === reverse value

-- | 测试advancePos
prop_advance_pos :: Int -> Int -> Char -> Property
prop_advance_pos line column c =
  let pos = SL.SourcePos line column 0
      advanced = SL.advancePos pos c
  in if c == '\n'
     then property $ SL.posLine advanced === line + 1 .&.
                   SL.posColumn advanced === 1
     else property $ SL.posLine advanced === line .&.
                   SL.posColumn advanced === column + 1

-- | 测试advancePosBy
prop_advance_pos_by :: Int -> Int -> Int -> Property
prop_advance_pos_by line column count =
  let pos = SL.SourcePos line column 0
      advanced = SL.advancePosBy pos count
  in property $ SL.posLine advanced === line .&.
                SL.posColumn advanced === column + count

-- | 测试advancePosByText
prop_advance_pos_by_text :: Int -> Int -> String -> Property
prop_advance_pos_by_text line column text =
  let pos = SL.SourcePos line column 0
      textObj = T.pack text
      advanced = SL.advancePosByText pos textObj
  in property $ SL.posOffset advanced === SL.posOffset pos + T.length textObj

-- | 测试advancePosByLine
prop_advance_pos_by_line :: Int -> Int -> Int -> Property
prop_advance_pos_by_line line column lineCount =
  let pos = SL.SourcePos line column 0
      advanced = SL.advancePosByLine pos lineCount
  in property $ SL.posLine advanced === line + lineCount .&.
                SL.posColumn advanced === column

-- | 测试comparePos
prop_compare_pos :: Int -> Int -> Int -> Int -> Property
prop_compare_pos line1 col1 line2 col2 =
  let pos1 = SL.SourcePos line1 col1 0
      pos2 = SL.SourcePos line2 col2 0
      comparison = SL.comparePos pos1 pos2
  in if line1 < line2
     then property $ comparison == LT
     else if line1 > line2
          then property $ comparison == GT
          else if col1 < col2
               then property $ comparison == LT
               else if col1 > col2
                    then property $ comparison == GT
                    else property $ comparison == EQ

-- | 测试toErrorLocation
prop_to_error_location :: Int -> Int -> Int -> Int -> Property
prop_to_error_location line1 col1 line2 col2 =
  let start = SL.SourcePos line1 col1 0
      end = SL.SourcePos line2 col2 10
      span = SL.SourceSpan start end
      errorLoc = SL.toErrorLocation span
  in property $ SL.elStartLine errorLoc === line1 .&.
                SL.elStartColumn errorLoc === col1 .&.
                SL.elEndLine errorLoc === line2 .&.
                SL.elEndColumn errorLoc === col2

-- | 测试toErrorLocationWithSpan
prop_to_error_location_with_span :: Int -> Int -> Int -> Int -> Property
prop_to_error_location_with_span line1 col1 line2 col2 =
  let start = SL.SourcePos line1 col1 0
      end = SL.SourcePos line2 col2 10
      span = SL.SourceSpan start end
      errorLoc = SL.toErrorLocationWithSpan span
  in property $ SL.elStartLine errorLoc === line1 .&.
                SL.elStartColumn errorLoc === col1 .&.
                SL.elEndLine errorLoc === line2 .&.
                SL.elEndColumn errorLoc === col2

-- | 测试sourceLine
prop_source_line :: Int -> Int -> Int -> Property
prop_source_line line column offset =
  let pos = SL.SourcePos line column offset
  in property $ SL.sourceLine pos === line

-- | 测试sourceColumn
prop_source_column :: Int -> Int -> Int -> Property
prop_source_column line column offset =
  let pos = SL.SourcePos line column offset
  in property $ SL.sourceColumn pos === column

-- | 测试sourcePosOffset
prop_source_pos_offset :: Int -> Int -> Int -> Property
prop_source_pos_offset line column offset =
  let pos = SL.SourcePos line column offset
  in property $ SL.sourcePosOffset pos === offset

-- 将所有测试组合在一起
testSuite :: TestTree
testSuite = testGroup "SourceLocation模块Comprehensive QuickCheck测试"
  [ testProperty "SourcePos的创建" prop_source_pos_creation
  , testProperty "startPos" prop_start_pos
  , testProperty "empty SourcePos" prop_empty_source_pos
  , testProperty "posAfter" prop_pos_after
  , testProperty "posAt" prop_pos_at
  , testProperty "posAtLineCol" prop_pos_at_line_col
  , testProperty "posAdvanceBy" prop_pos_advance_by
  , testProperty "SourceSpan的创建" prop_source_span_creation
  , testProperty "emptySpan" prop_empty_span
  , testProperty "spanFrom" prop_span_from
  , testProperty "spanTo" prop_span_to
  , testProperty "spanBetween" prop_span_between
  , testProperty "spanStartPos" prop_span_start_pos
  , testProperty "spanEndPos" prop_span_end_pos
  , testProperty "mergeSpans" prop_merge_spans
  , testProperty "isValidSpan" prop_is_valid_span
  , testProperty "isValidBlockSpan" prop_is_valid_block_span
  , testProperty "locatedAt" prop_located_at
  , testProperty "locatedWithSpan" prop_located_with_span
  , testProperty "mapLocated" prop_map_located
  , testProperty "advancePos" prop_advance_pos
  , testProperty "advancePosBy" prop_advance_pos_by
  , testProperty "advancePosByText" prop_advance_pos_by_text
  , testProperty "advancePosByLine" prop_advance_pos_by_line
  , testProperty "comparePos" prop_compare_pos
  , testProperty "toErrorLocation" prop_to_error_location
  , testProperty "toErrorLocationWithSpan" prop_to_error_location_with_span
  , testProperty "sourceLine" prop_source_line
  , testProperty "sourceColumn" prop_source_column
  , testProperty "sourcePosOffset" prop_source_pos_offset
  ]