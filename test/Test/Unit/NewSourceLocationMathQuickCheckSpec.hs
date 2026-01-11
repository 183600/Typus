{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Source location math QuickCheck tests for the Typus compiler
-- This module contains property-based tests for source location mathematical operations
module Test.Unit.NewSourceLocationMathQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.QuickCheck ((==>), conjoin, counterexample)
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
  )
import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum)
import Data.List (isPrefixOf, isInfixOf)
import Control.Monad (foldM)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Compiler.Errors.Core as Error
import Compiler.Errors.Core (ErrorSeverity(..), ErrorCategory(..), TypeError(..), ErrorLocation(..), ErrorContext(..),
                            errorAt, errorWithCategory, warningAt, infoAt, 
                            fatalError, withLocation, withContext, combineErrors,
                            combinedErrorSeverity, filterByCategory, filterBySeverity,
                            hasCategory, isAtLeast, severityPriority, location, line, column, 
                            fatalRecovery, emptyContext, contextCode)
import SourceLocation (toErrorLocation)
import Data.Time (UTCTime, getCurrentTime)
import Data.List (sort, nub)
import Data.Ord (comparing)

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- | Check if a position is valid (positive line and column)
isValidPos :: SourcePos -> Bool
isValidPos pos = posLine pos > 0 && posColumn pos > 0 && posOffset pos >= 0

-- | Check if a span is properly ordered (start <= end)
isOrderedSpan :: SourceSpan -> Bool
isOrderedSpan span = 
  let start = spanStart span
      end = spanEnd span
  in posLine start < posLine end || 
     (posLine start == posLine end && posColumn start <= posColumn end)

-- | Check if a position is within a span
posInSpan :: SourcePos -> SourceSpan -> Bool
posInSpan pos span = 
  let start = spanStart span
      end = spanEnd span
  in posLine pos > posLine start || 
     (posLine pos == posLine start && posColumn pos >= posColumn start) &&
     (posLine pos < posLine end || 
      (posLine pos == posLine end && posColumn pos <= posColumn end))

-- | Calculate the distance between two positions
posDistance :: SourcePos -> SourcePos -> Int
posDistance pos1 pos2 = 
  if posLine pos1 == posLine pos2
  then posColumn pos2 - posColumn pos1
  else posOffset pos2 - posOffset pos1

-- ============================================================================
-- Source Position Tests
-- ============================================================================

-- | Test startPos: starting position should be (1,1,0)
prop_startPos_values :: Bool
prop_startPos_values = 
  posLine startPos == 1 && posColumn startPos == 1 && posOffset startPos == 0

-- | Test posAfter: newline advances line number
prop_posAfter_newline :: Int -> Int -> Int -> Bool
prop_posAfter_newline line col offset = 
  let pos = SourcePos line col offset
      newPos = posAfter '\n' pos
  in posLine newPos == posLine pos + 1 && 
     posColumn newPos == 1 && 
     posOffset newPos == posOffset pos + 1

-- | Test posAfter: tab advances to next tab stop
prop_posAfter_tab :: Int -> Int -> Int -> Bool
prop_posAfter_tab line col offset = 
  let pos = SourcePos line col offset
      newPos = posAfter '\t' pos
      expectedCol = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in posLine newPos == posLine pos && 
     posColumn newPos == expectedCol && 
     posOffset newPos == posOffset pos + 1

-- | Test posAfter: regular character advances column
prop_posAfter_regular :: Int -> Int -> Int -> Char -> Property
prop_posAfter_regular line col offset c = 
  c `notElem` ['\n', '\t'] ==>
  let pos = SourcePos line col offset
      newPos = posAfter c pos
  in posLine newPos == posLine pos && 
     posColumn newPos == posColumn pos + 1 && 
     posOffset newPos == posOffset pos + 1

-- | Test posAt: creating position at line and column
prop_posAt_values :: Int -> Int -> Property
prop_posAt_values line col = 
  line > 0 && col > 0 ==>
  let pos = posAt line col
  in posLine pos == line && posColumn pos == col && posOffset pos == 0

-- | Test posAtLineCol: creating position with offset
prop_posAtLineCol_values :: Int -> Int -> Int -> Property
prop_posAtLineCol_values line col offset = 
  line > 0 && col > 0 && offset >= 0 ==>
  let pos = posAtLineCol line col offset
  in posLine pos == line && posColumn pos == col && posOffset pos == offset

-- | Test advancePos: advancing by character
prop_advancePos_char :: Int -> Int -> Int -> Char -> Bool
prop_advancePos_char line col offset c = 
  let pos = SourcePos line col offset
      advanced = advancePos c pos
      expected = posAfter c pos
  in advanced == expected

-- | Test advancePosBy: advancing by multiple characters
prop_advancePosBy_string :: Int -> Int -> Int -> String -> Bool
prop_advancePosBy_string line col offset s = 
  let pos = SourcePos line col offset
      advanced = advancePosBy s pos
      expected = foldl (\p c -> posAfter c p) pos s
  in advanced == expected

-- | Test advancePosByText: advancing by text
prop_advancePosByText :: Int -> Int -> Int -> String -> Bool
prop_advancePosByText line col offset s = 
  let pos = SourcePos line col offset
      text = T.pack s
      advanced = advancePosByText text pos
      expected = advancePosBy s pos
  in advanced == expected

-- | Test advancePosByLine: advancing by lines
prop_advancePosByLine :: Int -> Int -> Int -> Int -> Bool
prop_advancePosByLine line col offset n = 
  let pos = SourcePos line col offset
      advanced = advancePosByLine n pos
      expected = SourcePos (posLine pos + n) 1 (posOffset pos + n)
  in advanced == expected

-- ============================================================================
-- Source Span Tests
-- ============================================================================

-- | Test emptySpan: empty span at start position
prop_emptySpan_values :: Bool
prop_emptySpan_values = 
  let empty = emptySpan startPos
  in spanStart empty == startPos && 
     spanEnd empty == startPos

-- | Test spanFrom: creating span from position
prop_spanFrom_values :: Int -> Int -> Int -> Bool
prop_spanFrom_values line col offset = 
  let pos = SourcePos line col offset
      span = spanFrom pos
  in spanStart span == pos && spanEnd span == pos

-- | Test spanTo: creating span to position
prop_spanTo_values :: Int -> Int -> Int -> Bool
prop_spanTo_values line col offset = 
  let pos = SourcePos line col offset
      span = spanTo pos
  in spanStart span == pos && spanEnd span == pos

-- | Test spanBetween: creating span between two positions
prop_spanBetween_ordered :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
prop_spanBetween_ordered line1 col1 offset1 line2 col2 offset2 = 
  let pos1 = SourcePos line1 col1 offset1
      pos2 = SourcePos line2 col2 offset2
      span = spanBetween pos1 pos2
  in spanStart span == pos1 && spanEnd span == pos2

-- | Test spanBetween: swapping positions if needed
prop_spanBetween_unordered :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
prop_spanBetween_unordered line1 col1 offset1 line2 col2 offset2 = 
  let pos1 = SourcePos line1 col1 offset1
      pos2 = SourcePos line2 col2 offset2
      span = spanBetween pos1 pos2
      start = spanStart span
      end = spanEnd span
  in (posLine start < posLine end || 
      (posLine start == posLine end && posColumn start <= posColumn end)) &&
     (start == pos1 || start == pos2) &&
     (end == pos1 || end == pos2)

-- | Test mergeSpans: merging adjacent spans
prop_mergeSpans_adjacent :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
prop_mergeSpans_adjacent line1 col1 offset1 line2 col2 offset2 = 
  let pos1 = SourcePos line1 col1 offset1
      pos2 = SourcePos line2 col2 offset2
      span1 = spanBetween pos1 pos2
      span2 = spanBetween pos2 pos1
      merged = mergeSpans span1 span2
  in spanStart merged == spanStart span1 && spanEnd merged == spanEnd span2

-- | Test mergeSpans: merging overlapping spans
prop_mergeSpans_overlapping :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
prop_mergeSpans_overlapping line1 col1 offset1 line2 col2 offset2 = 
  let pos1 = SourcePos line1 col1 offset1
      pos2 = SourcePos line2 col2 offset2
      midPos = SourcePos ((line1 + line2) `div` 2) ((col1 + col2) `div` 2) ((offset1 + offset2) `div` 2)
      span1 = spanBetween pos1 midPos
      span2 = spanBetween midPos pos2
      merged = mergeSpans span1 span2
      expected = spanBetween pos1 pos2
  in spanStart merged == spanStart expected && spanEnd merged == spanEnd expected

-- | Test isValidSpan: valid spans
prop_isValidSpan_valid :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
prop_isValidSpan_valid line1 col1 offset1 line2 col2 offset2 = 
  let pos1 = SourcePos line1 col1 offset1
      pos2 = SourcePos line2 col2 offset2
      span = spanBetween pos1 pos2
  in isValidSpan span == isOrderedSpan span

-- | Test isValidSpan: empty spans
prop_isValidSpan_empty :: Bool
prop_isValidSpan_empty = isValidSpan (emptySpan startPos)

-- | Test isValidBlockSpan: block spans
prop_isValidBlockSpan_valid :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
prop_isValidBlockSpan_valid line1 col1 offset1 line2 col2 offset2 = 
  let pos1 = SourcePos line1 col1 offset1
      pos2 = SourcePos line2 col2 offset2
      span = spanBetween pos1 pos2
  in isValidBlockSpan span == (posLine pos1 < posLine pos2)

-- ============================================================================
-- Located Value Tests
-- ============================================================================

-- | Test locatedAt: creating located value
prop_locatedAt_values :: Int -> Int -> Int -> String -> Bool
prop_locatedAt_values line col offset value = 
  let pos = SourcePos line col offset
      located = locatedAt pos value
  in locatedPos located == pos && locatedValue located == value

-- | Test locatedWithSpan: creating located value with span
prop_locatedWithSpan_values :: Int -> Int -> Int -> Int -> Int -> Int -> String -> Bool
prop_locatedWithSpan_values line1 col1 offset1 line2 col2 offset2 value = 
  let pos1 = SourcePos line1 col1 offset1
      pos2 = SourcePos line2 col2 offset2
      span = spanBetween pos1 pos2
      located = locatedWithSpan span value
  in locatedSpan located == span && locatedValue located == value

-- | Test mapLocated: mapping over located value
prop_mapLocated_values :: Int -> Int -> Int -> String -> Bool
prop_mapLocated_values line col offset value = 
  let pos = SourcePos line col offset
      located = locatedAt pos value
      mapped = mapLocated length located
  in locatedPos mapped == pos && locatedValue mapped == length value

-- | Test mapLocated: identity
prop_mapLocated_identity :: Int -> Int -> Int -> String -> Bool
prop_mapLocated_identity line col offset value = 
  let pos = SourcePos line col offset
      located = locatedAt pos value
      mapped = mapLocated id located
  in mapped == located

-- | Test mapLocated: composition
prop_mapLocated_composition :: Int -> Int -> Int -> String -> Bool
prop_mapLocated_composition line col offset value = 
  let pos = SourcePos line col offset
      located = locatedAt pos value
      mapped1 = mapLocated (map toUpper) (mapLocated reverse located)
      mapped2 = mapLocated (map toUpper . reverse) located
  in mapped1 == mapped2
  where
    toUpper c = if c >= 'a' && c <= 'z' then toEnum (fromEnum c - 32) else c

-- ============================================================================
-- Position Math Tests
-- ============================================================================

-- | Test position addition: combining positions
prop_pos_addition :: Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_pos_addition line1 col1 offset1 line2 col2 offset2 = 
  let pos1 = SourcePos line1 col1 offset1
      pos2 = SourcePos line2 col2 offset2
      distance = posDistance pos1 pos2
  in distance >= 0 ==> 
     let expected = SourcePos (posLine pos1 + posLine pos2 - 1) 
                              (posColumn pos1 + posColumn pos2 - 1)
                              (posOffset pos1 + posOffset pos2)
     in posDistance startPos expected >= posDistance startPos pos1 + 
        posDistance startPos pos2 - 2

-- | Test position ordering: comparison
prop_pos_ordering :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
prop_pos_ordering line1 col1 offset1 line2 col2 offset2 = 
  let pos1 = SourcePos line1 col1 offset1
      pos2 = SourcePos line2 col2 offset2
  in (pos1 <= pos2) == (posLine pos1 < posLine pos2 || 
                       (posLine pos1 == posLine pos2 && posColumn pos1 <= posColumn pos2))

-- | Test span ordering: comparison
prop_span_ordering :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool
prop_span_ordering line1 col1 offset1 line2 col2 offset2 line3 col3 offset3 line4 col4 offset4 = 
  let pos1 = SourcePos line1 col1 offset1
      pos2 = SourcePos line2 col2 offset2
      pos3 = SourcePos line3 col3 offset3
      pos4 = SourcePos line4 col4 offset4
      span1 = spanBetween pos1 pos2
      span2 = spanBetween pos3 pos4
  in (span1 <= span2) == (spanStart span1 <= spanStart span2)

-- | Test span distance: calculating span length
prop_span_distance :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
prop_span_distance line1 col1 offset1 line2 col2 offset2 = 
  let pos1 = SourcePos line1 col1 offset1
      pos2 = SourcePos line2 col2 offset2
      span = spanBetween pos1 pos2
      distance = posDistance (spanStart span) (spanEnd span)
  in distance >= 0

-- | Test span contains: checking if position is in span
prop_span_contains :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool
prop_span_contains line1 col1 offset1 line2 col2 offset2 line3 col3 offset3 = 
  let pos1 = SourcePos line1 col1 offset1
      pos2 = SourcePos line2 col2 offset2
      pos3 = SourcePos line3 col3 offset3
      span = spanBetween pos1 pos2
  in posInSpan pos3 span == (spanStart span <= pos3 && pos3 <= spanEnd span)

-- ============================================================================
-- Advanced Math Tests
-- ============================================================================

-- | Test position arithmetic: addition and subtraction
prop_pos_arithmetic :: Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_pos_arithmetic line1 col1 offset1 line2 col2 offset2 = 
  let pos1 = SourcePos line1 col1 offset1
      pos2 = SourcePos line2 col2 offset2
  in posDistance pos1 pos2 >= 0 ==> 
     let distance = posDistance pos1 pos2
         midOffset = posOffset pos1 + distance `div` 2
         midPos = SourcePos (posLine pos1) (posColumn pos1 + distance `div` 2) midOffset
     in posDistance pos1 midPos <= distance && posDistance midPos pos2 <= distance

-- | Test span arithmetic: merging and splitting
prop_span_arithmetic :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool
prop_span_arithmetic line1 col1 offset1 line2 col2 offset2 line3 col3 offset3 = 
  let pos1 = SourcePos line1 col1 offset1
      pos2 = SourcePos line2 col2 offset2
      pos3 = SourcePos line3 col3 offset3
      span1 = spanBetween pos1 pos2
      span2 = spanBetween pos2 pos3
      merged = mergeSpans span1 span2
  in posDistance (spanStart merged) (spanEnd merged) >= 
     posDistance (spanStart span1) (spanEnd span1) + 
     posDistance (spanStart span2) (spanEnd span2)

-- | Test position transformation: applying functions
prop_pos_transformation :: Int -> Int -> Int -> Bool
prop_pos_transformation line col offset = 
  let pos = SourcePos line col offset
      transformed = SourcePos (posLine pos * 2) (posColumn pos * 2) (posOffset pos * 2)
  in posDistance startPos transformed >= posDistance startPos pos * 2

-- | Test span transformation: applying functions
prop_span_transformation :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
prop_span_transformation line1 col1 offset1 line2 col2 offset2 = 
  let pos1 = SourcePos line1 col1 offset1
      pos2 = SourcePos line2 col2 offset2
      span = spanBetween pos1 pos2
      transformedStart = SourcePos (posLine pos1 * 2) (posColumn pos1 * 2) (posOffset pos1 * 2)
      transformedEnd = SourcePos (posLine pos2 * 2) (posColumn pos2 * 2) (posOffset pos2 * 2)
      transformed = spanBetween transformedStart transformedEnd
  in posDistance (spanStart transformed) (spanEnd transformed) >= 
     posDistance (spanStart span) (spanEnd span) * 2

-- ============================================================================
-- Edge Case Tests
-- ============================================================================

-- | Test position edge cases: minimum values
prop_pos_edge_minimum :: Bool
prop_pos_edge_minimum = 
  let pos = SourcePos 1 1 0
  in isValidPos pos

-- | Test position edge cases: large values
prop_pos_edge_large :: Int -> Property
prop_pos_edge_large n = 
  n > 0 ==>
  let pos = SourcePos n n n
  in isValidPos pos

-- | Test span edge cases: empty spans
prop_span_edge_empty :: Int -> Int -> Int -> Bool
prop_span_edge_empty line col offset = 
  let pos = SourcePos line col offset
      span = spanBetween pos pos
  in isValidSpan span && spanStart span == spanEnd span

-- | Test span edge cases: single character spans
prop_span_edge_single :: Int -> Int -> Int -> Bool
prop_span_edge_single line col offset = 
  let pos1 = SourcePos line col offset
      pos2 = posAfter 'a' pos1
      span = spanBetween pos1 pos2
  in isValidSpan span && posDistance (spanStart span) (spanEnd span) == 1

-- | Test located edge cases: empty values
prop_located_edge_empty :: Int -> Int -> Int -> Bool
prop_located_edge_empty line col offset = 
  let pos = SourcePos line col offset
      located = locatedAt pos ""
  in locatedPos located == pos && locatedValue located == ""

-- | Test located edge cases: large values
prop_located_edge_large :: Int -> Int -> Int -> Int -> Property
prop_located_edge_large line col offset n = 
  n > 0 ==>
  let pos = SourcePos line col offset
      value = replicate n 'a'
      located = locatedAt pos value
  in locatedPos located == pos && locatedValue located == value

-- ============================================================================
-- Test Group
-- ============================================================================

tests :: TestTree
tests = testGroup "Source Location Math QuickCheck Tests"
  [ -- Source Position Tests
    testProperty "startPos values" prop_startPos_values
  , testProperty "posAfter newline" prop_posAfter_newline
  , testProperty "posAfter tab" prop_posAfter_tab
  , testProperty "posAfter regular" prop_posAfter_regular
  , testProperty "posAt values" prop_posAt_values
  , testProperty "posAtLineCol values" prop_posAtLineCol_values
  , testProperty "advancePos char" prop_advancePos_char
  , testProperty "advancePosBy string" prop_advancePosBy_string
  , testProperty "advancePosByText" prop_advancePosByText
  , testProperty "advancePosByLine" prop_advancePosByLine
  
  -- Source Span Tests
  , testProperty "emptySpan values" prop_emptySpan_values
  , testProperty "spanFrom values" prop_spanFrom_values
  , testProperty "spanTo values" prop_spanTo_values
  , testProperty "spanBetween ordered" prop_spanBetween_ordered
  , testProperty "spanBetween unordered" prop_spanBetween_unordered
  , testProperty "mergeSpans adjacent" prop_mergeSpans_adjacent
  , testProperty "mergeSpans overlapping" prop_mergeSpans_overlapping
  , testProperty "isValidSpan valid" prop_isValidSpan_valid
  , testProperty "isValidSpan empty" prop_isValidSpan_empty
  , testProperty "isValidBlockSpan valid" prop_isValidBlockSpan_valid
  
  -- Located Value Tests
  , testProperty "locatedAt values" prop_locatedAt_values
  , testProperty "locatedWithSpan values" prop_locatedWithSpan_values
  , testProperty "mapLocated values" prop_mapLocated_values
  , testProperty "mapLocated identity" prop_mapLocated_identity
  , testProperty "mapLocated composition" prop_mapLocated_composition
  
  -- Position Math Tests
  , testProperty "pos addition" prop_pos_addition
  , testProperty "pos ordering" prop_pos_ordering
  , testProperty "span ordering" prop_span_ordering
  , testProperty "span distance" prop_span_distance
  , testProperty "span contains" prop_span_contains
  
  -- Advanced Math Tests
  , testProperty "pos arithmetic" prop_pos_arithmetic
  , testProperty "span arithmetic" prop_span_arithmetic
  , testProperty "pos transformation" prop_pos_transformation
  , testProperty "span transformation" prop_span_transformation
  
  -- Edge Case Tests
  , testProperty "pos edge minimum" prop_pos_edge_minimum
  , testProperty "pos edge large" prop_pos_edge_large
  , testProperty "span edge empty" prop_span_edge_empty
  , testProperty "span edge single" prop_span_edge_single
  , testProperty "located edge empty" prop_located_edge_empty
  , testProperty "located edge large" prop_located_edge_large
  ]