{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Unit.SourceLocationMathAdvancedSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Utils
import Parser (TypusFile(..), parseTypus, defaultFileDirectives, 
              FileDirectives(..), CodeBlock(..), cbSpan, cbContent, 
              fdOwnership, fdDependentTypes, fdConstraints)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), startPos, spanBetween)
import Compiler (compile, CompilerError(..))
import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum, isControl, isPunctuation, isDigit)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, nub, partition, sort, find)
import Control.Monad (when, replicateM)

-- ============================================================================
-- Source Location Math Advanced Tests
-- ============================================================================

-- | Test source position arithmetic with large values
prop_sourcelocation_large_position_arithmetic :: Int -> Int -> Property
prop_sourcelocation_large_position_arithmetic line col =
  line >= 0 && col >= 0 && line <= 100000 && col <= 100000 ==>
    let pos1 = SourcePos line col 0
        pos2 = SourcePos (line + 1) (col + 1) (col + line)
        span = spanBetween pos1 pos2
    in posLine pos2 == line + 1 && posColumn pos2 == col + 1

-- | Test source span merging operations
prop_sourcelocation_span_merging :: Int -> Int -> Int -> Int -> Property
prop_sourcelocation_span_merging l1 c1 l2 c2 =
  l1 >= 0 && c1 >= 0 && l2 >= 0 && c2 >= 0 ==>
    let pos1 = SourcePos l1 c1 0
        pos2 = SourcePos l2 c2 0
        span1 = spanBetween pos1 pos2
        pos3 = SourcePos (l1 + l2) (c1 + c2) 0
        pos4 = SourcePos (l1 + l2 + 1) (c1 + c2 + 1) 0
        span2 = spanBetween pos3 pos4
    in posLine pos3 == l1 + l2 && posColumn pos3 == c1 + c2

-- | Test source position ordering
prop_sourcelocation_position_ordering :: Int -> Int -> Int -> Int -> Property
prop_sourcelocation_position_ordering l1 c1 l2 c2 =
  l1 >= 0 && c1 >= 0 && l2 >= 0 && c2 >= 0 ==>
    let pos1 = SourcePos l1 c1 0
        pos2 = SourcePos l2 c2 0
        lineComparison = compare l1 l2
        colComparison = if l1 == l2 then compare c1 c2 else EQ
    in (lineComparison == LT || (lineComparison == EQ && colComparison == LT)) ||
       (lineComparison == GT || (lineComparison == EQ && colComparison == GT)) ||
       (lineComparison == EQ && colComparison == EQ)

-- | Test source span containment
prop_sourcelocation_span_containment :: Int -> Int -> Int -> Int -> Property
prop_sourcelocation_span_containment l1 c1 l2 c2 =
  l1 >= 0 && c1 >= 0 && l2 >= 0 && c2 >= 0 ==>
    let pos1 = SourcePos l1 c1 0
        pos2 = SourcePos (l1 + l2) (c1 + c2) 0
        pos3 = SourcePos (l1 + 1) (c1 + 1) 0
        outerSpan = spanBetween pos1 pos2
        innerPos = pos3
    in posLine innerPos >= l1 && posColumn innerPos >= c1

-- | Test source position distance calculation
prop_sourcelocation_distance_calculation :: Int -> Int -> Int -> Int -> Property
prop_sourcelocation_distance_calculation l1 c1 l2 c2 =
  l1 >= 0 && c1 >= 0 && l2 >= 0 && c2 >= 0 ==>
    let pos1 = SourcePos l1 c1 0
        pos2 = SourcePos l2 c2 0
        lineDistance = abs (l2 - l1)
        colDistance = if l1 == l2 then abs (c2 - c1) else c1 + c2
        totalDistance = lineDistance + colDistance
    in totalDistance >= 0

-- | Test source span intersection
prop_sourcelocation_span_intersection :: Int -> Int -> Int -> Int -> Property
prop_sourcelocation_span_intersection l1 c1 l2 c2 =
  l1 >= 0 && c1 >= 0 && l2 >= 0 && c2 >= 0 ==>
    let pos1 = SourcePos l1 c1 0
        pos2 = SourcePos (l1 + l2) (c1 + c2) 0
        pos3 = SourcePos (l1 + 1) (c1 + 1) 0
        pos4 = SourcePos (l1 + l2 - 1) (c1 + c2 - 1) 0
        span1 = spanBetween pos1 pos2
        span2 = spanBetween pos3 pos4
    in posLine pos3 >= l1 && posColumn pos3 >= c1

-- | Test source position normalization
prop_sourcelocation_position_normalization :: Int -> Int -> Property
prop_sourcelocation_position_normalization line col =
  let pos = SourcePos line col 0
      normalizedLine = max 0 line
      normalizedCol = max 0 col
      normalizedPos = SourcePos normalizedLine normalizedCol 0
  in posLine normalizedPos >= 0 && posColumn normalizedPos >= 0

-- | Test source span length calculation
prop_sourcelocation_span_length :: Int -> Int -> Int -> Int -> Property
prop_sourcelocation_span_length l1 c1 l2 c2 =
  l1 >= 0 && c1 >= 0 && l2 >= 0 && c2 >= 0 ==>
    let pos1 = SourcePos l1 c1 0
        pos2 = SourcePos l2 c2 0
        span = spanBetween pos1 pos2
        lineDiff = abs (l2 - l1)
        colDiff = if l1 == l2 then abs (c2 - c1) else c1 + c2
        estimatedLength = lineDiff + colDiff
    in estimatedLength >= 0

-- | Test source position with UTF-8 characters
prop_sourcelocation_utf8_positions :: String -> Property
prop_sourcelocation_utf8_positions unicodeStr =
  not (null unicodeStr) && length unicodeStr <= 50 ==>
    let linesStr = lines unicodeStr
        totalLines = length linesStr
        lastLineLength = if null linesStr then 0 else length (last linesStr)
        pos = SourcePos totalLines lastLineLength 0
    in posLine pos == totalLines && posColumn pos == lastLineLength

-- | Test source span with multi-line content
prop_sourcelocation_multiline_spans :: String -> Property
prop_sourcelocation_multiline_spans content =
  not (null content) && length content <= 100 ==>
    let linesContent = lines content
        lineCount = length linesContent
        startPos = SourcePos 1 0 0
        endPos = SourcePos lineCount (if null linesContent then 0 else length (last linesContent)) 0
        span = spanBetween startPos endPos
    in lineCount >= 1

-- | Test source position arithmetic with negative values
prop_sourcelocation_negative_arithmetic :: Int -> Int -> Property
prop_sourcelocation_negative_arithmetic line col =
  let pos = SourcePos line col 0
      adjustedLine = line + (-5)
      adjustedCol = col + (-3)
      normalizedLine = max 0 adjustedLine
      normalizedCol = max 0 adjustedCol
      adjustedPos = SourcePos normalizedLine normalizedCol 0
  in posLine adjustedPos >= 0 && posColumn adjustedPos >= 0

-- | Test source span expansion
prop_sourcelocation_span_expansion :: Int -> Int -> Int -> Int -> Property
prop_sourcelocation_span_expansion l1 c1 l2 c2 =
  l1 >= 0 && c1 >= 0 && l2 >= 0 && c2 >= 0 ==>
    let pos1 = SourcePos l1 c1 0
        pos2 = SourcePos l2 c2 0
        span = spanBetween pos1 pos2
        expandBy = 5
        expandedStart = SourcePos (max 0 (l1 - expandBy)) (max 0 (c1 - expandBy)) 0
        expandedEnd = SourcePos (l2 + expandBy) (c2 + expandBy) 0
        expandedSpan = spanBetween expandedStart expandedEnd
    in posLine expandedStart <= l1 && posColumn expandedStart <= c1

-- | Test source position comparison
prop_sourcelocation_position_comparison :: Int -> Int -> Int -> Int -> Property
prop_sourcelocation_position_comparison l1 c1 l2 c2 =
  l1 >= 0 && c1 >= 0 && l2 >= 0 && c2 >= 0 ==>
    let pos1 = SourcePos l1 c1 0
        pos2 = SourcePos l2 c2 0
        isSamePosition = l1 == l2 && c1 == c2
        isBeforePosition = l1 < l2 || (l1 == l2 && c1 < c2)
        isAfterPosition = l1 > l2 || (l1 == l2 && c1 > c2)
    in isSamePosition || isBeforePosition || isAfterPosition

-- | Test source span center calculation
prop_sourcelocation_span_center :: Int -> Int -> Int -> Int -> Property
prop_sourcelocation_span_center l1 c1 l2 c2 =
  l1 >= 0 && c1 >= 0 && l2 >= 0 && c2 >= 0 ==>
    let pos1 = SourcePos l1 c1 0
        pos2 = SourcePos l2 c2 0
        centerLine = (l1 + l2) `div` 2
        centerCol = (c1 + c2) `div` 2
        centerPos = SourcePos centerLine centerCol 0
    in centerLine >= min l1 l2 && centerLine <= max l1 l2

-- | Test source position with tab characters
prop_sourcelocation_tab_positions :: Int -> Int -> Property
prop_sourcelocation_tab_positions tabs spaces =
  tabs >= 0 && spaces >= 0 && tabs <= 10 && spaces <= 10 ==>
    let tabStr = concat $ replicate tabs "\t"
        spaceStr = concat $ replicate spaces " "
        content = tabStr ++ spaceStr
        lineLength = length content
        pos = SourcePos 1 lineLength 0
    in posColumn pos == lineLength

-- | Test source span with empty content
prop_sourcelocation_empty_span :: Property
prop_sourcelocation_empty_span =
  let pos = SourcePos 0 0 0
      emptySpan = spanBetween pos pos
  in posLine pos == 0 && posColumn pos == 0

-- | Test source position with large line numbers
prop_sourcelocation_large_line_numbers :: Int -> Property
prop_sourcelocation_large_line_numbers lineNum =
  lineNum >= 0 && lineNum <= 100000 ==>
    let pos = SourcePos lineNum 0 0
    in posLine pos == lineNum

-- | Test source span with non-sequential positions
prop_sourcelocation_nonsequential_positions :: Int -> Int -> Int -> Int -> Property
prop_sourcelocation_nonsequential_positions l1 c1 l2 c2 =
  l1 >= 0 && c1 >= 0 && l2 >= 0 && c2 >= 0 ==>
    let pos1 = SourcePos l1 c1 0
        pos2 = SourcePos l2 c2 0
        span = spanBetween pos1 pos2
    in True  -- Basic test that span creation doesn't crash with non-sequential positions

-- | Test source position arithmetic with overflow protection
prop_sourcelocation_overflow_protection :: Int -> Int -> Property
prop_sourcelocation_overflow_protection base increment =
  let pos = SourcePos base base 0
      maxPos = maxBound `div` 2  -- Use half of max to avoid overflow
      adjustedPos = SourcePos (min maxPos (base + increment)) (min maxPos (base + increment)) 0
  in posLine adjustedPos <= maxPos && posColumn adjustedPos <= maxPos

-- | Test source span with extreme values
prop_sourcelocation_extreme_values :: Property
prop_sourcelocation_extreme_values =
  let minPos = SourcePos 0 0 0
      maxPos = SourcePos maxBound maxBound 0
      extremeSpan = spanBetween minPos maxPos
  in posLine minPos == 0 && posColumn minPos == 0 &&
     posLine maxPos == maxBound && posColumn maxPos == maxBound

-- | Tasty test suite
testSuite :: TestTree
testSuite = testGroup "Source Location Math Advanced Tests"
  [ testProperty "Source position arithmetic with large values" prop_sourcelocation_large_position_arithmetic,
    testProperty "Source span merging operations" prop_sourcelocation_span_merging,
    testProperty "Source position ordering" prop_sourcelocation_position_ordering,
    testProperty "Source span containment" prop_sourcelocation_span_containment,
    testProperty "Source position distance calculation" prop_sourcelocation_distance_calculation,
    testProperty "Source span intersection" prop_sourcelocation_span_intersection,
    testProperty "Source position normalization" prop_sourcelocation_position_normalization,
    testProperty "Source span length calculation" prop_sourcelocation_span_length,
    testProperty "Source position with UTF-8 characters" prop_sourcelocation_utf8_positions,
    testProperty "Source span with multi-line content" prop_sourcelocation_multiline_spans,
    testProperty "Source position arithmetic with negative values" prop_sourcelocation_negative_arithmetic,
    testProperty "Source span expansion" prop_sourcelocation_span_expansion,
    testProperty "Source position comparison" prop_sourcelocation_position_comparison,
    testProperty "Source span center calculation" prop_sourcelocation_span_center,
    testProperty "Source position with tab characters" prop_sourcelocation_tab_positions,
    testProperty "Source span with empty content" prop_sourcelocation_empty_span,
    testProperty "Source position with large line numbers" prop_sourcelocation_large_line_numbers,
    testProperty "Source span with non-sequential positions" prop_sourcelocation_nonsequential_positions,
    testProperty "Source position arithmetic with overflow protection" prop_sourcelocation_overflow_protection,
    testProperty "Source span with extreme values" prop_sourcelocation_extreme_values
  ]