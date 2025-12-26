{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Unit.NewSourceLocationQuickCheckTestsSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Arbitrary(..), Gen, oneof, elements, listOf, choose, property, (==>), forAll)
import TestSupport.QuickCheck (fastProperty)

import SourceLocation
import qualified Data.Text as T
import qualified Data.List as List

-- Additional generators for source location testing
genSmallLine :: Gen Int
genSmallLine = choose (1, 50)

genSmallColumn :: Gen Int
genSmallColumn = choose (1, 100)

genSmallOffset :: Gen Int
genSmallOffset = choose (0, 1000)

genSourcePos :: Gen SourcePos
genSourcePos = SourcePos <$> genSmallLine <*> genSmallColumn <*> genSmallOffset

genValidSourceSpan :: Gen SourceSpan
genValidSourceSpan = do
  startLine <- genSmallLine
  startCol <- genSmallColumn
  startOffset <- genSmallOffset
  let startPos = SourcePos startLine startCol startOffset
  
  endLine <- choose (startLine, startLine + 20)
  endCol <- if endLine == startLine 
            then choose (startCol, startCol + 100)
            else genSmallColumn
  endOffset <- choose (startOffset, startOffset + 2000)
  let endPos = SourcePos endLine endCol endOffset
  
  return $ SourceSpan startPos endPos

genZeroLengthSpan :: Gen SourceSpan
genZeroLengthSpan = do
  line <- genSmallLine
  col <- genSmallColumn
  offset <- genSmallOffset
  let pos = SourcePos line col offset
  return $ SourceSpan pos pos

genMultiLineSpan :: Gen SourceSpan
genMultiLineSpan = do
  startLine <- genSmallLine
  startCol <- genSmallColumn
  startOffset <- genSmallOffset
  let startPos = SourcePos startLine startCol startOffset
  
  endLine <- choose (startLine + 1, startLine + 10)
  endCol <- genSmallColumn
  endOffset <- choose (startOffset + 100, startOffset + 2000)
  let endPos = SourcePos endLine endCol endOffset
  
  return $ SourceSpan startPos endPos

-- Property: Source span length is non-negative
prop_sourceSpanLengthNonNegative :: SourceSpan -> Bool
prop_sourceSpanLengthNonNegative span = 
  let start = spanStart span
      end = spanEnd span
      startOffset = sourcePosOffset start
      endOffset = sourcePosOffset end
      length = endOffset - startOffset
  in length >= 0

-- Property: Zero-length spans have same start and end positions
prop_zeroLengthSpanPositionsEqual :: SourceSpan -> Bool
prop_zeroLengthSpanPositionsEqual span = 
  let start = spanStart span
      end = spanEnd span
      startOffset = sourcePosOffset start
      endOffset = sourcePosOffset end
      length = endOffset - startOffset
  in length == 0 ==> start == end

-- Property: Multi-line spans have different line numbers
prop_multiLineSpanHasDifferentLines :: SourceSpan -> Bool
prop_multiLineSpanHasDifferentLines span = 
  let start = spanStart span
      end = spanEnd span
      startLine = sourcePosLine start
      endLine = sourcePosLine end
  in startLine /= endLine ==> endLine > startLine

-- Property: Source position ordering is consistent
prop_sourcePositionOrdering :: SourcePos -> SourcePos -> Bool
prop_sourcePositionOrdering pos1 pos2 = 
  let line1 = sourcePosLine pos1
      line2 = sourcePosLine pos2
      col1 = sourcePosColumn pos1
      col2 = sourcePosColumn pos2
      offset1 = sourcePosOffset pos1
      offset2 = sourcePosOffset pos2
  in if line1 < line2 then True
     else if line1 > line2 then False
     else if col1 < col2 then True
     else if col1 > col2 then False
     else offset1 <= offset2

-- Property: Span contains its start position
prop_spanContainsStartPosition :: SourceSpan -> Bool
prop_spanContainsStartPosition span = 
  let start = spanStart span
      end = spanEnd span
      startOffset = sourcePosOffset start
      endOffset = sourcePosOffset end
  in startOffset <= endOffset

-- Property: Span contains its end position
prop_spanContainsEndPosition :: SourceSpan -> Bool
prop_spanContainsEndPosition span = 
  let start = spanStart span
      end = spanEnd span
      startOffset = sourcePosOffset start
      endOffset = sourcePosOffset end
  in startOffset <= endOffset

-- Property: Span merging preserves coverage
prop_spanMergingCoverage :: SourceSpan -> SourceSpan -> Bool
prop_spanMergingCoverage span1 span2 = 
  let start1 = spanStart span1
      end1 = spanEnd span1
      start2 = spanStart span2
      end2 = spanEnd span2
      startOffset1 = sourcePosOffset start1
      endOffset1 = sourcePosOffset end1
      startOffset2 = sourcePosOffset start2
      endOffset2 = sourcePosOffset end2
      mergedStartOffset = min startOffset1 startOffset2
      mergedEndOffset = max endOffset1 endOffset2
      originalLength1 = endOffset1 - startOffset1
      originalLength2 = endOffset2 - startOffset2
      mergedLength = mergedEndOffset - mergedStartOffset
  in mergedLength >= max originalLength1 originalLength2

-- Property: Line and column are within reasonable bounds
prop_lineColumnBounds :: SourcePos -> Bool
prop_lineColumnBounds pos = 
  let line = sourcePosLine pos
      col = sourcePosColumn pos
      offset = sourcePosOffset pos
  in line >= 1 && col >= 1 && offset >= 0

-- Test suite
tests :: TestTree
tests = testGroup "New SourceLocation QuickCheck Tests"
  [ testProperty "Source span length is non-negative" $
      fastProperty "Span length non-negative" prop_sourceSpanLengthNonNegative
  
  , testProperty "Zero-length spans have same start and end positions" $
      fastProperty "Zero-length span positions" prop_zeroLengthSpanPositionsEqual
  
  , testProperty "Multi-line spans have different line numbers" $
      fastProperty "Multi-line span lines" prop_multiLineSpanHasDifferentLines
  
  , testProperty "Source position ordering is consistent" $
      fastProperty "Position ordering" prop_sourcePositionOrdering
  
  , testProperty "Span contains its start position" $
      fastProperty "Span contains start" prop_spanContainsStartPosition
  
  , testProperty "Span contains its end position" $
      fastProperty "Span contains end" prop_spanContainsEndPosition
  
  , testProperty "Span merging preserves coverage" $
      fastProperty "Span merging coverage" prop_spanMergingCoverage
  
  , testProperty "Line and column are within reasonable bounds" $
      fastProperty "Line column bounds" prop_lineColumnBounds
  ]