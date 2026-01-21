{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.Unit.SourceLocationTestSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperties, Arbitrary(..), Gen, choose, listOf, elements, oneof, vectorOf, property, (===), forAll, counterexample)
import Test.QuickCheck (Gen)
import Data.List (sort)

import SourceLocation

-- Helper generators for source location tests
genLine :: Gen Int
genLine = choose (1, 10000)

genColumn :: Gen Int
genColumn = choose (1, 1000)

genOffset :: Gen Int
genOffset = choose (0, 1000000)

genSourcePos :: Gen SourcePos
genSourcePos = SourcePos <$> genLine <*> genColumn <*> genOffset

genValidSourceSpan :: Gen SourceSpan
genValidSourceSpan = do
  startLine <- genLine
  startCol <- genColumn
  startOffset <- genOffset
  let startPos = SourcePos startLine startCol startOffset
  
  endLine' <- choose (startLine, startLine + 100)
  endCol <- if endLine' == startLine 
            then choose (startCol, startCol + 100)
            else genColumn
  endOffset <- choose (startOffset, startOffset + 1000)
  let endPos = SourcePos endLine' endCol endOffset
  
  return $ SourceSpan startPos endPos

genEmptySourceSpan :: Gen SourceSpan
genEmptySourceSpan = do
  line <- genLine
  col <- genColumn
  offset <- genOffset
  let pos = SourcePos line col offset
  return $ SourceSpan pos pos

instance Arbitrary SourcePos where
  arbitrary = genSourcePos

instance Arbitrary SourceSpan where
  arbitrary = oneof [genValidSourceSpan, genEmptySourceSpan]

-- Test properties for source location

-- Property 1: Source position preserves line, column, and offset
prop_sourcePosPreservation :: Int -> Int -> Int -> Bool
prop_sourcePosPreservation line col offset =
  let pos = SourcePos line col offset
  in case pos of
    SourcePos l c o -> l == line && c == col && o == offset

-- Property 2: Source span preserves start and end positions
prop_sourceSpanPreservation :: SourcePos -> SourcePos -> Bool
prop_sourceSpanPreservation start end =
  let span = SourceSpan start end
  in case span of
    SourceSpan s e -> s == start && e == end

-- Property 3: Source position ordering is consistent
prop_sourcePosOrdering :: SourcePos -> SourcePos -> Bool
prop_sourcePosOrdering pos1 pos2 =
  let comparison = compare pos1 pos2
      reverseComparison = compare pos2 pos1
  in if comparison == EQ 
     then reverseComparison == EQ
     else comparison /= reverseComparison  -- Different elements should have different orderings

-- Property 4: Source span ordering is consistent
prop_sourceSpanOrdering :: SourceSpan -> SourceSpan -> Bool
prop_sourceSpanOrdering span1 span2 =
  let comparison = compare span1 span2
      reverseComparison = compare span2 span1
  in if comparison == EQ 
     then reverseComparison == EQ
     else comparison /= reverseComparison  -- Different elements should have different orderings

-- Property 5: Empty spans have equal start and end positions
prop_emptySpanEquality :: Int -> Int -> Int -> Bool
prop_emptySpanEquality line col offset =
  let pos = SourcePos line col offset
      span = SourceSpan pos pos
  in case span of
    SourceSpan s e -> s == e

-- Property 6: Span start is less than or equal to span end
prop_spanStartLessThanEnd :: SourceSpan -> Bool
prop_spanStartLessThanEnd span =
  case span of
    SourceSpan start end -> start <= end

-- Helper function to check if span contains position
contains :: SourceSpan -> SourcePos -> Bool
contains srcSpan pos = pos >= spanStart srcSpan && pos <= spanEnd srcSpan

-- Property 7: Span contains its start position
prop_spanContainsStart :: SourceSpan -> Bool
prop_spanContainsStart span =
  case span of
    SourceSpan start _ -> contains span start

-- Property 8: Span contains its end position
prop_spanContainsEnd :: SourceSpan -> Bool
prop_spanContainsEnd span =
  case span of
    SourceSpan _ end -> contains span end

sourceLocationTests :: TestTree
sourceLocationTests = testGroup "Source Location Tests"
  [ testProperties "Source Position Properties"
    [ ("Source position components are preserved", property prop_sourcePosPreservation)
    , ("Source position ordering is consistent", property prop_sourcePosOrdering)
    ]
  , testProperties "Source Span Properties"
    [ ("Source span preserves start and end positions", property prop_sourceSpanPreservation)
    , ("Source span ordering is consistent", property prop_sourceSpanOrdering)
    , ("Empty spans have equal start and end positions", property prop_emptySpanEquality)
    , ("Span start is less than or equal to span end", property prop_spanStartLessThanEnd)
    , ("Span contains its start position", property prop_spanContainsStart)
    , ("Span contains its end position", property prop_spanContainsEnd)
    ]
  ]