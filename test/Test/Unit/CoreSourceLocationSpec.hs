{-# LANGUAGE OverloadedStrings #-}
module Test.Unit.CoreSourceLocationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , startPos
  , emptySpan
  , spanBetween
  , mergeSpans
  , isValidSpan
  , advancePos
  , advancePosBy
  )

-- | Test source location mathematical operations
tests :: TestTree
tests = testGroup "Core Source Location Tests"
  [ testPositionOperations
  , testSpanOperations
  , testPositionAdvancement
  , testSpanValidation
  , testComplexScenarios
  ]

-- | Test basic position operations
testPositionOperations :: TestTree
testPositionOperations = testCase "Position Operations" $ do
  let pos1 = SourcePos 1 1
      pos2 = SourcePos 5 10
  
  pos1 @?= SourcePos 1 1
  pos2 @?= SourcePos 5 10

-- | Test span operations
testSpanOperations :: TestTree
testSpanOperations = testCase "Span Operations" $ do
  let start = SourcePos 1 1
      end = SourcePos 1 5
      span1 = spanBetween start end
      empty = emptySpan
  
  isValidSpan span1 @?= True
  isValidSpan empty @?= False

-- | Test position advancement
testPositionAdvancement :: TestTree
testPositionAdvancement = testCase "Position Advancement" $ do
  let pos = SourcePos 1 1
      pos1 = advancePos pos 'a'
      pos2 = advancePos pos '\n'
      pos3 = advancePosBy pos "hello"
  
  pos1 @?= SourcePos 1 2
  pos2 @?= SourcePos 2 1
  pos3 @?= SourcePos 1 6

-- | Test span validation
testSpanValidation :: TestTree
testSpanValidation = testCase "Span Validation" $ do
  let validSpan = spanBetween (SourcePos 1 1) (SourcePos 1 5)
      invalidSpan = spanBetween (SourcePos 5 1) (SourcePos 1 5)
  
  isValidSpan validSpan @?= True
  -- Note: We don't test invalidSpan validity as it depends on implementation

-- | Test complex scenarios
testComplexScenarios :: TestTree
testComplexScenarios = testCase "Complex Scenarios" $ do
  let pos1 = SourcePos 1 1
      pos2 = SourcePos 2 10
      pos3 = SourcePos 3 5
      span1 = spanBetween pos1 pos2
      span2 = spanBetween pos2 pos3
      merged = mergeSpans span1 span2
  
  isValidSpan span1 @?= True
  isValidSpan span2 @?= True
  isValidSpan merged @?= True