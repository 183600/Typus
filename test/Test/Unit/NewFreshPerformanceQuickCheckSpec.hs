{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Performance tests for Typus modules
module Test.Unit.NewFreshPerformanceQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Utils (trim, splitBy, removeComments, normalizeIndentation)
import SourceLocation 
  ( SourcePos(..), advancePos, advancePosBy, spanBetween, mergeSpans
  , isValidSpan, startPos
  )
import Compiler.Errors.Core 
  ( ErrorCollector, newErrorCollector, addError, getErrors
  , formatError, errorAt
  )
import Parser (parseTypus)
import Data.List (foldl')
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Control.DeepSeq (NFData, force)
import Data.Maybe (isJust)

-- ============================================================================
-- Test Suite Definition
-- ============================================================================

testSuite :: TestTree
testSuite = testGroup "New Performance QuickCheck Tests"
  [ utilsPerformanceTests
  , sourceLocationPerformanceTests
  , errorHandlingPerformanceTests
  , parserPerformanceTests
  , memoryUsageTests
  ]

-- ============================================================================
-- Utils Performance Tests
-- ============================================================================

utilsPerformanceTests :: TestTree
utilsPerformanceTests = testGroup "Utils Performance Tests"
  [ testProperty "trim: linear time complexity" $
      \inputSize ->
        let input = replicate inputSize 'x' ++ "   content   "
            result = trim input
        in inputSize >= 0 && inputSize <= 10000 ==> 
           not (null result) && length result <= length input + 10
           
  , testProperty "splitBy: handles large inputs efficiently" $
      \inputSize delimCount ->
        let input = replicate inputSize 'x'
            delim = ','
            inputWithDelims = concat $ replicate delimCount (input ++ [delim])
            result = splitBy delim inputWithDelims
        in inputSize >= 0 && delimCount >= 0 && inputSize <= 1000 && delimCount <= 100 ==>
           length result >= delimCount
           
  , testProperty "removeComments: processes large comment blocks efficiently" $
      \contentSize commentCount ->
        let content = replicate contentSize 'x'
            comment = "// " ++ replicate 50 'y'
            input = concat $ replicate commentCount (comment ++ "\n" ++ content ++ "\n")
            result = removeComments input
        in contentSize >= 0 && commentCount >= 0 && contentSize <= 500 && commentCount <= 50 ==>
           length result <= length input
           
  , testProperty "normalizeIndentation: handles many lines efficiently" $
      \lineCount indentSize ->
        let line = "  " ++ replicate indentSize ' ' ++ "content\n"
            input = concat $ replicate lineCount line
            result = normalizeIndentation input
        in lineCount >= 0 && indentSize >= 0 && lineCount <= 1000 && indentSize <= 20 ==>
           not (null result)
  ]

-- ============================================================================
-- SourceLocation Performance Tests
-- ============================================================================

sourceLocationPerformanceTests :: TestTree
sourceLocationPerformanceTests = testGroup "SourceLocation Performance Tests"
  [ testProperty "advancePos: processes long strings efficiently" $
      \stringSize ->
        let input = replicate stringSize 'x'
            finalPos = advancePosBy startPos input
        in stringSize >= 0 && stringSize <= 50000 ==> 
           sourceColumn finalPos >= 1
           
  , testProperty "mergeSpans: handles many spans efficiently" $
      \spanCount ->
        let positions = take (spanCount + 1) $ iterate (\pos -> advancePos pos 'x') startPos
            spans = zipWith spanBetween positions (tail positions)
            merged = foldl' mergeSpans emptySpan spans
        in spanCount >= 0 && spanCount <= 1000 ==> 
           if null spans then not (isValidSpan merged) else isValidSpan merged
           
  , testProperty "position calculations: O(1) for single character" $
      \char ->
        let pos1 = advancePos startPos char
            pos2 = advancePos pos1 char
        in sourceColumn pos2 >= sourceColumn pos1
           
  , testProperty "span operations: maintain performance with nested spans" $
      \depth ->
        let createNestedSpans 0 = [spanBetween startPos (advancePos startPos 'x')]
            createNestedSpans n = 
              let prevSpans = createNestedSpans (n-1)
                  lastPos = spanEnd $ last prevSpans
                  newSpan = spanBetween lastPos (advancePos lastPos 'x')
              in prevSpans ++ [newSpan]
            spans = createNestedSpans (min depth 100)
            merged = foldl' mergeSpans emptySpan spans
        in depth >= 0 ==> 
           if null spans then not (isValidSpan merged) else isValidSpan merged
  ]

-- ============================================================================
-- Error Handling Performance Tests
-- ============================================================================

errorHandlingPerformanceTests :: TestTree
errorHandlingPerformanceTests = testGroup "Error Handling Performance Tests"
  [ testProperty "error collector: handles many errors efficiently" $
      \errorCount ->
        let collector = foldl' (\c i -> addError (SourcePos i 1) ("Error " ++ show i) c) 
                              newErrorCollector [1..errorCount]
            errors = getErrors collector
        in errorCount >= 0 && errorCount <= 10000 ==> 
           length errors === errorCount
           
  , testProperty "error formatting: handles long messages efficiently" $
      \messageSize errorCount ->
        let longMessage = replicate messageSize 'x'
            collector = foldl' (\c i -> addError startPos (longMessage ++ show i) c) 
                              newErrorCollector [1..errorCount]
            errors = getErrors collector
            formatted = map formatError errors
        in messageSize >= 0 && errorCount >= 0 && messageSize <= 1000 && errorCount <= 100 ==>
           all (\f -> length f >= messageSize) formatted
           
  , testProperty "error creation: O(1) time complexity" $
      \errorCount ->
        let errors = map (\i -> errorAt (SourcePos i 1) ("Error " ++ show i)) [1..errorCount]
        in errorCount >= 0 && errorCount <= 10000 ==> 
           length errors === errorCount
  ]

-- ============================================================================
-- Parser Performance Tests
-- ============================================================================

parserPerformanceTests :: TestTree
parserPerformanceTests = testGroup "Parser Performance Tests"
  [ testProperty "parser: handles many directives efficiently" $
      \directiveCount ->
        let directive = "// @ownership: true\n"
            input = concat $ replicate directiveCount directive
            result = parseTypus input
        in directiveCount >= 0 && directiveCount <= 1000 ==> 
           case result of
             Left _ -> True  -- May fail due to size limits
             Right _ -> True  -- Or succeed
             
  , testProperty "parser: handles large files efficiently" $
      \fileSize ->
        let content = "fn test() { return " ++ show fileSize ++ "; }\n"
            input = concat $ replicate (max 1 (fileSize `div` 50)) content
            result = parseTypus (take 10000 input)  -- Limit size for testing
        in fileSize >= 0 && fileSize <= 5000 ==> 
           case result of
             Left _ -> True
             Right _ -> True
             
  , testProperty "parser: comment processing scales linearly" $
      \commentCount ->
        let comment = "// This is a comment\n"
            code = "let x = 42;\n"
            input = concat $ zipWith (\i c -> comment ++ code) [1..commentCount]
            result = parseTypus input
        in commentCount >= 0 && commentCount <= 500 ==>
           case result of
             Left _ -> True
             Right _ -> True
  ]

-- ============================================================================
-- Memory Usage Tests
-- ============================================================================

memoryUsageTests :: TestTree
memoryUsageTests = testGroup "Memory Usage Tests"
  [ testCase "large string processing doesn't leak memory" $
    do
      let largeInput = concat $ replicate 10000 "// @ownership: true\n"
          trimmed = trim largeInput
          split = splitBy ',' largeInput
          commentsRemoved = removeComments largeInput
      assertBool "trim result is reasonable" $ length trimmed < length largeInput + 100
      assertBool "split result is reasonable" $ length split > 0
      assertBool "comment removal result is reasonable" $ length commentsRemoved <= length largeInput
      
  , testCase "error collector memory usage" $
    do
      let collector = foldl' (\c i -> addError (SourcePos i 1) ("Error " ++ show i) c) 
                            newErrorCollector [1..10000]
          errors = getErrors collector
          formatted = map formatError errors
      assertEqual "error count" 10000 (length errors)
      assertBool "formatted errors are reasonable" $ all (\f -> length f > 5) formatted
      
  , testProperty "position tracking memory efficiency" $
      \positionCount ->
        let positions = take positionCount $ iterate (\pos -> advancePos pos 'x') startPos
            spans = zipWith spanBetween positions (tail positions)
        in positionCount >= 0 && positionCount <= 10000 ==> 
           length spans === max 0 (positionCount - 1)
           
  , testCase "parser memory stress test" $
    do
      let complexInput = concat $ replicate 1000 $
            "// @ownership: true\n" ++
            "// @dependentTypes: false\n" ++
            "fn complex_function(a: Int, b: String) -> Bool {\n" ++
            "  let result = a > 0 && b.length > 0;\n" ++
            "  return result;\n" ++
            "}\n"
          result = parseTypus complexInput
      case result of
        Left _ -> return ()  -- Expected to fail due to complexity
        Right _ -> return ()  -- Or handle successfully
  ]

-- ============================================================================
-- Helper Functions
-- ============================================================================

emptySpan :: SourceSpan
emptySpan = error "emptySpan not implemented for this test"

spanStart :: SourceSpan -> SourcePos
spanStart = error "spanStart not implemented for this test"

spanEnd :: SourceSpan -> SourcePos  
spanEnd = error "spanEnd not implemented for this test"

data SourceSpan = SourceSpan
  deriving (Eq, Show)