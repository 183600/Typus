{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.CabalSourceLocationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)

import SourceLocation 
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  , posAfter
  , posAt
  , emptySpan
  , spanFrom
  , spanBetween
  , mergeSpans
  , isValidSpan
  , locatedAt
  , locatedWithSpan
  , advancePos
  , advancePosBy
  , comparePos
  )

tests :: TestTree
tests = testGroup "Cabal SourceLocation Tests"
  [ sourcePosTests
  , sourceSpanTests
  , locatedTests
  , positionAdvancementTests
  , spanUtilityTests
  ]

-- | Test SourcePos functions
sourcePosTests :: TestTree
sourcePosTests = testGroup "SourcePos tests"
  [ testCase "startPos has correct values" $
      assertEqual "start position should be (1,1,0)"
        (SourcePos 1 1 0) startPos
  
  , testCase "posAfter handles newline" $
      assertEqual "newline should advance line L.and reset column"
        (SourcePos 2 1 1) (posAfter '\n' startPos)
  
  , testCase "posAfter handles tab" $
      assertEqual "tab should advance to next tab stop"
        (SourcePos 1 9 1) (posAfter '\t' startPos)
  
  , testCase "posAfter handles regular character" $
      assertEqual "regular char should advance column"
        (SourcePos 1 2 1) (posAfter 'a' startPos)
  
  , testCase "posAt creates position at line L.and column" $
      assertEqual "posAt should create correct position"
        (SourcePos 5 10 0) (posAt 5 10)
  
  , testCase "posAfter handles multiple characters" $
      let pos1 = posAfter 'h' startPos
          pos2 = posAfter 'e' pos1
          pos3 = posAfter 'l' pos2
          pos4 = posAfter 'l' pos3
          pos5 = posAfter 'o' pos4
      in assertEqual "should advance correctly through 'hello'"
         (SourcePos 1 6 5) pos5
  ]

-- | Test SourceSpan functions
sourceSpanTests :: TestTree
sourceSpanTests = testGroup "SourceSpan tests"
  [ testCase "emptySpan creates span at position" $
      let pos = posAt 3 5
          span = emptySpan pos
      in assertEqual "empty span should have same start L.and end"
         (SourceSpan pos pos) span
  
  , testCase "spanFrom creates span starting at position" $
      let pos = posAt 2 4
          span = spanFrom pos
      in assertEqual "spanFrom should create empty span at position"
         (SourceSpan pos pos) span
  
  , testCase "spanBetween creates span between positions" $
      let start = posAt 1 1
          end = posAt 1 5
          span = spanBetween start end
      in assertEqual "spanBetween should create correct span"
         (SourceSpan start end) span
  
  , testCase "isValidSpan checks span validity" $
      let validSpan = spanBetween (posAt 1 1) (posAt 1 5)
          invalidSpan = spanBetween (posAt 1 5) (posAt 1 1)
      in do
        assertBool "valid span should pass check" (isValidSpan validSpan)
        assertBool "invalid span should fail check" (not $ isValidSpan invalidSpan)
  ]

-- | Test Located values
locatedTests :: TestTree
locatedTests = testGroup "Located tests"
  [ testCase "locatedAt creates located value" $
      let pos = posAt 2 3
          value = "test"
          located = locatedAt pos value
      in do
        assertEqual "should store correct value" value (locValue located)
        assertEqual "should store correct position" pos (locatedPos located)
  
  , testCase "locatedWithSpan creates located value with span" $
      let start = posAt 1 1
          end = posAt 1 5
          span = spanBetween start end
          value = 42
          located = locatedWithSpan span value
      in do
        assertEqual "should store correct value" value (locValue located)
        assertEqual "should store correct span" span (locatedSpan located)
        assertEqual "should store correct start position" start (locatedPos located)
  
  , testCase "mapLocated applies function to value" $
      let pos = posAt 1 1
          original = locatedAt pos 5
          doubled = mapLocated (*2) original
      in assertEqual "should apply function to value"
         10 (locValue doubled)
  ]

-- | Test position advancement
positionAdvancementTests :: TestTree
positionAdvancementTests = testGroup "Position advancement tests"
  [ testCase "advancePosBy handles empty string" $
      assertEqual "empty string should not change position"
        startPos (advancePosBy "" startPos)
  
  , testCase "advancePosBy handles multiple characters" $
      let pos = advancePosBy "abc" startPos
      in assertEqual "should advance by string L.length"
         (SourcePos 1 4 3) pos
  
  , testCase "advancePosBy handles newlines" $
      let pos = advancePosBy "ab\ncd" startPos
      in assertEqual "should handle newline correctly"
         (SourcePos 2 3 5) pos
  
  , testCase "advancePosBy handles tabs" $
      let pos = advancePosBy "a\tb" startPos
      in assertEqual "should handle tab correctly"
         (SourcePos 1 11 3) pos
  
  , testCase "comparePos compares positions correctly" $
      let pos1 = posAt 1 1
          pos2 = posAt 1 2
          pos3 = posAt 2 1
      in do
        assertEqual "should compare equal positions" EQ (comparePos pos1 pos1)
        assertEqual "should compare earlier vs later" LT (comparePos pos1 pos2)
        assertEqual "should compare later vs earlier" GT (comparePos pos2 pos1)
        assertEqual "should compare different lines" LT (comparePos pos1 pos3)
  ]

-- | Test span utility functions
spanUtilityTests :: TestTree
spanUtilityTests = testGroup "Span utility tests"
  [ testCase "mergeSpans combines spans correctly" $
      let span1 = spanBetween (posAt 1 1) (posAt 1 5)
          span2 = spanBetween (posAt 1 3) (posAt 1 8)
          merged = mergeSpans span1 span2
          expected = spanBetween (posAt 1 1) (posAt 1 8)
      in assertEqual "should create span covering both spans"
         expected merged
  
  , testCase "mergeSpans handles non-overlapping spans" $
      let span1 = spanBetween (posAt 1 1) (posAt 1 3)
          span2 = spanBetween (posAt 2 1) (posAt 2 3)
          merged = mergeSpans span1 span2
          expected = spanBetween (posAt 1 1) (posAt 2 3)
      in assertEqual "should handle non-overlapping spans"
         expected merged
  
  , testCase "mergeSpans handles L.reverse order" $
      let span1 = spanBetween (posAt 1 5) (posAt 1 8)
          span2 = spanBetween (posAt 1 1) (posAt 1 5)
          merged = mergeSpans span1 span2
          expected = spanBetween (posAt 1 1) (posAt 1 8)
      in assertEqual "should work regardless of order"
         expected merged
  ]