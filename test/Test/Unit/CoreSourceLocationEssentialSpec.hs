{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.CoreSourceLocationEssentialSpec (tests) where

import Test.Tasty (TestTree, testGroup)
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
  , spanTo
  , mergeSpans
  , isValidSpan
  , locatedAt
  , locatedWithSpan
  , locatedValue
  , advancePos
  )

tests :: TestTree
tests = testGroup "Core SourceLocation Essential Tests"
  [ testGroup "Source Position Operations"
    [ testCase "startPos creates position at (1,1)" $
        assertEqual "startPos should be (1,1)" (SourcePos 1 1) startPos
    
    , testCase "posAfter advances column" $
        assertEqual "posAfter should advance column" (SourcePos 1 5) (posAfter (SourcePos 1 1) "test")
    
    , testCase "posAfter handles newlines" $
        assertEqual "posAfter should handle newlines" (SourcePos 2 1) (posAfter (SourcePos 1 3) "te\nst")
    
    , testCase "posAfter handles multiple newlines" $
        assertEqual "posAfter should handle multiple newlines" (SourcePos 3 2) (posAfter (SourcePos 1 1) "\n\nx")
    
    , testCase "posAt creates position at specific coordinates" $
        assertEqual "posAt should create position" (SourcePos 5 10) (posAt 5 10)
    ]
  
  , testGroup "Source Span Operations"
    [ testCase "emptySpan has zero length" $
        assertBool "emptySpan should have same start and end" 
          (spanStart emptySpan == spanEnd emptySpan)
    
    , testCase "spanFrom creates span from position" $
        let pos = SourcePos 2 5
            span = spanFrom pos
        in assertEqual "spanFrom should use position as start" pos (spanStart span)
    
    , testCase "spanTo creates span to position" $
        let pos = SourcePos 3 8
            span = spanTo pos
        in assertEqual "spanTo should use position as end" pos (spanEnd span)
    
    , testCase "mergeSpans combines two spans" $
        let span1 = SourceSpan (SourcePos 1 1) (SourcePos 1 5)
            span2 = SourceSpan (SourcePos 2 1) (SourcePos 2 8)
            merged = mergeSpans span1 span2
        in do
          assertEqual "merge should take earliest start" (SourcePos 1 1) (spanStart merged)
          assertEqual "merge should take latest end" (SourcePos 2 8) (spanEnd merged)
    
    , testCase "isValidSpan validates span" $
        let validSpan = SourceSpan (SourcePos 1 1) (SourcePos 1 5)
            invalidSpan = SourceSpan (SourcePos 2 1) (SourcePos 1 5)
        in do
          assertBool "valid span should pass" (isValidSpan validSpan)
          assertBool "invalid span should fail" (not $ isValidSpan invalidSpan)
    ]
  
  , testGroup "Located Values"
    [ testCase "locatedAt creates located value" $
        let value = "test"
            located = locatedAt (SourcePos 1 1) value
        in do
          assertEqual "should store value" value (locatedValue located)
          assertEqual "should create span from position" (SourcePos 1 1) (spanStart $ locatedSpan located)
    
    , testCase "locatedWithSpan creates located value with span" $
        let value = 42
            span = SourceSpan (SourcePos 1 1) (SourcePos 1 3)
            located = locatedWithSpan span value
        in do
          assertEqual "should store value" value (locatedValue located)
          assertEqual "should store span" span (locatedSpan located)
    
    , testCase "mapLocated transforms located value" $
        let value = "hello"
            located = locatedAt (SourcePos 1 1) value
            transformed = mapLocated length located
        in do
          assertEqual "should transform value" 5 (locatedValue transformed)
          assertEqual "should preserve location" (locatedSpan located) (locatedSpan transformed)
    ]
  
  , testGroup "Position Advancement"
    [ testCase "advancePos handles simple text" $
        assertEqual "should advance correctly" (SourcePos 1 5) (advancePos (SourcePos 1 1) "test")
    
    , testCase "advancePos handles newline" $
        assertEqual "should handle newline" (SourcePos 2 3) (advancePos (SourcePos 1 8) "ab\ncd")
    
    , testCase "advancePos handles multiple newlines" $
        assertEqual "should handle multiple newlines" (SourcePos 4 2) (advancePos (SourcePos 1 1) "a\nb\nc\nd")
    
    , testCase "advancePos handles tabs" $
        assertEqual "should handle tabs" (SourcePos 1 5) (advancePos (SourcePos 1 1) "\t\t\t")
    ]
  ]