{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.SourceLocationComprehensiveQuickCheckSpec where

import Test.Tasty
import qualified Data.List as L
import Test.Tasty.QuickCheck (property)
import Test.Tasty.HUnit

import SourceLocation
import TestSupport.Arbitrary ()

-- | Test suite for SourceLocation module with comprehensive QuickCheck properties
sourceLocationComprehensiveQuickCheckSpec :: TestTree
sourceLocationComprehensiveQuickCheckSpec = testGroup "SourceLocation Comprehensive QuickCheck Tests"
  [ sourcePosProperties
  , sourceSpanProperties
  , locatedValueProperties
  , locationTrackerProperties
  , positionAdvancementProperties
  ]

-- | Properties for SourcePos
sourcePosProperties :: TestTree
sourcePosProperties = testGroup "SourcePos Properties"
  [ testProperty "startPos has correct values" $
      posLine startPos == 1 && posColumn startPos == 1 && posOffset startPos == 0
  
  , testProperty "posAt creates position with correct line L.and column" $
      \line col -> line > 0 && col > 0 ==>
        let pos = posAt line col
        in posLine pos == line && posColumn pos == col
  
  , testProperty "posAtLineCol creates position with correct values" $
      \line col offset -> line > 0 && col > 0 && offset >= 0 ==>
        let pos = posAtLineCol line col offset
        in posLine pos == line && posColumn pos == col && posOffset pos == offset
  
  , testProperty "posAfter newline increments line L.and resets column" $
      \pos ->
        let newPos = posAfter '\n' pos
        in posLine newPos == posLine pos + 1 && posColumn newPos == 1 &&
           posOffset newPos == posOffset pos + 1
  
  , testProperty "posAfter tab aligns to next tab stop" $
      \pos ->
        let newPos = posAfter '\t' pos
            expectedCol = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
        in posColumn newPos == expectedCol &&
           posOffset newPos == posOffset pos + 1
  
  , testProperty "posAfter regular character increments column" $
      \pos c -> c /= '\n' && c /= '\t' ==>
        let newPos = posAfter c pos
        in posLine newPos == posLine pos &&
           posColumn newPos == posColumn pos + 1 &&
           posOffset newPos == posOffset pos + 1
  ]

-- | Properties for SourceSpan
sourceSpanProperties :: TestTree
sourceSpanProperties = testGroup "SourceSpan Properties"
  [ testProperty "emptySpan has same start L.and end" $
      \pos ->
        let span = emptySpan pos
        in spanStart span == pos && spanEnd span == pos
  
  , testProperty "spanFrom creates empty span at position" $
      \pos ->
        let span = spanFrom pos
        in spanStart span == pos && spanEnd span == pos
  
  , testProperty "spanTo creates empty span at position" $
      \pos ->
        let span = spanTo pos
        in spanStart span == pos && spanEnd span == pos
  
  , testProperty "spanBetween creates span with correct start L.and end" $
      \start end ->
        let span = spanBetween start end
        in spanStart span == start && spanEnd span == end
  
  , testProperty "mergeSpans creates span covering both spans" $
      \span1 span2 ->
        let merged = mergeSpans span1 span2
        in spanStart merged == min (spanStart span1) (spanStart span2) &&
           spanEnd merged == max (spanEnd span1) (spanEnd span2)
  
  , testProperty "isValidSpan checks start <= end" $
      \start end ->
        let span = spanBetween start end
        in isValidSpan span == (start <= end)
  
  , testProperty "mergeSpans is commutative" $
      \span1 span2 ->
        let merged1 = mergeSpans span1 span2
            merged2 = mergeSpans span2 span1
        in merged1 == merged2
  ]

-- | Properties for Located values
locatedValueProperties :: TestTree
locatedValueProperties = testGroup "Located Value Properties"
  [ testProperty "locatedAt creates located value with correct position" $
      \pos value ->
        let located = locatedAt pos value
        in locPos located == pos && locValue located == value &&
           locSpan located == emptySpan pos
  
  , testProperty "locatedWithSpan creates located value with correct span" $
      \span value ->
        let located = locatedWithSpan span value
        in locSpan located == span && locValue located == value &&
           locPos located == spanStart span
  
  , testProperty "locatedValue extracts the value" $
      \pos value ->
        let located = locatedAt pos value
        in locatedValue located == value
  
  , testProperty "locatedSpan extracts the span" $
      \pos value ->
        let located = locatedAt pos value
        in locatedSpan located == emptySpan pos
  
  , testProperty "locatedPos extracts the start position" $
      \pos value ->
        let located = locatedAt pos value
        in locatedPos located == pos
  
  , testProperty "mapLocated applies function to value" $
      \pos value ->
        let located = locatedAt pos value
            doubled = mapLocated (*2) located
        in locValue doubled == value * 2 &&
           locPos doubled == pos &&
           locSpan doubled == emptySpan pos
  ]

-- | Properties for LocationTracker
locationTrackerProperties :: TestTree
locationTrackerProperties = testGroup "LocationTracker Properties"
  [ testProperty "runLocationTracker starts at startPos" $
      runLocationTracker getCurrentPos == startPos
  
  , testProperty "setCurrentPos changes current position" $
      \pos ->
        let (_, finalPos) = withLocationTracking startPos (setCurrentPos pos >> getCurrentPos)
        in finalPos == pos
  
  , testProperty "markSpanStart returns current position" $
      \pos ->
        let (startPosResult, _) = withLocationTracking pos markSpanStart
        in startPosResult == pos
  
  , testProperty "markSpanEnd creates span from start to current" $
      \startPos endPos ->
        let (span, _) = withLocationTracking startPos $
                           setCurrentPos endPos >> markSpanEnd startPos
        in spanStart span == startPos && spanEnd span == endPos
  ]

-- | Properties for position advancement
positionAdvancementProperties :: TestTree
positionAdvancementProperties = testGroup "Position Advancement Properties"
  [ testProperty "advancePos equals posAfter" $
      \pos c ->
        let result1 = advancePos c pos
            result2 = posAfter c pos
        in result1 == result2
  
  , testProperty "advancePosBy advances by multiple characters" $
      \pos chars ->
        let result = advancePosBy chars pos
            expected = L.foldl (flip posAfter) pos chars
        in result == expected
  
  , testProperty "advancePosBy empty string returns original position" $
      \pos -> advancePosBy "" pos == pos
  
  , testProperty "advancePosByLine increments line L.and resets column" $
      \pos numLines ->
        let result = advancePosByLine numLines pos
        in posLine result == posLine pos + numLines &&
           posColumn result == 1
  
  , testProperty "advancePosByLine with 0 returns original position" $
      \pos -> advancePosByLine 0 pos == pos
  
  , testProperty "position advancement is consistent for newlines" $
      \pos ->
        let afterNewline = posAfter '\n' pos
            afterLine = advancePosByLine 1 pos
        in posLine afterNewline == posLine afterLine &&
           posColumn afterNewline == posColumn afterLine
  ]

-- Additional utility functions for testing
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 1000)
    col <- choose (1, 1000)
    offset <- choose (0, 1000000)
    return $ SourcePos line col offset

instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    return $ SourceSpan start end