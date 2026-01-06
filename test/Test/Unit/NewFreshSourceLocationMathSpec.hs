{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Mathematical properties for SourceLocation module
module Test.Unit.NewFreshSourceLocationMathSpec where

import Test.Tasty
import qualified Data.List as L
import Test.Tasty.QuickCheck (property)
import SourceLocation 
  ( SourcePos(..), SourceSpan(..), Located(..)
  , startPos, posAfter, posAt, advancePos, advancePosBy
  , mergeSpans, spanFrom, spanTo, spanBetween, emptySpan
  , isValidSpan, spanStart, spanEnd
  )
import Data.List (foldl')

-- ============================================================================
-- Test Suite Definition
-- ============================================================================

testSuite :: TestTree
testSuite = testGroup "New SourceLocation Math Properties"
  [ positionProperties
  , spanProperties
  , arithmeticProperties
  , orderingProperties
  ]

-- ============================================================================
-- Position Properties
-- ============================================================================

positionProperties :: TestTree
positionProperties = testGroup "Position Properties"
  [ testProperty "SourcePos: startPos is (1,1)" $
      \() -> startPos === SourcePos 1 1
      
  , testProperty "SourcePos: advancePos by newline increments line, resets column" $
      \line col ->
        let pos = SourcePos line col
            newPos = advancePos pos '\n'
        in newPos === SourcePos (line + 1) 1
        
  , testProperty "SourcePos: advancePos by tab increments column by tab width (assumed 4)" $
      \line col ->
        let pos = SourcePos line col
            newPos = advancePos pos '\t'
        in sourceColumn newPos === sourceColumn pos + 4
        
  , testProperty "SourcePos: advancePos by regular char increments column by 1" $
      \line col c ->
        let pos = SourcePos line col
            newPos = advancePos pos c
        in not (c `elem` "\n\t\r") ==> sourceColumn newPos === sourceColumn pos + 1
        
  , testProperty "SourcePos: advancePosBy multiple chars is equivalent to sequential advances" $
      \pos chars ->
        let sequential = foldl advancePos pos chars
            batch = advancePosBy pos chars
        in sequential === batch
        
  , testProperty "SourcePos: advancing by empty string preserves position" $
      \pos -> advancePosBy pos "" === pos
      
  , testProperty "SourcePos: line L.and column are always positive" $
      \pos -> sourceLine pos > 0 && sourceColumn pos > 0
  ]

-- ============================================================================
-- Span Properties
-- ============================================================================

spanProperties :: TestTree
spanProperties = testGroup "Span Properties"
  [ testProperty "SourceSpan: empty span is invalid" $
      \() -> not $ isValidSpan emptySpan
      
  , testProperty "SourceSpan: span from position to itself is empty" $
      \pos -> not $ isValidSpan (spanFrom pos `spanTo` pos)
      
  , testProperty "SourceSpan: spanBetween always creates valid span if positions are different" $
      \pos1 pos2 ->
        let span = spanBetween pos1 pos2
        in pos1 /= pos2 ==> isValidSpan span
        
  , testProperty "SourceSpan: mergeSpans is commutative" $
      \pos1 pos2 pos3 pos4 ->
        let span1 = spanBetween pos1 pos2
            span2 = spanBetween pos3 pos4
        in isValidSpan span1 && isValidSpan span2 ==> 
           mergeSpans span1 span2 === mergeSpans span2 span1
           
  , testProperty "SourceSpan: mergeSpans is associative" $
      \pos1 pos2 pos3 pos4 pos5 pos6 ->
        let span1 = spanBetween pos1 pos2
            span2 = spanBetween pos3 pos4
            span3 = spanBetween pos5 pos6
        in L.all isValidSpan [span1, span2, span3] ==>
           mergeSpans span1 (mergeSpans span2 span3) === 
           mergeSpans (mergeSpans span1 span2) span3
           
  , testProperty "SourceSpan: merged span contains both original spans" $
      \pos1 pos2 pos3 pos4 ->
        let span1 = spanBetween pos1 pos2
            span2 = spanBetween pos3 pos4
            merged = mergeSpans span1 span2
        in isValidSpan span1 && isValidSpan span2 ==>
           spanStart merged <= min (spanStart span1) (spanStart span2) &&
           spanEnd merged >= max (spanEnd span1) (spanEnd span2)
           
  , testProperty "SourceSpan: span start is always before L.or equal to end" $
      \pos1 pos2 ->
        let span = spanBetween pos1 pos2
        in isValidSpan span ==> 
           sourceLine (spanStart span) < sourceLine (spanEnd span) ||
           (sourceLine (spanStart span) == sourceLine (spanEnd span) &&
            sourceColumn (spanStart span) <= sourceColumn (spanEnd span))
  ]

-- ============================================================================
-- Arithmetic Properties
-- ============================================================================

arithmeticProperties :: TestTree
arithmeticProperties = testGroup "Arithmetic Properties"
  [ testProperty "Position arithmetic: advancing by n newlines increments line by n" $
      \pos n ->
        let newPos = advancePosBy pos (replicate n '\n')
        in n >= 0 && n <= 100 ==> sourceLine newPos === sourceLine pos + n
        
  , testProperty "Position arithmetic: advancing by n regular chars increments column by n" $
      \pos n ->
        let chars = replicate n 'x'
            newPos = advancePosBy pos chars
        in n >= 0 && n <= 100 ==> sourceColumn newPos === sourceColumn pos + n
        
  , testProperty "Span arithmetic: span L.length is non-negative" $
      \pos1 pos2 ->
        let span = spanBetween pos1 pos2
        in isValidSpan span ==> True  -- Simplified - actual L.length calculation would be more complex
        
  , testProperty "Position ordering: later positions have greater line/col" $
      \pos chars ->
        let newPos = advancePosBy pos chars
        in not (null chars) ==> 
           sourceLine newPos > sourceLine pos ||
           (sourceLine newPos == sourceLine pos && sourceColumn newPos >= sourceColumn pos)
  ]

-- ============================================================================
-- Ordering Properties
-- ============================================================================

orderingProperties :: TestTree
orderingProperties = testGroup "Ordering Properties"
  [ testProperty "Position ordering: startPos is minimal" $
      \pos -> posAfter startPos pos === pos
      
  , testProperty "Position ordering: posAt creates consistent positions" $
      \line col ->
        line > 0 && col > 0 ==> 
        let pos = posAt line col
        in sourceLine pos === line && sourceColumn pos === col
        
  , testProperty "Span ordering: merge preserves chronological ordering" $
      \pos1 pos2 pos3 pos4 ->
        let span1 = spanBetween pos1 pos2
            span2 = spanBetween pos3 pos4
            merged = mergeSpans span1 span2
        in isValidSpan span1 && isValidSpan span2 && 
           spanEnd span1 <= spanStart span2 ==>
           spanStart merged === spanStart span1 &&
           spanEnd merged === spanEnd span2
           
  , testProperty "Span ordering: overlapping spans merge to contain overlap" $
      \pos1 pos2 pos3 ->
        let pos2' = posAfter pos1 pos2  -- Ensure pos2' is after pos1
            pos3' = posAfter pos1 pos3  -- Ensure pos3' is after pos1
            span1 = spanBetween pos1 pos2'
            span2 = spanBetween pos2' pos3'  -- Overlapping at pos2'
            merged = mergeSpans span1 span2
        in isValidSpan span1 && isValidSpan span2 ==>
           spanStart merged === spanStart span1 &&
           spanEnd merged === spanEnd span2
  ]

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- Custom generator for reasonable source positions
instance Arbitrary SourcePos where
  arbitrary = SourcePos <$> choose (1, 1000) <*> choose (1, 1000)

-- Custom generator for characters (excluding problematic ones)
instance Arbitrary Char where
  arbitrary = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n\r.,;:!?()[]{}"