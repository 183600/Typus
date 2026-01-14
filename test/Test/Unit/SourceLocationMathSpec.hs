{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.SourceLocationMathSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import SourceLocation (SourcePos(..), SourceSpan(..))

-- Test source location mathematical properties
tests :: TestTree
tests = testGroup "Source Location Math Tests"
  [ testGroup "SourcePos properties"
    [ testProperty "position comparison is transitive" $
        \pos1 pos2 pos3 ->
          let p1 = SourcePos (abs pos1) 0
              p2 = SourcePos (abs pos2) 0
              p3 = SourcePos (abs pos3) 0
          in if p1 <= p2 && p2 <= p3 then p1 <= p3 else property True
    
    , testProperty "line numbers are non-negative" $
        \line col -> sourceLine (SourcePos (abs line) (abs col)) >= 0
    
    , testProperty "column numbers are non-negative" $
        \line col -> sourceColumn (SourcePos (abs line) (abs col)) >= 0
    
    , testProperty "position equality is reflexive" $
        \line col -> 
          let pos = SourcePos (abs line) (abs col)
          in pos == pos
    
    , testProperty "position equality is symmetric" $
        \line1 col1 line2 col2 ->
          let pos1 = SourcePos (abs line1) (abs col1)
              pos2 = SourcePos (abs line2) (abs col2)
          in if pos1 == pos2 then pos2 == pos1 else property True
    
    , testProperty "position equality is transitive" $
        \line1 col1 line2 col2 line3 col3 ->
          let pos1 = SourcePos (abs line1) (abs col1)
              pos2 = SourcePos (abs line2) (abs col2)
              pos3 = SourcePos (abs line3) (abs col3)
          in if pos1 == pos2 && pos2 == pos3 then pos1 == pos3 else property True
    
    , testProperty "same line positions can be ordered by column" $
        \line col1 col2 ->
          let pos1 = SourcePos (abs line) (abs col1)
              pos2 = SourcePos (abs line) (abs col2)
          in if sourceColumn pos1 <= sourceColumn pos2 then pos1 <= pos2 else property True
    
    , testProperty "different line positions can be ordered by line" $
        \line1 line2 col ->
          let pos1 = SourcePos (abs line1) col
              pos2 = SourcePos (abs line2) col
          in if sourceLine pos1 <= sourceLine pos2 then pos1 <= pos2 else property True
    
    , testProperty "minimum line is 0" $
        \col -> SourcePos 0 col <= SourcePos (abs (1 :: Int)) col
    
    , testProperty "minimum column is 0" $
        \line -> SourcePos line 0 <= SourcePos line (abs (1 :: Int))
    ]
  
  , testGroup "SourceSpan properties"
    [ testProperty "span start is before or equal to end" $
        \startLine startCol endLine endCol ->
          let start = SourcePos (abs startLine) (abs startCol)
              end = SourcePos (abs (startLine + abs endLine)) (abs (startCol + abs endCol))
              span = SourceSpan start end
          in spanStart span <= spanEnd span
    
    , testProperty "span length is non-negative" $
        \startLine startCol endLine endCol ->
          let start = SourcePos (abs startLine) (abs startCol)
              end = SourcePos (abs (startLine + abs endLine)) (abs (startCol + abs endCol))
              lineDiff = sourceLine end - sourceLine start
              colDiff = sourceColumn end - sourceColumn start
          in lineDiff >= 0 && colDiff >= 0
    
    , testProperty "span contains its start position" $
        \startLine startCol endLine endCol ->
          let start = SourcePos (abs startLine) (abs startCol)
              end = SourcePos (abs (startLine + abs endLine)) (abs (startCol + abs endCol))
              span = SourceSpan start end
          in spanContains span start
    
    , testProperty "span contains its end position" $
        \startLine startCol endLine endCol ->
          let start = SourcePos (abs startLine) (abs startCol)
              end = SourcePos (abs (startLine + abs endLine)) (abs (startCol + abs endCol))
              span = SourceSpan start end
          in spanContains span end
    
    , testProperty "empty span has same start and end" $
        \line col ->
          let pos = SourcePos (abs line) (abs col)
              span = SourceSpan pos pos
          in spanStart span == spanEnd span
    
    , testProperty "span equality is reflexive" $
        \startLine startCol endLine endCol ->
          let start = SourcePos (abs startLine) (abs startCol)
              end = SourcePos (abs (startLine + abs endLine)) (abs (startCol + abs endCol))
              span = SourceSpan start end
          in span == span
    
    , testProperty "span equality is symmetric" $
        \startLine1 startCol1 endLine1 endCol1 startLine2 startCol2 endLine2 endCol2 ->
          let start1 = SourcePos (abs startLine1) (abs startCol1)
              end1 = SourcePos (abs (startLine1 + abs endLine1)) (abs (startCol1 + abs endCol1))
              span1 = SourceSpan start1 end1
              start2 = SourcePos (abs startLine2) (abs startCol2)
              end2 = SourcePos (abs (startLine2 + abs endLine2)) (abs (startCol2 + abs endCol2))
              span2 = SourceSpan start2 end2
          in if span1 == span2 then span2 == span1 else property True
    
    , testProperty "span equality is transitive" $
        \line1 col1 line2 col2 line3 col3 ->
          let pos1 = SourcePos (abs line1) (abs col1)
              pos2 = SourcePos (abs line2) (abs col2)
              pos3 = SourcePos (abs line3) (abs col3)
              span1 = SourceSpan pos1 pos2
              span2 = SourceSpan pos2 pos3
              span3 = SourceSpan pos1 pos3
          in if span1 == span2 && span2 == span3 then span1 == span3 else property True
    
    , testProperty "nested spans maintain containment" $
        \startLine startCol midLine midCol endLine endCol ->
          let start = SourcePos (abs startLine) (abs startCol)
              middle = SourcePos (abs (startLine + abs midLine)) (abs (startCol + abs midCol))
              end = SourcePos (abs (startLine + abs endLine)) (abs (startCol + abs endCol))
              outer = SourceSpan start end
              inner = SourceSpan start middle
          in spanContains outer inner
    
    , testProperty "adjacent spans can be merged" $
        \startLine startCol midLine midCol endLine endCol ->
          let start = SourcePos (abs startLine) (abs startCol)
              middle = SourcePos (abs (startLine + abs midLine)) (abs (startCol + abs midCol))
              end = SourcePos (abs (startLine + abs endLine)) (abs (startCol + abs endCol))
              span1 = SourceSpan start middle
              span2 = SourceSpan middle end
              merged = SourceSpan start end
          in spanContains merged span1 && spanContains merged span2
    ]
  ]

-- Helper function to check if a span contains a position
spanContains :: SourceSpan -> SourcePos -> Bool
spanContains span pos = spanStart span <= pos && pos <= spanEnd span