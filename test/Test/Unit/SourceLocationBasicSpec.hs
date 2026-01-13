module Test.Unit.SourceLocationBasicSpec where

import Test.Tasty
import Test.Tasty.HUnit
import SourceLocation
  ( SourcePos(..), SourceSpan(..), startPos
  , posAfter, posAt, posAtLineCol
  , emptySpan, spanFrom, spanTo, spanBetween
  )

tests :: TestTree
tests = testGroup "SourceLocation Basic Tests"
  [ testCase "startPos has correct values" $ do
      posLine startPos @?= 1
      posColumn startPos @?= 1
      posOffset startPos @?= 0
      
  , testCase "posAfter with newline" $ do
      let pos = startPos { posLine = 5, posColumn = 10, posOffset = 100 }
      let newPos = posAfter '\n' pos
      posLine newPos @?= 6
      posColumn newPos @?= 1
      posOffset newPos @?= 101
      
  , testCase "posAfter with tab" $ do
      let pos1 = startPos { posColumn = 1, posOffset = 0 }
      let newPos1 = posAfter '\t' pos1
      posColumn newPos1 @?= 9
      posOffset newPos1 @?= 1
      
      let pos2 = startPos { posColumn = 8, posOffset = 7 }
      let newPos2 = posAfter '\t' pos2
      posColumn newPos2 @?= 9
      posOffset newPos2 @?= 8
      
      let pos3 = startPos { posColumn = 10, posOffset = 9 }
      let newPos3 = posAfter '\t' pos3
      posColumn newPos3 @?= 17
      posOffset newPos3 @?= 10
      
  , testCase "posAfter with regular character" $ do
      let pos = startPos { posLine = 3, posColumn = 5, posOffset = 20 }
      let newPos = posAfter 'a' pos
      posLine newPos @?= 3
      posColumn newPos @?= 6
      posOffset newPos @?= 21
      
  , testCase "posAt creates correct position" $ do
      let pos = posAt 10 20 100
      posLine pos @?= 10
      posColumn pos @?= 20
      posOffset pos @?= 100
      
  , testCase "posAtLineCol creates correct position" $ do
      let pos = posAtLineCol 15 25 200
      posLine pos @?= 15
      posColumn pos @?= 25
      posOffset pos @?= 200
      
  , testCase "emptySpan properties" $ do
      let span = emptySpan
      -- emptySpan should have invalid positions
      posLine (spanStart span) @?= 0
      posColumn (spanStart span) @?= 0
      posOffset (spanStart span) @?= 0
      
  , testCase "spanFrom creates correct span" $ do
      let pos = posAt 5 10 50
      let span = spanFrom pos
      spanStart span @?= pos
      spanEnd span @?= pos
      
  , testCase "spanTo creates correct span" $ do
      let pos = posAt 8 15 80
      let span = spanTo pos
      spanStart span @?= pos
      spanEnd span @?= pos
      
  , testCase "spanBetween creates correct span" $ do
      let start = posAt 3 5 25
      let end = posAt 7 12 75
      let span = spanBetween start end
      spanStart span @?= start
      spanEnd span @?= end
      
  , testCase "spanBetween with same position" $ do
      let pos = posAt 4 8 40
      let span = spanBetween pos pos
      spanStart span @?= pos
      spanEnd span @?= pos
      
  , testCase "spanBetween with different lines" $ do
      let start = posAt 2 5 15
      let end = posAt 5 3 50
      let span = spanBetween start end
      spanStart span @?= start
      spanEnd span @?= end
      
  , testCase "position comparison" $ do
      let pos1 = posAt 1 1 0
      let pos2 = posAt 1 2 1
      let pos3 = posAt 2 1 10
      
      assertBool "pos1 < pos2" $ pos1 < pos2
      assertBool "pos2 < pos3" $ pos2 < pos3
      assertBool "pos1 < pos3" $ pos1 < pos3
      assertBool "pos1 == pos1" $ pos1 == pos1
  ]