{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewAdditionalSourceLocationQuickCheckSpec where


import Test.Tasty
import Test.Tasty.QuickCheck



import Test.Tasty
import Test.Tasty.QuickCheck

import Test.QuickCheck (conjoin, (===), Property, property, forAll, choose, listOf1, elements, Positive(..))

import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), startPos, posAfter, 
                       posAt, emptySpan, spanFrom, spanTo, spanBetween, mergeSpans, 
                       isValidSpan, advancePosByText, comparePos, locatedAt, 
                       locatedWithSpan, locatedValue, locatedSpan, locatedPos, mapLocated)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Char (isAlphaNum, isAlpha, isSpace, isControl)
import Data.Either (isLeft, isRight)
import Control.Monad (replicateM)
import qualified Data.Map.Strict as Map

-- Test 1: 测试SourcePos的比较操作
prop_sourcepos_comparison :: Positive Int -> Positive Int -> Positive Int -> 
                            Positive Int -> Positive Int -> Positive Int -> Property
prop_sourcepos_comparison (Positive line1) (Positive col1) (Positive offset1)
                          (Positive line2) (Positive col2) (Positive offset2) =
  let pos1 = SourcePos line1 col1 offset1
      pos2 = SourcePos line2 col2 offset2
      comparison = comparePos pos1 pos2
  in conjoin 
     [ property $ line1 < line2 ==> comparison === LT
     , property $ line1 > line2 ==> comparison === GT
     , property $ (line1 == line2 && col1 < col2) ==> comparison === LT
     , property $ (line1 == line2 && col1 > col2) ==> comparison === GT
     , property $ (line1 == line2 && col1 == col2 && offset1 < offset2) ==> comparison === LT
     , property $ (line1 == line2 && col1 == col2 && offset1 > offset2) ==> comparison === GT
     , property $ (line1 == line2 && col1 == col2 && offset1 == offset2) ==> comparison === EQ
     ]

-- Test 2: 测试SourceSpan的有效性
prop_sourcespan_validity :: Positive Int -> Positive Int -> Positive Int -> 
                           Positive Int -> Positive Int -> Positive Int -> Property
prop_sourcespan_validity (Positive startLine) (Positive startCol) (Positive startOffset)
                         (Positive endLine) (Positive endCol) (Positive endOffset) =
  let startPos' = SourcePos startLine startCol startOffset
      endPos' = SourcePos endLine endCol endOffset
      span = SourceSpan startPos' endPos'
      valid = isValidSpan span
  in conjoin 
     [ property $ startLine < endLine ==> valid === True
     , property $ startLine > endLine ==> valid === False
     , property $ (startLine == endLine && startCol <= endCol) ==> valid === True
     , property $ (startLine == endLine && startCol > endCol) ==> valid === False
     ]

-- Test 3: 测试advancePosByText对换行符的处理
prop_advanceposbytext_newlines :: String -> Positive Int -> Positive Int -> Positive Int -> Property
prop_advanceposbytext_newlines text (Positive line) (Positive col) (Positive offset) =
  let newlineCount = length $ filter (== '\n') text
      pos = SourcePos line col offset
      textText = T.pack text
      finalPos = advancePosByText textText pos
  in conjoin 
     [ property $ posLine finalPos >= line
     , property $ (newlineCount == 0) ==> posLine finalPos === line
     , property $ (newlineCount > 0) ==> posLine finalPos > line
     , property $ posOffset finalPos >= offset
     ]

-- Test 4: 测试spanBetween的顺序无关性
prop_spanbetween_order :: Positive Int -> Positive Int -> Positive Int -> 
                        Positive Int -> Positive Int -> Positive Int -> Property
prop_spanbetween_order (Positive line1) (Positive col1) (Positive offset1)
                      (Positive line2) (Positive col2) (Positive offset2) =
  let pos1 = SourcePos line1 col1 offset1
      pos2 = SourcePos line2 col2 offset2
      span1 = spanBetween pos1 pos2
      span2 = spanBetween pos2 pos1
  in conjoin 
     [ property $ spanStart span1 === min pos1 pos2
     , property $ spanEnd span1 === max pos1 pos2
     , property $ spanStart span2 === min pos1 pos2
     , property $ spanEnd span2 === max pos1 pos2
     , property $ span1 === span2
     ]

-- Test 5: 测试mergeSpans的结合性
prop_mergespans_associative :: Positive Int -> Positive Int -> Positive Int -> 
                             Positive Int -> Positive Int -> Positive Int -> 
                             Positive Int -> Positive Int -> Positive Int -> Property
prop_mergespans_associative (Positive line1) (Positive col1) (Positive offset1)
                           (Positive line2) (Positive col2) (Positive offset2)
                           (Positive line3) (Positive col3) (Positive offset3) =
  let pos1 = SourcePos line1 col1 offset1
      pos2 = SourcePos line2 col2 offset2
      pos3 = SourcePos line3 col3 offset3
      span1 = spanBetween pos1 pos2
      span2 = spanBetween pos2 pos3
      span3 = spanBetween pos1 pos3
      merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span1 span3
  in conjoin 
     [ property $ spanStart merged1 === pos1
     , property $ spanEnd merged1 === pos3
     , property $ spanStart merged2 === pos1
     , property $ spanEnd merged2 === pos3
     , property $ merged1 === merged2
     ]

-- Test 6: 测试Located值的基本属性
prop_located_basic :: String -> Positive Int -> Positive Int -> Positive Int -> Property
prop_located_basic value (Positive line) (Positive col) (Positive offset) =
  let pos = SourcePos line col offset
      span = SourceSpan pos pos
      locatedValue' = locatedAt pos value
      locatedWithSpan' = locatedWithSpan span value
  in conjoin 
     [ property $ locatedValue locatedValue' === value
     , property $ locatedPos locatedValue' === pos
     , property $ locatedValue locatedWithSpan' === value
     , property $ locatedSpan locatedWithSpan' === span
     ]

-- 测试套件
tests :: TestTree
tests = testGroup "New Additional SourceLocation QuickCheck Tests"
  [ testProperty "SourcePos comparison" prop_sourcepos_comparison
  , testProperty "SourceSpan validity" prop_sourcespan_validity
  , testProperty "AdvancePosByText newlines" prop_advanceposbytext_newlines
  , testProperty "SpanBetween order" prop_spanbetween_order
  , testProperty "MergeSpans associative" prop_mergespans_associative
  , testProperty "Located basic" prop_located_basic
  ]