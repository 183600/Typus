{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewCoreSourceLocationQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.QuickCheck (conjoin, (===), Property, property, forAll, choose, listOf1, elements, Positive(..))

import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), startPos, posAfter, 
                       posAt, emptySpan, spanFrom, spanTo, spanBetween, mergeSpans, 
                       isValidSpan, advancePosByText, comparePos, locatedAt, 
                       locatedWithSpan, locatedValue, locatedSpan, mapLocated)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Char (isAlphaNum, isAlpha, isSpace, isControl)
import Data.Either (isLeft, isRight)
import Control.Monad (replicateM)
import qualified Data.Map.Strict as Map

-- Test 1: 测试SourcePos的基本属性
prop_sourcePos_basic :: Positive Int -> Positive Int -> Positive Int -> Property
prop_sourcePos_basic (Positive line) (Positive col) (Positive offset) =
  let pos = SourcePos line col offset
  in conjoin 
     [ property $ posLine pos === line
     , property $ posColumn pos === col
     , property $ posOffset pos === offset
     ]

-- Test 2: 测试posAfter函数对不同字符的处理
prop_posAfter_characters :: Char -> Positive Int -> Positive Int -> Positive Int -> Property
prop_posAfter_characters c (Positive line) (Positive col) (Positive offset) =
  let pos = SourcePos line col offset
      newPos = posAfter c pos
  in conjoin 
     [ property $ posOffset newPos === offset + 1
     , property $ (c == '\n') ==> (posLine newPos == line + 1 && posColumn newPos == 1)
     , property $ (c == '\t') ==> (posLine newPos == line && posColumn newPos == ((col - 1) `div` 8 + 1) * 8 + 1)
     , property $ (c /= '\n' && c /= '\t') ==> (posLine newPos == line && posColumn newPos == col + 1)
     ]

-- Test 3: 测试advancePosByText函数
prop_advancePosByText :: String -> Positive Int -> Positive Int -> Positive Int -> Property
prop_advancePosByText text (Positive line) (Positive col) (Positive offset) =
  let pos = SourcePos line col offset
      textText = T.pack text
      finalPos = advancePosByText textText pos
  in conjoin 
     [ property $ posOffset finalPos >= offset
     , null text ==> property $ finalPos === pos
     , not (null text) ==> property $ posOffset finalPos > offset
     , all (/= '\n') text ==> property $ posLine finalPos === line
     ]

-- Test 4: 测试SourceSpan的基本属性
prop_sourceSpan_basic :: Positive Int -> Positive Int -> Positive Int -> 
                         Positive Int -> Positive Int -> Positive Int -> Property
prop_sourceSpan_basic (Positive startLine) (Positive startCol) (Positive startOffset)
                       (Positive endLine) (Positive endCol) (Positive endOffset) =
  let startPos' = SourcePos startLine startCol startOffset
      endPos' = SourcePos endLine endCol endOffset
      span = SourceSpan startPos' endPos'
  in conjoin 
     [ property $ spanStart span === startPos'
     , property $ spanEnd span === endPos'
     ]

-- Test 5: 测试spanBetween函数
prop_spanBetween :: Positive Int -> Positive Int -> Positive Int -> 
                   Positive Int -> Positive Int -> Positive Int -> Property
prop_spanBetween (Positive line1) (Positive col1) (Positive offset1)
                 (Positive line2) (Positive col2) (Positive offset2) =
  let pos1 = SourcePos line1 col1 offset1
      pos2 = SourcePos line2 col2 offset2
      span = spanBetween pos1 pos2
  in conjoin 
     [ property $ spanStart span === pos1
     , property $ spanEnd span === pos2
     ]

-- Test 6: 测试mergeSpans函数
prop_mergeSpans :: Positive Int -> Positive Int -> Positive Int -> 
                  Positive Int -> Positive Int -> Positive Int -> 
                  Positive Int -> Positive Int -> Positive Int -> Property
prop_mergeSpans (Positive line1) (Positive col1) (Positive offset1)
                (Positive line2) (Positive col2) (Positive offset2)
                (Positive line3) (Positive col3) (Positive offset3) =
  let pos1 = SourcePos line1 col1 offset1
      pos2 = SourcePos line2 col2 offset2
      pos3 = SourcePos line3 col3 offset3
      span1 = spanBetween pos1 pos2
      span2 = spanBetween pos2 pos3
      mergedSpan = mergeSpans span1 span2
  in conjoin 
     [ property $ spanStart mergedSpan === pos1
     , property $ spanEnd mergedSpan === pos3
     ]

-- 测试套件
tests :: TestTree
tests = testGroup "New Core SourceLocation QuickCheck Tests"
  [ testProperty "SourcePos basic" prop_sourcePos_basic
  , testProperty "PosAfter characters" prop_posAfter_characters
  , testProperty "AdvancePosByText" prop_advancePosByText
  , testProperty "SourceSpan basic" prop_sourceSpan_basic
  , testProperty "SpanBetween" prop_spanBetween
  , testProperty "MergeSpans" prop_mergeSpans
  ]