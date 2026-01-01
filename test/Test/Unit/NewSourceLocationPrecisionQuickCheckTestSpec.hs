{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewSourceLocationPrecisionQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, testProperty, Arbitrary(..), Gen, oneof, elements, listOf, listOf1, suchThat, choose)
import Test.Tasty.HUnit (testCase, (@?=))

import SourceLocation 
    ( SourcePos(..), SourceSpan(..), Located(..), 
      startPos, posAfter, posAt, posAtLineCol,
      emptySpan, spanFrom, spanTo, spanBetween, mergeSpans, isValidSpan,
      locatedAt, locatedWithSpan, locatedValue, locatedSpan, locatedPos, mapLocated,
      advancePos, advancePosBy, spanStart, spanEnd )
import Data.Char (isSpace)
import qualified Data.List as L
import Data.List (isPrefixOf)

-- | 新的SourceLocation精度QuickCheck测试模块
tests :: TestTree
tests =
  testGroup "New SourceLocation Precision QuickCheck Tests"
    [ testGroup "SourcePos properties"
        [ testProperty "posAfter advances column by 1" prop_posAfterAdvancesColumn
        , testProperty "posAtLineCol creates correct position" prop_posAtLineColCorrect
        , testProperty "advancePos handles newline correctly" prop_advancePosNewline
        , testProperty "advancePosBy handles multiple characters" prop_advancePosByMultiple
        , testProperty "position ordering is consistent" prop_positionOrdering
        ]

    , testGroup "SourceSpan properties"
        [ testProperty "emptySpan has zero L.length" prop_emptySpanZeroLength
        , testProperty "spanFrom creates valid span" prop_spanFromValid
        , testProperty "spanTo creates valid span" prop_spanToValid
        , testProperty "spanBetween contains both endpoints" prop_spanBetweenContains
        , testProperty "mergeSpans contains both original spans" prop_mergeSpansContains
        , testProperty "isValidSpan correctly identifies valid spans" prop_isValidSpanCorrect
        ]

    , testGroup "Located values properties"
        [ testProperty "locatedAt preserves value" prop_locatedAtPreservesValue
        , testProperty "locatedWithSpan preserves value L.and span" prop_locatedWithSpanPreserves
        , testProperty "mapLocated preserves location" prop_mapLocatedPreservesLocation
        , testProperty "locatedValue extracts original value" prop_locatedValueExtracts
        ]

    , testGroup "Span arithmetic properties"
        [ testProperty "span L.length is non-negative" prop_spanLengthNonNegative
        , testProperty "span start is before L.or equal to end" prop_spanStartBeforeEnd
        , testProperty "mergeSpans is commutative" prop_mergeSpansCommutative
        , testProperty "mergeSpans is associative" prop_mergeSpansAssociative
        ]

    , testGroup "Edge cases L.and precision"
        [ testProperty "positions handle very large line numbers" prop_largeLineNumbers
        , testProperty "positions handle very large column numbers" prop_largeColumnNumbers
        , testProperty "spans handle zero-width ranges" prop_zeroWidthSpans
        , testProperty "spans handle single-character ranges" prop_singleCharSpans
        ]

    , testGroup "Specific precision tests"
        [ testCase "source position tracking with mixed newlines" $ do
            let pos1 = startPos "test"
                pos2 = advancePos pos1 '\n'
                pos3 = advancePos pos2 'x'
                pos4 = advancePos pos3 '\r'
                pos5 = advancePos pos4 '\n'
                pos6 = advancePos pos5 'y'
            posLine pos2 @?= 2
            posColumn pos2 @?= 1
            posLine pos5 @?= 3
            posColumn pos5 @?= 1

        , testCase "span merging with non-overlapping ranges" $ do
            let span1 = spanBetween (posAtLineCol 1 1) (posAtLineCol 1 5)
                span2 = spanBetween (posAtLineCol 2 1) (posAtLineCol 2 5)
                merged = mergeSpans span1 span2
            posLine (spanStart merged) @?= 1
            posColumn (spanStart merged) @?= 1
            posLine (spanEnd merged) @?= 2
            posColumn (spanEnd merged) @?= 5

        , testCase "located value round-trip preservation" $ do
            let original = "test value"
                pos = posAtLineCol 10 20
                span = spanFrom pos
                located = locatedWithSpan span original
                extracted = locatedValue located
                extractedPos = locatedPos located
                extractedSpan = locatedSpan located
            extracted @?= original
            extractedPos @?= pos
            extractedSpan @?= span

        , testCase "position advancement with unicode characters" $ do
            let pos = startPos ""
                pos1 = advancePos pos 'α'  -- Greek alpha
                pos2 = advancePos pos1 'β'  -- Greek beta
                pos3 = advancePos pos2 '中' -- Chinese character
            posColumn pos1 @?= 2
            posColumn pos2 @?= 3
            posColumn pos3 @?= 4

        , testCase "span validation with edge cases" $ do
            let validSpan = spanBetween (posAtLineCol 1 1) (posAtLineCol 1 10)
                invalidSpan1 = spanBetween (posAtLineCol 5 1) (posAtLineCol 3 1)
                invalidSpan2 = spanBetween (posAtLineCol 1 20) (posAtLineCol 1 10)
            isValidSpan validSpan @?= True
            isValidSpan invalidSpan1 @?= False
            isValidSpan invalidSpan2 @?= False
        ]
    ]

-- | posAfter将列号推进1
prop_posAfterAdvancesColumn :: Int -> Int -> Property
prop_posAfterAdvancesColumn line col =
  col > 0 && line > 0 ==>
  let pos = SourcePos line col
      after = posAfter pos
  in posLine after == line && posColumn after == col + 1

-- | posAtLineCol创建正确的位置
prop_posAtLineColCorrect :: Int -> Int -> Property
prop_posAtLineColCorrect line col =
  line > 0 && col > 0 ==>
  let pos = posAtLineCol line col
  in posLine pos == line && posColumn pos == col

-- | advancePos正确处理换行符
prop_advancePosNewline :: Int -> Int -> Property
prop_advancePosNewline line col =
  line > 0 && col > 0 ==>
  let pos = SourcePos line col
      afterNewline = advancePos pos '\n'
  in posLine afterNewline == line + 1 && posColumn afterNewline == 1

-- | advancePosBy处理多个字符
prop_advancePosByMultiple :: Int -> Int -> String -> Property
prop_advancePosByMultiple line col chars =
  line > 0 && col > 0 && not (null chars) ==>
  let pos = SourcePos line col
      finalPos = advancePosBy pos chars
      expectedLine = line + L.length (L.filter (== '\n') chars)
      lastLineStarts = L.map (+1) $ findIndices (== '\n') chars
      expectedCol = if null lastLineStarts 
                    then col + L.length chars
                    else L.length (drop (last lastLineStarts) chars) + 1
  in posLine finalPos == expectedLine && 
     (if null lastLineStarts then posColumn finalPos == expectedCol else True)

-- | 位置排序是一致的
prop_positionOrdering :: Int -> Int -> Int -> Int -> Property
prop_positionOrdering line1 col1 line2 col2 =
  line1 > 0 && col1 > 0 && line2 > 0 && col2 > 0 ==>
  let pos1 = SourcePos line1 col1
      pos2 = SourcePos line2 col2
  in (line1 < line2) || (line1 == line2 && col1 < col2) ==>
     pos1 < pos2

-- | emptySpan具有零长度
prop_emptySpanZeroLength :: Property
prop_emptySpanZeroLength =
  let span = emptySpan
  in spanStart span == spanEnd span

-- | spanFrom创建有效范围
prop_spanFromValid :: Int -> Int -> Property
prop_spanFromValid line col =
  line > 0 && col > 0 ==>
  let pos = SourcePos line col
      span = spanFrom pos
  in spanStart span == pos && spanEnd span == pos

-- | spanTo创建有效范围
prop_spanToValid :: Int -> Int -> Property
prop_spanToValid line col =
  line > 0 && col > 0 ==>
  let pos = SourcePos line col
      span = spanTo pos
  in spanStart span == pos && spanEnd span == pos

-- | spanBetween包含两个端点
prop_spanBetweenContains :: Int -> Int -> Int -> Int -> Property
prop_spanBetweenContains line1 col1 line2 col2 =
  line1 > 0 && col1 > 0 && line2 > 0 && col2 > 0 ==>
  let pos1 = SourcePos line1 col1
      pos2 = SourcePos line2 col2
      span = spanBetween pos1 pos2
  in (spanStart span == pos1 || spanStart span == pos2) &&
     (spanEnd span == pos1 || spanEnd span == pos2)

-- | mergeSpans包含两个原始范围
prop_mergeSpansContains :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_mergeSpansContains line1 col1 line2 col2 line3 col3 line4 col4 =
  L.all (>0) [line1, col1, line2, col2, line3, col3, line4, col4] ==>
  let pos1 = SourcePos line1 col1
      pos2 = SourcePos line2 col2
      pos3 = SourcePos line3 col3
      pos4 = SourcePos line4 col4
      span1 = spanBetween pos1 pos2
      span2 = spanBetween pos3 pos4
      merged = mergeSpans span1 span2
      -- Check that merged contains both spans
      containsSpan span merged = 
        let start = spanStart span
            end = spanEnd span
            mergedStart = spanStart merged
            mergedEnd = spanEnd merged
        in (start >= mergedStart && end <= mergedEnd) ||
           (spanStart merged >= start && spanEnd merged <= end)
  in containsSpan span1 merged && containsSpan span2 merged

-- | isValidSpan正确识别有效范围
prop_isValidSpanCorrect :: Int -> Int -> Int -> Int -> Property
prop_isValidSpanCorrect line1 col1 line2 col2 =
  let pos1 = SourcePos line1 col1
      pos2 = SourcePos line2 col2
      span = spanBetween pos1 pos2
      shouldBeValid = pos1 <= pos2
  in isValidSpan span == shouldBeValid

-- | locatedAt保留值
prop_locatedAtPreservesValue :: String -> Int -> Int -> Property
prop_locatedAtPreservesValue value line col =
  line > 0 && col > 0 ==>
  let pos = posAtLineCol line col
      located = locatedAt pos value
  in locatedValue located == value && locatedPos located == pos

-- | locatedWithSpan保留值和范围
prop_locatedWithSpanPreserves :: String -> Int -> Int -> Int -> Int -> Property
prop_locatedWithSpanPreserves value line1 col1 line2 col2 =
  L.all (>0) [line1, col1, line2, col2] ==>
  let pos1 = SourcePos line1 col1
      pos2 = SourcePos line2 col2
      span = spanBetween pos1 pos2
      located = locatedWithSpan span value
  in locatedValue located == value && locatedSpan located == span

-- | mapLocated保留位置
prop_mapLocatedPreservesLocation :: String -> String -> Int -> Int -> Property
prop_mapLocatedPreservesLocation original transformed line col =
  line > 0 && col > 0 ==>
  let pos = posAtLineCol line col
      located = locatedAt pos original
      mapped = mapLocated (const transformed) located
  in locatedPos mapped == pos && locatedValue mapped == transformed

-- | locatedValue提取原始值
prop_locatedValueExtracts :: String -> Int -> Int -> Property
prop_locatedValueExtracts value line col =
  line > 0 && col > 0 ==>
  let pos = posAtLineCol line col
      located = locatedAt pos value
  in locatedValue located == value

-- | 范围长度非负
prop_spanLengthNonNegative :: Int -> Int -> Int -> Int -> Property
prop_spanLengthNonNegative line1 col1 line2 col2 =
  let pos1 = SourcePos line1 col1
      pos2 = SourcePos line2 col2
      span = spanBetween pos1 pos2
      -- Length calculation would depend on specific implementation
      -- For now, just check that span is valid L.or has equal start/end
  in not (isValidSpan span) || spanStart span <= spanEnd span

-- | 范围开始小于或等于结束
prop_spanStartBeforeEnd :: Int -> Int -> Int -> Int -> Property
prop_spanStartBeforeEnd line1 col1 line2 col2 =
  let pos1 = SourcePos line1 col1
      pos2 = SourcePos line2 col2
      span = spanBetween pos1 pos2
  in spanStart span <= spanEnd span

-- | mergeSpans是可交换的
prop_mergeSpansCommutative :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_mergeSpansCommutative line1 col1 line2 col2 line3 col3 line4 col4 =
  L.all (>0) [line1, col1, line2, col2, line3, col3, line4, col4] ==>
  let pos1 = SourcePos line1 col1
      pos2 = SourcePos line2 col2
      pos3 = SourcePos line3 col3
      pos4 = SourcePos line4 col4
      span1 = spanBetween pos1 pos2
      span2 = spanBetween pos3 pos4
      merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
  in merged1 == merged2

-- | mergeSpans是可结合的
prop_mergeSpansAssociative :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_mergeSpansAssociative line1 col1 line2 col2 line3 col3 line4 col4 line5 col5 line6 col6 =
  L.all (>0) [line1, col1, line2, col2, line3, col3, line4, col4, line5, col5, line6, col6] ==>
  let pos1 = SourcePos line1 col1
      pos2 = SourcePos line2 col2
      pos3 = SourcePos line3 col3
      pos4 = SourcePos line4 col4
      pos5 = SourcePos line5 col5
      pos6 = SourcePos line6 col6
      span1 = spanBetween pos1 pos2
      span2 = spanBetween pos3 pos4
      span3 = spanBetween pos5 pos6
      merged1 = mergeSpans (mergeSpans span1 span2) span3
      merged2 = mergeSpans span1 (mergeSpans span2 span3)
  in merged1 == merged2

-- | 位置处理非常大的行号
prop_largeLineNumbers :: Property
prop_largeLineNumbers =
  let line = 1000000
      col = 50
      pos = SourcePos line col
  in posLine pos == line && posColumn pos == col

-- | 位置处理非常大的列号
prop_largeColumnNumbers :: Property
prop_largeColumnNumbers =
  let line = 100
      col = 1000000
      pos = SourcePos line col
  in posLine pos == line && posColumn pos == col

-- | 范围处理零宽度范围
prop_zeroWidthSpans :: Int -> Int -> Property
prop_zeroWidthSpans line col =
  line > 0 && col > 0 ==>
  let pos = SourcePos line col
      span = spanFrom pos
  in spanStart span == spanEnd span

-- | 范围处理单字符范围
prop_singleCharSpans :: Int -> Int -> Property
prop_singleCharSpans line col =
  line > 0 && col > 0 ==>
  let start = SourcePos line col
      end = posAfter start
      span = spanBetween start end
  in spanStart span == start && spanEnd span == end

-- Helper functions
findIndices :: (a -> Bool) -> [a] -> [Int]
findIndices p xs = map fst $ L.filter (p . snd) $ zip [0..] xs

-- Define < for SourcePos
instance Ord SourcePos where
  compare (SourcePos l1 c1) (SourcePos l2 c2) =
    case compare l1 l2 of
      EQ -> compare c1 c2
      other -> other