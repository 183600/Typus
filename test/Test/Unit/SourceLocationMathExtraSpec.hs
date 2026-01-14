{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.SourceLocationMathExtraSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import SourceLocation
import Data.List (minimum, maximum)

-- 辅助函数
sourcePosLine :: SourcePos -> Int
sourcePosLine (SourcePos line _) = line

sourcePosColumn :: SourcePos -> Int
sourcePosColumn (SourcePos _ col) = col

-- | 测试SourceLocation模块中的位置计算功能
tests :: TestTree
tests = testGroup "SourceLocationMathExtraSpec Tests"
  [ testGroup "SourcePos函数测试"
    [ testCase "startPos returns (1,1)" $ startPos @?= SourcePos 1 1
    , testCase "posAfter calculates next position in same line" $
        (posAfter (SourcePos 1 1) @?= SourcePos 1 2) *>
        (posAfter (SourcePos 5 10) @?= SourcePos 5 11)
    , testCase "posAt creates position at specified coordinates" $
        (posAt 3 5 @?= SourcePos 3 5) *>
        (posAt 1 1 @?= startPos)
    , testCase "posAtLineCol creates position at specified line and column" $
        posAtLineCol 2 3 @?= SourcePos 2 3
    , testProperty "posAfter preserves line number" $
        \pos -> sourcePosLine (posAfter pos) == sourcePosLine pos
    , testProperty "posAfter increments column by 1" $
        \pos -> sourcePosColumn (posAfter pos) == sourcePosColumn pos + 1
    ]
  
  , testGroup "SourceSpan函数测试"
    [ testCase "emptySpan creates empty span" $ do
        let span = emptySpan
        spanStart span @?= startPos
        spanEnd span @?= startPos
    , testCase "spanFrom creates span from specified position" $ do
        let pos = SourcePos 2 3
            span = spanFrom pos
        spanStart span @?= pos
        spanEnd span @?= pos
    , testCase "spanTo creates span to specified position" $ do
        let pos = SourcePos 2 3
            span = spanTo pos
        spanStart span @?= pos
        spanEnd span @?= pos
    , testCase "spanBetween creates span between two positions" $ do
        let start = SourcePos 2 3
            end = SourcePos 4 6
            span = spanBetween start end
        spanStart span @?= start
        spanEnd span @?= end
    , testCase "spanBetweenOrdered handles ordering correctly" $ do
        let pos1 = SourcePos 2 3
            pos2 = SourcePos 4 6
            span1 = spanBetweenOrdered pos1 pos2
            span2 = spanBetweenOrdered pos2 pos1
        spanStart span1 @?= pos1
        spanEnd span1 @?= pos2
        spanStart span2 @?= pos1
        spanEnd span2 @?= pos2
    , testProperty "spanBetweenOrdered start <= end" $
        \pos1 pos2 ->
          let span = spanBetweenOrdered pos1 pos2
              start = spanStart span
              end = spanEnd span
          in (sourcePosLine start < sourcePosLine end) ||
             (sourcePosLine start == sourcePosLine end && sourcePosColumn start <= sourcePosColumn end)
    ]
  
  , testGroup "mergeSpans函数测试"
    [ testCase "mergeSpans adjacent spans" $ do
        let span1 = spanBetween (SourcePos 1 1) (SourcePos 1 5)
            span2 = spanBetween (SourcePos 1 6) (SourcePos 1 10)
            merged = mergeSpans span1 span2
        spanStart merged @?= SourcePos 1 1
        spanEnd merged @?= SourcePos 1 10
    , testCase "mergeSpans overlapping spans" $ do
        let span1 = spanBetween (SourcePos 1 1) (SourcePos 1 8)
            span2 = spanBetween (SourcePos 1 5) (SourcePos 1 12)
            merged = mergeSpans span1 span2
        spanStart merged @?= SourcePos 1 1
        spanEnd merged @?= SourcePos 1 12
    , testCase "mergeSpans multiline spans" $ do
        let span1 = spanBetween (SourcePos 1 5) (SourcePos 3 10)
            span2 = spanBetween (SourcePos 2 3) (SourcePos 4 8)
            merged = mergeSpans span1 span2
        spanStart merged @?= SourcePos 1 5
        spanEnd merged @?= SourcePos 4 8
    , testProperty "mergeSpans contains original spans" $
        \span1 span2 ->
          let merged = mergeSpans span1 span2
              start1 = spanStart span1
              end1 = spanEnd span1
              start2 = spanStart span2
              end2 = spanEnd span2
              mergedStart = spanStart merged
              mergedEnd = spanEnd merged
          in (sourcePosLine mergedStart <= sourcePosLine start1 ||
              (sourcePosLine mergedStart == sourcePosLine start1 && sourcePosColumn mergedStart <= sourcePosColumn start1)) &&
             (sourcePosLine mergedEnd >= sourcePosLine end1 ||
              (sourcePosLine mergedEnd == sourcePosLine end1 && sourcePosColumn mergedEnd >= sourcePosColumn end1)) &&
             (sourcePosLine mergedStart <= sourcePosLine start2 ||
              (sourcePosLine mergedStart == sourcePosLine start2 && sourcePosColumn mergedStart <= sourcePosColumn start2)) &&
             (sourcePosLine mergedEnd >= sourcePosLine end2 ||
              (sourcePosLine mergedEnd == sourcePosLine end2 && sourcePosColumn mergedEnd >= sourcePosColumn end2))
    ]
  
  , testGroup "isValidSpan函数测试"
    [ testCase "isValidSpan valid span" $ do
        let span = spanBetween (SourcePos 1 1) (SourcePos 1 5)
        isValidSpan span @?= True
    , testCase "isValidSpan empty span" $ isValidSpan emptySpan @?= True
    , testCase "isValidSpan invalid span" $ do
        let span = spanBetween (SourcePos 2 5) (SourcePos 1 10)
        isValidSpan span @?= False
    , testCase "isValidSpan same line invalid span" $ do
        let span = spanBetween (SourcePos 1 10) (SourcePos 1 5)
        isValidSpan span @?= False
    , testProperty "spanBetweenOrdered creates valid span" $
        \pos1 pos2 -> isValidSpan (spanBetweenOrdered pos1 pos2)
    , testCase "emptySpan is valid" $ isValidSpan emptySpan @?= True
    ]
  
  , testGroup "isValidBlockSpan函数测试"
    [ testCase "isValidBlockSpan valid block span" $ do
        let span = spanBetween (SourcePos 1 1) (SourcePos 3 1)
        isValidBlockSpan span @?= True
    , testCase "isValidBlockSpan single line span" $ do
        let span = spanBetween (SourcePos 1 1) (SourcePos 1 10)
        isValidBlockSpan span @?= False
    , testCase "isValidBlockSpan empty span" $ isValidBlockSpan emptySpan @?= False
    , testCase "isValidBlockSpan invalid block span" $ do
        let span = spanBetween (SourcePos 3 1) (SourcePos 1 1)
        isValidBlockSpan span @?= False
    , testProperty "valid block span is also valid span" $
        \span -> isValidBlockSpan span ==> isValidSpan span
    ]
  
  , testGroup "Located值函数测试"
    [ testCase "locatedAt creates located value" $ do
        let pos = SourcePos 2 3
            value = "test"
            located = locatedAt pos value
        locatedPos located @?= pos
        locatedValue located @?= value
    , testCase "locatedWithSpan creates located value with span" $ do
        let span = spanBetween (SourcePos 1 1) (SourcePos 1 5)
            value = "test"
            located = locatedWithSpan span value
        locatedSpan located @?= span
        locatedValue located @?= value
    , testCase "mapLocated maps value correctly" $ do
        let pos = SourcePos 2 3
            value = "test"
            located = locatedAt pos value
            mapped = mapLocated (++ " mapped") located
        locatedPos mapped @?= pos
        locatedValue mapped @?= "test mapped"
    , testProperty "mapLocated preserves location" $
        \pos value f ->
          let located = locatedAt pos value
              mapped = mapLocated f located
          in locatedPos mapped === locatedPos located
    ]
  
  , testGroup "位置计算属性测试"
    [ testProperty "SourcePos reflexivity" $
        \pos -> pos == pos
    , testProperty "SourcePos symmetry" $
        \pos1 pos2 -> (pos1 == pos2) == (pos2 == pos1)
    , testProperty "SourcePos transitivity" $
        \pos1 pos2 pos3 ->
          if pos1 == pos2 && pos2 == pos3
          then pos1 == pos3
          else property True
    , testProperty "posAfter is monotonic" $
        \pos1 pos2 ->
          if sourcePosLine pos1 < sourcePosLine pos2 ||
             (sourcePosLine pos1 == sourcePosLine pos2 && sourcePosColumn pos1 < sourcePosColumn pos2)
          then posAfter pos1 /= posAfter pos2
          else property True
    , testProperty "spanBetweenOrdered is commutative" $
        \pos1 pos2 ->
          let span1 = spanBetweenOrdered pos1 pos2
              span2 = spanBetweenOrdered pos2 pos1
          in span1 == span2
    , testProperty "mergeSpans is commutative" $
        \span1 span2 ->
          let merged1 = mergeSpans span1 span2
              merged2 = mergeSpans span2 span1
          in merged1 == merged2
    , testProperty "mergeSpans is idempotent" $
        \span ->
          let merged = mergeSpans span span
          in merged == span
    ]
  ]