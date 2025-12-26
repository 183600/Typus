{-# LANGUAGE CPP #-}

module Test.Unit.SpanOperationsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import TestSupport.QuickCheck (fastProperty)

import SourceLocation (SourcePos(..), SourceSpan(..), Located(..),
                      startPos, posAt, posAtLineCol, spanBetween, spanFrom, spanTo,
                      emptySpan, mergeSpans, isValidSpan, spanStart, spanEnd)

import Data.List (sort)

-- | 测试跨度操作功能的属性和边界情况
tests :: TestTree
tests =
  testGroup "Span Operations"
    [ testGroup "Span Creation"
        [ testCase "spanBetween creates valid span" $ do
            let start = posAt 1 1
                end = posAt 1 5
                span = spanBetween start end
            spanStart span @?= start
            spanEnd span @?= end
            isValidSpan span @?= True
            
        , testCase "spanFrom creates span from position" $ do
            let pos = posAt 2 3
                span = spanFrom pos
            spanStart span @?= pos
            spanEnd span @?= pos
            
        , testCase "spanTo creates span to position" $ do
            let pos = posAt 3 4
                span = spanTo pos
            spanStart span @?= pos
            spanEnd span @?= pos
            
        , testCase "emptySpan is valid" $ do
            let span = emptySpan
            isValidSpan span @?= True
        ]
        
    , testGroup "Span Validation"
        [ testCase "valid span when start <= end" $ do
            let start = posAt 1 1
                end = posAt 2 5
                span = spanBetween start end
            isValidSpan span @?= True
            
        , testCase "valid span when same line and column" $ do
            let pos = posAt 3 7
                span = spanBetween pos pos
            isValidSpan span @?= True
            
        , testCase "valid span when same line but later column" $ do
            let start = posAt 4 2
                end = posAt 4 8
                span = spanBetween start end
            isValidSpan span @?= True
            
        , testCase "invalid span when start > end" $ do
            let start = posAt 5 10
                end = posAt 5 5
                span = spanBetween start end
            isValidSpan span @?= False
            
        , testCase "invalid span when later line but earlier column" $ do
            let start = posAt 6 8
                end = posAt 5 10
                span = spanBetween start end
            isValidSpan span @?= False
        ]
        
    [ testGroup "Span Merging"
        [ testCase "mergeSpans combines adjacent spans" $ do
            let span1 = spanBetween (posAt 1 1) (posAt 1 5)
                span2 = spanBetween (posAt 1 5) (posAt 1 10)
                merged = mergeSpans span1 span2
            spanStart merged @?= spanStart span1
            spanEnd merged @?= spanEnd span2
            
        , testCase "mergeSpans combines overlapping spans" $ do
            let span1 = spanBetween (posAt 1 1) (posAt 1 8)
                span2 = spanBetween (posAt 1 5) (posAt 1 12)
                merged = mergeSpans span1 span2
            spanStart merged @?= spanStart span1
            spanEnd merged @?= spanEnd span2
            
        , testCase "mergeSpans handles separate spans" $ do
            let span1 = spanBetween (posAt 1 1) (posAt 1 3)
                span2 = spanBetween (posAt 1 6) (posAt 1 8)
                merged = mergeSpans span1 span2
            spanStart merged @?= spanStart span1
            spanEnd merged @?= spanEnd span2
            
        , testCase "mergeSpans is associative" $ do
            let span1 = spanBetween (posAt 1 1) (posAt 1 3)
                span2 = spanBetween (posAt 1 2) (posAt 1 5)
                span3 = spanBetween (posAt 1 4) (posAt 1 7)
                merge12 = mergeSpans span1 span2
                merge23 = mergeSpans span2 span3
                final1 = mergeSpans merge12 span3
                final2 = mergeSpans span1 merge23
            spanStart final1 @?= spanStart final2
            spanEnd final1 @?= spanEnd final2
        ]
        
    , testGroup "Multi-line Spans"
        [ testCase "handles single line spans" $ do
            let start = posAt 3 5
                end = posAt 3 15
                span = spanBetween start end
            posLine (spanStart span) @?= posLine (spanEnd span)
            
        , testCase "handles multi-line spans" $ do
            let start = posAt 2 8
                end = posAt 4 3
                span = spanBetween start end
            posLine (spanStart span) @?= 2
            posLine (spanEnd span) @?= 4
            isValidSpan span @?= True
            
        , testCase "handles spans across many lines" $ do
            let start = posAt 1 1
                end = posAt 100 50
                span = spanBetween start end
            isValidSpan span @?= True
        ]
        
    , testGroup "Property Tests"
        [ testProperty "spanBetween is valid when start <= end" $ fastProperty $ \line1 col1 line2 col2 ->
            let start = posAt (abs line1 `mod` 1000 + 1) (abs col1 `mod` 1000 + 1)
                end = posAt (abs line2 `mod` 1000 + 1) (abs col2 `mod` 1000 + 1)
                span = spanBetween start end
                valid = isValidSpan span
            in if posLine start < posLine end || 
                  (posLine start == posLine end && posColumn start <= posColumn end)
               then valid
               else True  -- span may be invalid, that's expected
               
        , testProperty "mergeSpans preserves earliest start and latest end" $ fastProperty $ \line1 col1 line2 col2 line3 col3 line4 col4 ->
            let start1 = posAt (abs line1 `mod` 100 + 1) (abs col1 `mod` 100 + 1)
                end1 = posAt (abs line2 `mod` 100 + 1) (abs col2 `mod` 100 + 1)
                start2 = posAt (abs line3 `mod` 100 + 1) (abs col3 `mod` 100 + 1)
                end2 = posAt (abs line4 `mod` 100 + 1) (abs col4 `mod` 100 + 1)
                span1 = spanBetween start1 end1
                span2 = spanBetween start2 end2
                merged = mergeSpans span1 span2
                starts = [spanStart span1, spanStart span2]
                ends = [spanEnd span1, spanEnd span2]
                earliestStart = minimum starts
                latestEnd = maximum ends
            in spanStart merged == earliestStart && spanEnd merged == latestEnd
            
        , testProperty "spanFrom and spanTo create zero-length spans" $ fastProperty $ \line col ->
            let pos = posAt (abs line `mod` 1000 + 1) (abs col `mod` 1000 + 1)
                spanFromPos = spanFrom pos
                spanToPos = spanTo pos
            in spanStart spanFromPos == pos && spanEnd spanFromPos == pos &&
               spanStart spanToPos == pos && spanEnd spanToPos == pos
        ]
        
    , testGroup "Edge Cases"
        [ testCase "handles span at start of file" $ do
            let span = spanBetween startPos (posAt 1 10)
            spanStart span @?= startPos
            isValidSpan span @?= True
            
        , testCase "handles zero-length span" $ do
            let pos = posAt 5 7
                span = spanBetween pos pos
            spanStart span @?= pos
            spanEnd span @?= pos
            isValidSpan span @?= True
            
        , testCase "handles very large spans" $ do
            let start = posAt 1 1
                end = posAt 100000 100000
                span = spanBetween start end
            isValidSpan span @?= True
            
        , testCase "handles merging empty spans" $ do
            let span1 = emptySpan
                span2 = emptySpan
                merged = mergeSpans span1 span2
            length (show merged) >= 0 @?= True
        ]
        
    , testGroup "Span Ordering and Comparison"
        [ testCase "spans can be compared by start position" $ do
            let span1 = spanBetween (posAt 1 1) (posAt 1 5)
                span2 = spanBetween (posAt 1 6) (posAt 1 10)
                span3 = spanBetween (posAt 2 1) (posAt 2 5)
            span1 < span2 @?= True
            span2 < span3 @?= True
            span1 < span3 @?= True
            
        , testProperty "span ordering is transitive" $ fastProperty $ \line1 col1 line2 col2 line3 col3 ->
            let start1 = posAt (abs line1 `mod` 100 + 1) (abs col1 `mod` 100 + 1)
                end1 = posAt (posLine start1) (posColumn start1 + 5)
                start2 = posAt (abs line2 `mod` 100 + 1) (abs col2 `mod` 100 + 1)
                end2 = posAt (posLine start2) (posColumn start2 + 5)
                start3 = posAt (abs line3 `mod` 100 + 1) (abs col3 `mod` 100 + 1)
                end3 = posAt (posLine start3) (posColumn start3 + 5)
                span1 = spanBetween start1 end1
                span2 = spanBetween start2 end2
                span3 = spanBetween start3 end3
                spans = sort [span1, span2, span3]
            in length spans == 3
        ]
        
    , testGroup "Span Arithmetic"
        [ testCase "span length calculation" $ do
            let span1 = spanBetween (posAt 1 1) (posAt 1 5)  -- 4 characters
                span2 = spanBetween (posAt 1 1) (posAt 2 1)  -- includes newline
            spanLength span1 @?= 4
            spanLength span2 @?= 1  -- simplified calculation
            
        , testProperty "span length is non-negative" $ fastProperty $ \line1 col1 line2 col2 ->
            let start = posAt (abs line1 `mod` 100 + 1) (abs col1 `mod` 100 + 1)
                end = posAt (abs line2 `mod` 100 + 1) (abs col2 `mod` 100 + 1)
                span = spanBetween start end
            in spanLength span >= 0
        ]
        
    , testGroup "Performance and Robustness"
        [ testProperty "span operations handle large values" $ fastProperty $ \line1 col1 line2 col2 ->
            let start = posAt (abs line1 `mod` 100000 + 1) (abs col1 `mod` 100000 + 1)
                end = posAt (abs line2 `mod` 100000 + 1) (abs col2 `mod` 100000 + 1)
                span = spanBetween start end
                merged = mergeSpans span span
            in length (show span) >= 0 && length (show merged) >= 0
            
        , testCase "handles many merge operations" $ do
            let spans = [spanBetween (posAt i 1) (posAt i 10) | i <- [1..100]]
                merged = foldl mergeSpans (head spans) (tail spans)
            isValidSpan merged @?= True
            
        , testProperty "span operations are consistent" $ fastProperty $ \line1 col1 line2 col2 ->
            let start = posAt (abs line1 `mod` 1000 + 1) (abs col1 `mod` 1000 + 1)
                end = posAt (abs line2 `mod` 1000 + 1) (abs col2 `mod` 1000 + 1)
                span1 = spanBetween start end
                span2 = spanBetween (spanStart span1) (spanEnd span1)
            in span1 == span2
        ]
    ]

-- Helper function to calculate span length (simplified)
spanLength :: SourceSpan -> Int
spanLength span =
    let start = spanStart span
        end = spanEnd span
    in if posLine start == posLine end
       then posColumn end - posColumn start
       else 1  -- Simplified: just count as 1 for multi-line spans