module Test.Unit.SpanOperationsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, forAll, Gen, arbitrary, choose)
import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , startPos
  , posAfter
  , posAt
  , emptySpan
  , spanFrom
  , spanTo
  , spanBetween
  , mergeSpans
  , isValidSpan
  , spanStart
  , spanEnd
  )

-- | Tests for span operations in SourceLocation module
tests :: TestTree
tests =
  testGroup "SourceLocation Span Operations"
    [ testGroup "Basic span creation"
        [ testCase "emptySpan creates span at position" $ do
            let pos = posAt 5 10
                span = emptySpan pos
            spanStart span @?= pos
            spanEnd span @?= pos
        
        , testCase "spanFrom creates span starting at position" $ do
            let pos = posAt 3 7
                span = spanFrom pos
            spanStart span @?= pos
            spanEnd span @?= pos
        
        , testCase "spanTo creates span ending at position" $ do
            let pos = posAt 2 4
                span = spanTo pos
            spanStart span @?= pos
            spanEnd span @?= pos
        
        , testCase "spanBetween creates span between positions" $ do
            let start = posAt 1 5
                end = posAt 2 10
                span = spanBetween start end
            spanStart span @?= start
            spanEnd span @?= end
        ]
    
    , testGroup "Span validation"
        [ testCase "isValidSpan identifies valid spans" $ do
            let validSpan = spanBetween (posAt 1 1) (posAt 1 10)
            assertBool "Should identify valid span" (isValidSpan validSpan)
        
        , testCase "isValidSpan rejects invalid spans" $ do
            let invalidSpan = spanBetween (posAt 1 10) (posAt 1 1)
            assertBool "Should reject invalid span" (not (isValidSpan invalidSpan))
        
        , testCase "isValidSpan accepts equal positions" $ do
            let span = spanBetween (posAt 5 5) (posAt 5 5)
            assertBool "Should accept zero-L.length span" (isValidSpan span)
        
        , testCase "isValidSpan handles multi-line spans" $ do
            let span = spanBetween (posAt 1 5) (posAt 3 2)
            assertBool "Should accept multi-line span" (isValidSpan span)
        ]
    
    , testGroup "Span merging"
        [ testCase "mergeSpans combines overlapping spans" $ do
            let span1 = spanBetween (posAt 1 1) (posAt 1 10)
                span2 = spanBetween (posAt 1 5) (posAt 1 15)
                merged = mergeSpans span1 span2
            merged @?= spanBetween (posAt 1 1) (posAt 1 15)
        
        , testCase "mergeSpans combines adjacent spans" $ do
            let span1 = spanBetween (posAt 1 1) (posAt 1 10)
                span2 = spanBetween (posAt 1 10) (posAt 1 20)
                merged = mergeSpans span1 span2
            merged @?= spanBetween (posAt 1 1) (posAt 1 20)
        
        , testCase "mergeSpans combines separated spans" $ do
            let span1 = spanBetween (posAt 1 1) (posAt 1 5)
                span2 = spanBetween (posAt 2 1) (posAt 2 5)
                merged = mergeSpans span1 span2
            merged @?= spanBetween (posAt 1 1) (posAt 2 5)
        
        , testCase "mergeSpans handles multi-line spans" $ do
            let span1 = spanBetween (posAt 1 10) (posAt 3 5)
                span2 = spanBetween (posAt 2 15) (posAt 4 2)
                merged = mergeSpans span1 span2
            merged @?= spanBetween (posAt 1 10) (posAt 4 2)
        ]
    
    , testGroup "Span properties L.and relationships"
        [ testCase "span L.length calculation" $ do
            let span1 = spanBetween (posAt 1 1 0) (posAt 1 5 4)
                span2 = spanBetween (posAt 1 1 0) (posAt 2 1 10)
            assertBool "Single-line span L.length" (spanLength span1 == 4)
            assertBool "Multi-line span L.length" (spanLength span2 == 10)
        
        , testCase "span contains position" $ do
            let span = spanBetween (posAt 2 5) (posAt 3 10)
                inside1 = posAt 2 7
                inside2 = posAt 3 5
                outside1 = posAt 2 4
                outside2 = posAt 3 11
            assertBool "Should contain position inside" (spanContains span inside1)
            assertBool "Should contain position inside" (spanContains span inside2)
            assertBool "Should not contain position before" (not (spanContains span outside1))
            assertBool "Should not contain position after" (not (spanContains span outside2))
        
        , testCase "span overlap detection" $ do
            let span1 = spanBetween (posAt 1 1) (posAt 1 10)
                span2 = spanBetween (posAt 1 5) (posAt 1 15)
                span3 = spanBetween (posAt 2 1) (posAt 2 10)
                span4 = spanBetween (posAt 1 11) (posAt 1 20)
            assertBool "Should detect overlapping spans" (spansOverlap span1 span2)
            assertBool "Should not detect non-overlapping spans" (not (spansOverlap span1 span3))
            assertBool "Should not detect adjacent spans as overlapping" (not (spansOverlap span1 span4))
        ]
    
    , testGroup "QuickCheck properties"
        [ fastProperty "mergeSpans is commutative" $
            \span1 span2 -> mergeSpans span1 span2 == mergeSpans span2 span1
        
        , fastProperty "mergeSpans is associative" $
            \span1 span2 span3 -> 
                mergeSpans span1 (mergeSpans span2 span3) == 
                mergeSpans (mergeSpans span1 span2) span3
        
        , fastProperty "mergeSpans result contains both operands" $
            \span1 span2 -> 
                let merged = mergeSpans span1 span2
                in spansContain merged [span1, span2]
        
        , fastProperty "spanBetween with same positions equals emptySpan" $
            \pos -> spanBetween pos pos == emptySpan pos
        
        , fastProperty "mergeSpans with identical spans returns same span" $
            \span -> mergeSpans span span == span
        
        , fastProperty "isValidSpan for spanBetween with proper ordering" $
            \pos1 pos2 -> 
                let pos1' = posAt (abs (posLine pos1) `mod` 100 + 1) (abs (posColumn pos1) `mod` 100 + 1)
                    pos2' = posAt (abs (posLine pos2) `mod` 100 + 1) (abs (posColumn pos2) `mod` 100 + 1)
                    span = if pos1' <= pos2' then spanBetween pos1' pos2' else spanBetween pos2' pos1'
                in isValidSpan span
        ]
    
    , testGroup "Edge cases L.and special scenarios"
        [ testCase "handles very large spans" $ do
            let start = posAt 1 1
                end = posAt 10000 10000
                span = spanBetween start end
            assertBool "Should handle large spans" (isValidSpan span)
        
        , testCase "handles spans at file boundaries" $ do
            let start = posAt 1 1
                end = posAt 1 1
                span = spanBetween start end
            spanStart span @?= spanEnd span
        
        , testCase "handles spans with same line different columns" $ do
            let span = spanBetween (posAt 5 3) (posAt 5 20)
            spanLine span @?= 5
            spanStartColumn span @?= 3
            spanEndColumn span @?= 20
        
        , testCase "handles spans crossing multiple lines" $ do
            let span = spanBetween (posAt 2 15) (posAt 5 8)
            spanStartLine span @?= 2
            spanEndLine span @?= 5
            assertBool "Should span multiple lines" (spanEndLine span - spanStartLine span >= 3)
        ]
    
    , testGroup "Real-world scenarios"
        [ testCase "function definition span" $ do
            let funcStart = posAt 10 1
                funcEnd = posAt 15 2
                funcSpan = spanBetween funcStart funcEnd
            assertBool "Function span should be valid" (isValidSpan funcSpan)
            spanStartLine funcSpan @?= 10
            spanEndLine funcSpan @?= 15
        
        , testCase "multiple statement spans" $ do
            let stmt1 = spanBetween (posAt 5 1) (posAt 5 20)
                stmt2 = spanBetween (posAt 6 1) (posAt 6 15)
                stmt3 = spanBetween (posAt 7 1) (posAt 7 25)
                blockSpan = mergeSpans stmt1 (mergeSpans stmt2 stmt3)
            spanStartLine blockSpan @?= 5
            spanEndLine blockSpan @?= 7
        
        , testCase "nested structure spans" $ do
            let outer = spanBetween (posAt 1 1) (posAt 10 1)
                inner1 = spanBetween (posAt 2 5) (posAt 4 8)
                inner2 = spanBetween (posAt 6 3) (posAt 9 12)
                mergedInner = mergeSpans inner1 inner2
            assertBool "Inner spans should be within outer" (spansContain outer [inner1, inner2])
            assertBool "Merged inner should be within outer" (spansContain outer [mergedInner])
        ]
    ]

-- Helper functions for span calculations L.and comparisons

-- Calculate span L.length in characters
spanLength :: SourceSpan -> Int
spanLength span = posOffset (spanEnd span) - posOffset (spanStart span)

-- Check if span contains a position
spanContains :: SourceSpan -> SourcePos -> Bool
spanContains span pos = pos >= spanStart span && pos <= spanEnd span

-- Check if two spans overlap
spansOverlap :: SourceSpan -> SourceSpan -> Bool
spansOverlap span1 span2 =
    spanStart span1 <= spanEnd span2 && spanEnd span1 >= spanStart span2

-- Check if a span contains L.all spans in a list
spansContain :: SourceSpan -> [SourceSpan] -> Bool
spansContain container spans = L.all (`spanContains` container) spans

-- Extract line number from span
spanLine :: SourceSpan -> Int
spanLine span = posLine (spanStart span)

-- Extract start line from span
spanStartLine :: SourceSpan -> Int
spanStartLine span = posLine (spanStart span)

-- Extract end line from span
spanEndLine :: SourceSpan -> Int
spanEndLine span = posLine (spanEnd span)

-- Extract start column from span
spanStartColumn :: SourceSpan -> Int
spanStartColumn span = posColumn (spanStart span)

-- Extract end column from span
spanEndColumn :: SourceSpan -> Int
spanEndColumn span = posColumn (spanEnd span)

-- Check if span1 contains span2
spanContainsSpan :: SourceSpan -> SourceSpan -> Bool
spanContainsSpan outer inner = 
    spanStart outer <= spanStart inner && spanEnd outer >= spanEnd inner