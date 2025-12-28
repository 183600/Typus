module Test.Unit.NewSourceLocationMathPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), choose, Gen)
import SourceLocation
import qualified Data.Text as T

-- | Test mathematical properties of source location operations
tests :: TestTree
tests =
  testGroup "Source Location Mathematical Properties"
    [ testGroup "Position arithmetic"
        [ testCase "posAfter advances line correctly" $ do
            let start = SourcePos 1 5
                after = posAfter '\n' start
            after @?= SourcePos 2 1

        , testCase "posAfter advances column correctly" $ do
            let start = SourcePos 1 5
                after = posAfter 'a' start
            after @?= SourcePos 1 6

        , testCase "posAt creates correct position" $ do
            let pos = posAt 3 7
            pos @?= SourcePos 3 7
        ]

    , testGroup "Span operations"
        [ testCase "emptySpan has zero length" $ do
            let pos = SourcePos 1 1
                span = emptySpan pos
            spanStart span @?= pos
            spanEnd span @?= pos

        , testCase "spanBetween creates correct span" $ do
            let start = SourcePos 1 1
                end = SourcePos 1 5
                span = spanBetween start end
            spanStart span @?= start
            spanEnd span @?= end

        , testCase "mergeSpans contains both original spans" $ do
            let span1 = spanBetween (SourcePos 1 1) (SourcePos 1 3)
                span2 = spanBetween (SourcePos 1 5) (SourcePos 1 7)
                merged = mergeSpans span1 span2
            spanStart merged @?= SourcePos 1 1
            spanEnd merged @?= SourcePos 1 7
        ]

    , testGroup "Located value operations"
        [ testCase "locatedValue extracts correct value" $ do
            let located = locatedAt (SourcePos 1 1) "test"
            locatedValue located @?= "test"

        , testCase "locatedSpan preserves position" $ do
            let pos = SourcePos 2 3
                located = locatedAt pos "value"
            locatedSpan located @?= emptySpan pos

        , testCase "mapLocated preserves location" $ do
            let pos = SourcePos 1 1
                located = locatedAt pos 42
                mapped = mapLocated (*2) located
            locatedSpan mapped @?= locatedSpan located
            locatedValue mapped @?= 84
        ]

    , testGroup "Property-based tests"
        [ testProperty "posAfter newline increments line" prop_posAfterNewline
        , testProperty "posAfter regular char increments column" prop_posAfterRegularChar
        , testProperty "mergeSpans is associative" prop_mergeSpansAssociative
        , testProperty "spanBetween is ordered" prop_spanBetweenOrdered
        , testProperty "locatedAt creates valid span" prop_locatedAtValidSpan
        ]
    ]

-- Property: posAfter newline always increments line and resets column to 1
prop_posAfterNewline :: Int -> Int -> Bool
prop_posAfterNewline line col =
    let start = SourcePos (abs line + 1) (abs col + 1)
        after = posAfter '\n' start
    in sourceLine after == sourceLine start + 1 && sourceColumn after == 1

-- Property: posAfter regular character increments column but not line
prop_posAfterRegularChar :: Int -> Int -> Char -> Bool
prop_posAfterRegularChar line col ch
    | ch == '\n' = True  -- Skip newlines for this property
    | otherwise =
        let start = SourcePos (abs line + 1) (abs col + 1)
            after = posAfter ch start
        in sourceLine after == sourceLine start && 
           sourceColumn after == sourceColumn start + 1

-- Property: mergeSpans is associative
prop_mergeSpansAssociative :: SourcePos -> SourcePos -> SourcePos -> SourcePos -> Bool
prop_mergeSpansAssociative p1 p2 p3 p4 =
    let span1 = spanBetween p1 p2
        span2 = spanBetween p3 p4
        span3 = spanBetween p1 p4  -- Assume p1 <= p4 for simplicity
    in mergeSpans span1 (mergeSpans span2 span3) == 
       mergeSpans (mergeSpans span1 span2) span3

-- Property: spanBetween always has start <= end
prop_spanBetweenOrdered :: SourcePos -> SourcePos -> Bool
prop_spanBetweenOrdered start end =
    let span = spanBetween start end
        startPos = spanStart span
        endPos = spanEnd span
    in (sourceLine startPos < sourceLine endPos) ||
       (sourceLine startPos == sourceLine endPos && 
        sourceColumn startPos <= sourceColumn endPos)

-- Property: locatedAt creates a span with equal start and end
prop_locatedAtValidSpan :: SourcePos -> String -> Bool
prop_locatedAtValidSpan pos value =
    let located = locatedAt pos value
        span = locatedSpan located
    in spanStart span == spanEnd span && spanStart span == pos