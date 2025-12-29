module Test.Unit.SourceLocationBoundaryQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, choose, listOf, suchThat, oneof, elements, frequency)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), startPos, posAfter, spanBetween, mergeSpans, isValidSpan, locatedAt, locatedWithSpan, mapLocated, _isPosInSpan, _doSpansOverlap, _spanLength)

-- | Generate arbitrary source positions with boundary-focused values
instance Arbitrary SourcePos where
  arbitrary = frequency 
    [ (5, do -- Normal positions
        line <- choose (1, 100)
        column <- choose (1, 100)
        offset <- choose (0, 10000)
        return $ SourcePos line column offset)
    , (1, do -- Edge case: line 1
        column <- choose (1, 100)
        offset <- choose (0, 100)
        return $ SourcePos 1 column offset)
    , (1, do -- Edge case: column 1
        line <- choose (1, 100)
        offset <- choose (0, 100)
        return $ SourcePos line 1 offset)
    , (1, do -- Edge case: offset 0
        line <- choose (1, 100)
        column <- choose (1, 100)
        return $ SourcePos line column 0)
    ]

-- | Generate arbitrary source spans with boundary conditions
instance Arbitrary SourceSpan where
  arbitrary = frequency
    [ (5, do -- Normal spans
        startLine <- choose (1, 50)
        startCol <- choose (1, 50)
        endLineOffset <- choose (0, 10)
        endColOffset <- choose (0, 50)
        let endLine = startLine + endLineOffset
            endCol = if endLine == startLine then startCol + endColOffset else choose (1, 100)
        let start = SourcePos startLine startCol (startLine * 100 + startCol)
            end = SourcePos endLine endCol (endLine * 100 + endCol)
        return $ SourceSpan start end)
    , (2, do -- Zero-length spans
        pos <- arbitrary
        return $ SourceSpan pos pos)
    , (1, do -- Single character spans
        pos <- arbitrary
        let endPos = posAfter 'x' pos
        return $ SourceSpan pos endPos)
    ]

-- | Generate arbitrary located values
instance Arbitrary a => Arbitrary (Located a) where
  arbitrary = do
    value <- arbitrary
    pos <- arbitrary
    span <- arbitrary
    return $ Located value pos span

-- | Generate strings for position advancement with boundary cases
genBoundaryText :: Gen String
genBoundaryText = frequency
    [ (5, listOf $ elements ['a'..'z', 'A'..'Z', '0'..'9', ' '])
    , (2, return $ replicate 10 '\n') -- Many newlines
    , (2, return $ replicate 10 '\t') -- Many tabs
    , (1, return "") -- Empty string
    ]

tests :: TestTree
tests =
  testGroup "SourceLocation boundary conditions QuickCheck tests"
    [ testGroup "Position boundary conditions"
        [ testCase "posAfter handles all boundary characters" $ do
            let pos = startPos { posColumn = 5, posOffset = 10 }
                posNewline = posAfter '\n' pos
                posTab = posAfter '\t' pos
                posRegular = posAfter 'x' pos
            posNewline @?= SourcePos { posLine = 2, posColumn = 1, posOffset = 11 }
            posTab @?= SourcePos { posLine = 1, posColumn = 9, posOffset = 11 } -- Next tab stop
            posRegular @?= SourcePos { posLine = 1, posColumn = 6, posOffset = 11 }

        , fastProperty "posAfter maintains monotonic offset increase" $
            \pos ch ->
              let newPos = posAfter ch pos
              in posOffset newPos > posOffset pos

        , fastProperty "posAfter newline always sets column to 1" $
            \pos ->
              let newPos = posAfter '\n' pos
              in posColumn newPos == 1

        , fastProperty "posAfter tab advances to multiple of 8 plus 1" $
            \pos ->
              let newPos = posAfter '\t' pos
                  col = posColumn newPos
              in (col - 1) `mod` 8 == 0
        ]

    , testGroup "Span boundary conditions"
        [ testCase "zero-length span is valid" $ do
            let pos = SourcePos 5 10 100
                span = SourceSpan pos pos
            isValidSpan span @?= True

        , testCase "span covering single position has zero length" $ do
            let pos = SourcePos 3 7 50
                span = SourceSpan pos pos
            _spanLength span @?= 0

        , fastProperty "span length is non-negative" $
            \span ->
              _spanLength span >= 0

        , fastProperty "merged span contains both original spans" $
            \span1 span2 ->
              let merged = mergeSpans span1 span2
              in spanStart merged <= spanStart span1 &&
                 spanEnd merged >= spanEnd span1 &&
                 spanStart merged <= spanStart span2 &&
                 spanEnd merged >= spanEnd span2

        , fastProperty "span overlap detection is symmetric" $
            \span1 span2 ->
              _doSpansOverlap span1 span2 == _doSpansOverlap span2 span1
        ]

    , testGroup "Located value boundary conditions"
        [ testCase "locatedAt creates span with same start and end" $ do
            let pos = SourcePos 10 20 200
                value = "test"
                located = locatedAt pos value
            locSpan located @?= SourceSpan pos pos

        , fastProperty "mapLocated preserves position information" $
            \value1 value2 pos ->
              let located1 = locatedAt pos value1
                  located2 = mapLocated (const value2) located1
              in locPos located2 == locPos located1 && 
                 locSpan located2 == locSpan located1

        , fastProperty "located values maintain equality structure" $
            \value pos ->
              let located1 = locatedAt pos value
                  located2 = locatedAt pos value
              in located1 == located2
        ]

    , testGroup "Position containment and overlap"
        [ testCase "position containment works for span boundaries" $ do
            let start = SourcePos 1 1 0
                end = SourcePos 1 5 4
                span = SourceSpan start end
                inside = SourcePos 1 3 2
                outside = SourcePos 1 6 5
            _isPosInSpan inside span @?= True
            _isPosInSpan outside span @?= False
            _isPosInSpan start span @?= True
            _isPosInSpan end span @?= True

        , fastProperty "span overlap with zero-length spans" $
            \pos ->
              let zeroSpan = SourceSpan pos pos
                  normalSpan = spanBetween pos (posAfter 'x' pos)
              in _doSpansOverlap zeroSpan normalSpan

        , fastProperty "identical spans always overlap" $
            \span ->
              _doSpansOverlap span span

        , testCase "non-overlapping spans on different lines" $ do
            let span1 = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
                span2 = SourceSpan (SourcePos 2 1 10) (SourcePos 2 10 19)
            _doSpansOverlap span1 span2 @?= False
        ]

    , testGroup "Extreme boundary conditions"
        [ testCase "position advancement with empty string" $ do
            let pos = startPos
                result = pos `posAfter` ' ' -- Single character
            posOffset result @?= 1

        , fastProperty "span merging with identical spans preserves identity" $
            \span ->
              mergeSpans span span == span

        , testCase "tab advancement at column boundaries" $ do
            let pos1 = SourcePos 1 1 0 -- Before first tab stop
                pos2 = SourcePos 1 8 7 -- At tab stop
                pos3 = SourcePos 1 9 8 -- After tab stop
                newPos1 = posAfter '\t' pos1
                newPos2 = posAfter '\t' pos2
                newPos3 = posAfter '\t' pos3
            posColumn newPos1 @?= 9 -- Next tab stop
            posColumn newPos2 @?= 17 -- Next tab stop
            posColumn newPos3 @?= 17 -- Next tab stop
        ]
    ]