module Test.Unit.AdditionalSourceLocationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  , posAfter
  , posAt
  , posAtLineCol
  , emptySpan
  , spanFrom
  , spanTo
  , spanBetween
  , mergeSpans
  , isValidSpan
  , locatedAt
  , locatedWithSpan
  , locatedValue
  , locatedSpan
  , locatedPos
  , mapLocated
  , advancePos
  , advancePosBy
  , mergeSpans
  , spanStart
  , spanEnd
  )

-- | Additional unit tests for SourceLocation module
tests :: TestTree
tests =
  testGroup "Additional SourceLocation tests"
    [ testGroup "Position edge cases"
        [ testCase "posAfter handles various control characters" $ do
            let initial = SourcePos 1 1 0
                posAfterTab = posAfter '\t' initial
                posAfterNewline = posAfter '\n' initial
                posAfterCarriageReturn = posAfter '\r' initial
            posAfterTab @?= SourcePos 1 5 1
            posAfterNewline @?= SourcePos 2 1 1
            posAfterCarriageReturn @?= SourcePos 1 2 1

        , testCase "posAt creates position with correct offset" $ do
            let pos = posAt 5 10 42
            pos @?= SourcePos 5 10 42

        , testCase "posAtLineCol calculates offset correctly" $ do
            let pos = posAtLineCol 3 5 "line1\nline2\nline3"
            pos @?= SourcePos 3 5 12

        , testCase "advancePos handles empty string" $ do
            let result = advancePos "" startPos
            result @?= startPos

        , testCase "advancePosBy with zero offset" $ do
            let initial = SourcePos 5 10 42
                result = advancePosBy 0 initial
            result @?= initial
        ]

    , testGroup "Span validation and edge cases"
        [ testCase "isValidSpan identifies valid spans" $ do
            let validSpan = SourceSpan (SourcePos 1 1 0) (SourcePos 1 5 4)
                invalidSpan = SourceSpan (SourcePos 2 1 10) (SourcePos 1 1 0)
            isValidSpan validSpan @?= True
            isValidSpan invalidSpan @?= False

        , testCase "emptySpan creates zero-length span" $ do
            let pos = SourcePos 3 4 20
                span = emptySpan pos
            spanStart span @?= pos
            spanEnd span @?= pos

        , testCase "spanFrom and spanTo consistency" $ do
            let start = SourcePos 1 1 0
                end = SourcePos 1 3 2
                spanFromStart = spanFrom start
                spanToEnd = spanTo end
            spanStart spanFromStart @?= start
            spanEnd spanToEnd @?= end

        , testCase "spanBetween preserves order" $ do
            let start = SourcePos 1 1 0
                end = SourcePos 2 2 5
                span = spanBetween start end
            spanStart span @?= start
            spanEnd span @?= end
        ]

    , testGroup "Span merging edge cases"
        [ testCase "mergeSpans with identical spans" $ do
            let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 5 4)
                merged = mergeSpans span span
            merged @?= span

        , testCase "mergeSpans with nested spans" $ do
            let outer = SourceSpan (SourcePos 1 1 0) (SourcePos 3 1 20)
                inner = SourceSpan (SourcePos 2 1 5) (SourcePos 2 5 9)
                merged = mergeSpans outer inner
            merged @?= outer

        , testCase "mergeSpans chooses earliest start and latest end" $ do
            let span1 = SourceSpan (SourcePos 2 3 10) (SourcePos 2 8 15)
                span2 = SourceSpan (SourcePos 1 5 5) (SourcePos 3 1 20)
                merged = mergeSpans span1 span2
            spanStart merged @?= SourcePos 1 5 5
            spanEnd merged @?= SourcePos 3 1 20
        ]

    , testGroup "Located value operations"
        [ testCase "locatedAt creates simple located value" $ do
            let pos = SourcePos 2 3 10
                value = "test"
                located = locatedAt pos value
            locatedPos located @?= pos
            locatedValue located @?= value

        , testCase "locatedWithSpan creates spanned value" $ do
            let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 5 4)
                value = 42
                located = locatedWithSpan span value
            locatedSpan located @?= span
            locatedValue located @?= value

        , testCase "mapLocated transforms value" $ do
            let pos = SourcePos 1 1 0
                original = locatedAt pos "hello"
                transformed = mapLocated length original
            locatedValue transformed @?= 5
            locatedPos transformed @?= pos
        ]

    , testGroup "Position arithmetic edge cases"
        [ testCase "advancePosBy with negative offset" $ do
            let initial = SourcePos 2 5 15
                result = advancePosBy (-3) initial
            result @?= SourcePos 2 2 12

        , testCase "advancePosBy crosses line boundaries" $ do
            let initial = SourcePos 2 3 10
                result = advancePosBy 5 initial
            result @?= SourcePos 2 8 15

        , testCase "posAfter with multiple newlines" $ do
            let initial = SourcePos 1 5 4
                result = foldl posAfter initial "\n\n\n"
            result @?= SourcePos 4 1 7
        ]
    ]