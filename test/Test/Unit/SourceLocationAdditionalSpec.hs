module Test.Unit.SourceLocationAdditionalSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import SourceLocation

-- | Additional unit tests for SourceLocation module
tests :: TestTree
tests =
  testGroup "Additional SourceLocation tests"
    [ testGroup "SourcePos operations"
        [ testCase "posAfter handles newline correctly" $ do
            let start = startPos { posLine = 1, posColumn = 5 }
                result = posAfter '\n' start
            result @?= startPos { posLine = 2, posColumn = 1, posOffset = 5 }

        , testCase "posAfter handles tab correctly" $ do
            let start = startPos { posColumn = 3 }
                result = posAfter '\t' start
            result @?= startPos { posColumn = 9, posOffset = 1 }

        , testCase "posAfter handles regular character" $ do
            let start = startPos { posColumn = 5 }
                result = posAfter 'x' start
            result @?= startPos { posColumn = 6, posOffset = 1 }

        , testCase "posAt creates position at specific line and column" $ do
            let pos = posAt 10 20
            pos @?= SourcePos 10 20 0

        , testCase "posAtLineCol creates position with offset" $ do
            let pos = posAtLineCol 5 15 100
            pos @?= SourcePos 5 15 100
        ]

    , testGroup "SourceSpan operations"
        [ testCase "emptySpan creates span with same start and end" $ do
            let pos = posAt 3 7
                span = emptySpan pos
            span @?= SourceSpan pos pos

        , testCase "spanFrom creates empty span at position" $ do
            let pos = posAt 2 4
                span = spanFrom pos
            span @?= emptySpan pos

        , testCase "spanTo creates empty span at position" $ do
            let pos = posAt 8 12
                span = spanTo pos
            span @?= SourceSpan pos pos

        , testCase "spanBetween creates span between two positions" $ do
            let start = posAt 1 5
                end = posAt 3 10
                span = spanBetween start end
            span @?= SourceSpan start end

        , testCase "mergeSpans combines spans correctly" $ do
            let span1 = SourceSpan (posAt 1 5) (posAt 2 10)
                span2 = SourceSpan (posAt 1 3) (posAt 3 8)
                merged = mergeSpans span1 span2
            merged @?= SourceSpan (posAt 1 3) (posAt 3 8)

        , testCase "isValidSpan checks span validity" $ do
            let validSpan = SourceSpan (posAt 1 1) (posAt 2 5)
                invalidSpan = SourceSpan (posAt 3 10) (posAt 2 5)
            isValidSpan validSpan @?= True
            isValidSpan invalidSpan @?= False
        ]

    , testGroup "Located operations"
        [ testCase "locatedAt creates located value at position" $ do
            let pos = posAt 4 8
                value = "test"
                located = locatedAt pos value
            locatedValue located @?= value
            locatedPos located @?= pos
            locatedSpan located @?= emptySpan pos

        , testCase "locatedWithSpan creates located value with span" $ do
            let span = SourceSpan (posAt 1 2) (posAt 3 6)
                value = 42
                located = locatedWithSpan span value
            locatedValue located @?= value
            locatedSpan located @?= span
            locatedPos located @?= spanStart span

        , testCase "mapLocated transforms located value" $ do
            let span = SourceSpan (posAt 1 1) (posAt 1 5)
                original = locatedWithSpan span "hello"
                transformed = mapLocated length original
            locatedValue transformed @?= 5
            locatedSpan transformed @?= span
        ]

    , testGroup "Position advancement"
        [ testCase "advancePosBy advances by multiple characters" $ do
            let start = startPos
                result = advancePosBy "abc" start
            result @?= startPos { posColumn = 4, posOffset = 3 }

        , testCase "advancePosBy handles newlines in string" $ do
            let start = startPos
                result = advancePosBy "ab\nc" start
            result @?= startPos { posLine = 2, posColumn = 2, posOffset = 4 }

        , testCase "advancePosByText advances by text content" $ do
            let start = startPos
                text = "hello\nworld"
                result = advancePosByText text start
            result @?= startPos { posLine = 2, posColumn = 6, posOffset = 11 }

        , testCase "advancePosByLine advances by specified number of lines" $ do
            let start = posAt 3 10
                result = advancePosByLine 5 start
            result @?= posAt 8 1 0
        ]

    , testGroup "Error location conversion"
        [ testCase "toErrorLocation converts position to error location" $ do
            let pos = posAt 10 20
                errorLoc = toErrorLocation pos
            errorLoc @?= ErrorLocation
              { filePath = Nothing
              , line = 10
              , column = 20
              , endLine = Nothing
              , endColumn = Nothing
              }

        , testCase "toErrorLocationWithSpan converts span to error location with range" $ do
            let span = SourceSpan (posAt 5 10) (posAt 7 15)
                errorLoc = toErrorLocationWithSpan span
            errorLoc @?= ErrorLocation
              { filePath = Nothing
              , line = 5
              , column = 10
              , endLine = Just 7
              , endColumn = Just 15
              }
        ]
    ]