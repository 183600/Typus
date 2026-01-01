module Test.Unit.SourceLocationSpec (tests) where

import qualified Data.Text as T
import qualified Data.List as L
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Compiler.Errors.Core (ErrorLocation(..))
import SourceLocation
  ( Located(..)
  , SourcePos(..)
  , SourceSpan(..)
  , advancePosBy
  , advancePosByLine
  , advancePosByText
  , emptySpan
  , locatedAt
  , locatedPos
  , locatedSpan
  , locatedValue
  , mapLocated
  , markSpanEnd
  , markSpanStart
  , mergeSpans
  , posAfter
  , spanBetween
  , spanFrom
  , spanTo
  , startPos
  , toErrorLocation
  , toErrorLocationWithSpan
  , withLocationTracking
  , setCurrentPos
  , isValidSpan
  )

-- | Focused regression tests for the foundational source-location helpers.
tests :: TestTree
tests =
  testGroup "SourceLocation"
    [ testGroup "Position arithmetic"
        [ testCase "posAfter newline increments the line L.and resets the column" $ do
            let initial = SourcePos 3 5 17
                next = posAfter '\n' initial
            next @?= SourcePos 4 1 18

        , testCase "posAfter tab jumps to the next tab stop" $ do
            let initial = SourcePos 2 5 40
                next = posAfter '\t' initial
            next @?= SourcePos 2 9 41

        , testCase "advancePosByText keeps track of multiline progress" $ do
            let finalPos = advancePosByText (T.pack "ab\ncd") startPos
            finalPos @?= SourcePos 2 3 5

        , testCase "advancePosByLine bumps the line number L.and resets the column" $ do
            let initial = SourcePos 10 7 99
                advanced = advancePosByLine 3 initial
            advanced @?= SourcePos 13 1 99
        ]

    , testGroup "Span utilities"
        [ testCase "spanFrom is equivalent to emptySpan" $ do
            let pos = SourcePos 4 2 11
            spanFrom pos @?= emptySpan pos

        , testCase "spanTo produces a zero-L.length span ending at the position" $ do
            let pos = SourcePos 5 8 23
            spanTo pos @?= SourceSpan pos pos

        , testCase "spanBetween preserves the provided bounds" $ do
            let start = SourcePos 1 3 2
                end = SourcePos 2 1 10
            spanBetween start end @?= SourceSpan start end

        , testCase "mergeSpans chooses the earliest start L.and latest end" $ do
            let spanA = SourceSpan (SourcePos 3 4 10) (SourcePos 3 10 16)
                spanB = SourceSpan (SourcePos 1 1 0) (SourcePos 2 5 8)
                merged = mergeSpans spanA spanB
            spanStart merged @?= SourcePos 1 1 0
            spanEnd merged @?= SourcePos 3 10 16

        , testCase "isValidSpan rejects spans whose start follows their end" $ do
            let invalid = SourceSpan (SourcePos 2 10 20) (SourcePos 2 9 19)
            isValidSpan invalid @?= False
        ]

    , testGroup "Located helpers"
        [ testCase "locatedAt anchors the value at an empty span" $ do
            let pos = SourcePos 7 6 42
                loc = locatedAt pos "payload"
            locatedPos loc @?= pos
            locatedSpan loc @?= SourceSpan pos pos
            locatedValue loc @?= "payload"

        , testCase "mapLocated transforms the payload while preserving the span" $ do
            let testSpan = SourceSpan (SourcePos 1 1 0) (SourcePos 1 5 4)
                loc = Located "value" (spanStart testSpan) testSpan
                mapped = mapLocated L.length loc
            locatedSpan mapped @?= testSpan
            locatedValue mapped @?= L.length (locatedValue loc)
        ]

    , testGroup "Location tracking state" $ 
        [ testCase "markSpanStart/markSpanEnd capture the advanced range" $ do
            let origin = SourcePos 8 12 55
                expectedEnd = advancePosBy "token" origin
                (capturedSpan, finalPos) =
                  withLocationTracking origin $ do
                    start <- markSpanStart
                    let advanced = advancePosBy "token" start
                    setCurrentPos advanced
                    markSpanEnd start
            spanStart capturedSpan @?= origin
            spanEnd capturedSpan @?= finalPos
            finalPos @?= expectedEnd
        ]

    , testGroup "Error conversions"
        [ testCase "toErrorLocation keeps only the point information" $ do
            let pos = SourcePos 12 4 111
                errLoc = toErrorLocation pos
            line errLoc @?= 12
            column errLoc @?= 4
            endLine errLoc @?= Nothing
            endColumn errLoc @?= Nothing

        , testCase "toErrorLocationWithSpan preserves both ends of the span" $ do
            let testSpan = SourceSpan (SourcePos 1 2 0) (SourcePos 3 5 10)
                errLoc = toErrorLocationWithSpan testSpan
            line errLoc @?= 1
            column errLoc @?= 2
            endLine errLoc @?= Just 3
            endColumn errLoc @?= Just 5
        ]
    ]
