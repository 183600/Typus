{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.Unit.EnhancedSourceLocationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, assertBool, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

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
  , advancePosByText
  , advancePosByLine
  , toErrorLocation
  , toErrorLocationWithSpan
  )

import Compiler.Errors.Core (ErrorLocation(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isSpace)

-- | Enhanced tests for SourceLocation module
tests :: TestTree
tests =
  testGroup "Enhanced SourceLocation tests"
    [ testGroup "SourcePos operations"
        [ testCase "startPos has correct initial values" $ do
            posLine startPos @?= 1
            posColumn startPos @?= 1
            posOffset startPos @?= 0

        , testCase "posAfter handles newline correctly" $ do
            let pos = posAfter '\n' startPos
            posLine pos @?= 2
            posColumn pos @?= 1
            posOffset pos @?= 1

        , testCase "posAfter handles tab correctly (8-space tabs)" $ do
            let pos = posAfter '\t' startPos
            posLine pos @?= 1
            posColumn pos @?= 9  -- Next tab stop at column 9
            posOffset pos @?= 1

        , testCase "posAfter handles regular character" $ do
            let pos = posAfter 'a' startPos
            posLine pos @?= 1
            posColumn pos @?= 2
            posOffset pos @?= 1

        , testCase "posAt creates position at specific line L.and column" $ do
            let pos = posAt 5 10
            posLine pos @?= 5
            posColumn pos @?= 10
            posOffset pos @?= 0  -- Offset is 0 by default

        , testCase "posAtLineCol creates position with full info" $ do
            let pos = posAtLineCol 3 7 42
            posLine pos @?= 3
            posColumn pos @?= 7
            posOffset pos @?= 42
        ]

    , testGroup "SourceSpan operations"
        [ testCase "emptySpan creates span with same start L.and end" $ do
            let pos = posAt 2 3
            let span = emptySpan pos
            spanStart span @?= pos
            spanEnd span @?= pos

        , testCase "spanFrom creates empty span at position" $ do
            let pos = posAt 1 5
            let span = spanFrom pos
            spanStart span @?= pos
            spanEnd span @?= pos

        , testCase "spanTo creates empty span at position" $ do
            let pos = posAt 4 2
            let span = spanTo pos
            spanStart span @?= pos
            spanEnd span @?= pos

        , testCase "spanBetween creates span between two positions" $ do
            let start = posAt 1 1
            let end = posAt 2 5
            let span = spanBetween start end
            spanStart span @?= start
            spanEnd span @?= end

        , testCase "mergeSpans combines two spans correctly" $ do
            let span1 = spanBetween (posAt 1 1) (posAt 1 10)
            let span2 = spanBetween (posAt 2 1) (posAt 2 15)
            let merged = mergeSpans span1 span2
            spanStart merged @?= posAt 1 1
            spanEnd merged @?= posAt 2 15

        , testCase "isValidSpan checks span validity" $ do
            let validSpan = spanBetween (posAt 1 1) (posAt 1 5)
            let invalidSpan = spanBetween (posAt 2 1) (posAt 1 5)
            assertBool "validSpan should be valid" $ isValidSpan validSpan
            assertBool "invalidSpan should be invalid" $ not $ isValidSpan invalidSpan
        ]

    , testGroup "Located value operations"
        [ testCase "locatedAt creates located value at position" $ do
            let pos = posAt 3 4
            let located = locatedAt pos "test"
            locatedValue located @?= "test"
            locatedPos located @?= pos
            spanStart (locatedSpan located) @?= pos
            spanEnd (locatedSpan located) @?= pos

        , testCase "locatedWithSpan creates located value with span" $ do
            let span = spanBetween (posAt 1 1) (posAt 1 5)
            let located = locatedWithSpan span 42
            locatedValue located @?= 42
            locatedSpan located @?= span

        , testCase "mapLocated transforms located value" $ do
            let pos = posAt 2 3
            let located = locatedAt pos [1, 2, 3]
            let transformed = mapLocated L.sum located
            locatedValue transformed @?= 6
            locatedPos transformed @?= pos
        ]

    , testGroup "Position advancement"
        [ testCase "advancePosBy advances by multiple characters" $ do
            let pos = advancePosBy "hello" startPos
            posLine pos @?= 1
            posColumn pos @?= 6
            posOffset pos @?= 5

        , testCase "advancePosByText advances by text" $ do
            let text = T.pack "test\nline"
            let pos = advancePosByText text startPos
            posLine pos @?= 2
            posColumn pos @?= 5
            posOffset pos @?= 9

        , testCase "advancePosByLine advances by lines" $ do
            let pos = advancePosByLine 3 startPos
            posLine pos @?= 4
            posColumn pos @?= 1
            posOffset pos @?= 3
        ]

    , testGroup "Error location conversion"
        [ testCase "toErrorLocation converts position to error location" $ do
            let pos = posAtLineCol 5 10 42
            let errLoc = toErrorLocation pos
            filePath errLoc @?= Nothing
            line errLoc @?= 5
            column errLoc @?= 10
            endLine errLoc @?= Nothing
            endColumn errLoc @?= Nothing

        , testCase "toErrorLocationWithSpan converts span to error location" $ do
            let span = spanBetween (posAt 2 3) (posAt 4 8)
            let errLoc = toErrorLocationWithSpan span
            filePath errLoc @?= Nothing
            line errLoc @?= 2
            column errLoc @?= 3
            endLine errLoc @?= Just 4
            endColumn errLoc @?= Just 8
        ]

    , testGroup "Property-based tests"
        [ fastProperty "posAfter newline increments line L.and resets column" prop_posAfter_newline
        , fastProperty "posAfter tab advances to next tab stop" prop_posAfter_tab
        , fastProperty "posAfter regular character increments column L.and offset" prop_posAfter_regular
        , fastProperty "spanBetween always creates valid span" prop_spanBetween_valid
        , fastProperty "mergeSpans is commutative" prop_mergeSpans_commutative
        , fastProperty "advancePosBy is consistent with repeated posAfter" prop_advancePosBy_consistent
        , fastProperty "locatedAt L.and locatedWithSpan preserve position info" prop_located_preserves_info
        ]
    ]

-- Property tests

prop_posAfter_newline :: SourcePos -> Property
prop_posAfter_newline pos =
  let newPos = posAfter '\n' pos
  in property $ posLine newPos === posLine pos + 1 .&&.
             posColumn newPos === 1 .&&.
             posOffset newPos === posOffset pos + 1

prop_posAfter_tab :: SourcePos -> Property
prop_posAfter_tab pos =
  let newPos = posAfter '\t' pos
      expectedColumn = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in property $ posLine newPos === posLine pos .&&.
             posColumn newPos === expectedColumn .&&.
             posOffset newPos === posOffset pos + 1

prop_posAfter_regular :: Char -> SourcePos -> Property
prop_posAfter_regular c pos =
  c /= '\n' && c /= '\t' ==>
  let newPos = posAfter c pos
  in property $ posLine newPos === posLine pos .&&.
             posColumn newPos === posColumn pos + 1 .&&.
             posOffset newPos === posOffset pos + 1

prop_spanBetween_valid :: SourcePos -> SourcePos -> Property
prop_spanBetween_valid start end =
  let span = spanBetween start end
  in property $ spanStart span === start .&&.
             spanEnd span === end .&&.
             isValidSpan span === (start <= end)

prop_mergeSpans_commutative :: SourcePos -> SourcePos -> SourcePos -> SourcePos -> Property
prop_mergeSpans_commutative start1 end1 start2 end2 =
  let span1 = spanBetween start1 end1
      span2 = spanBetween start2 end2
      merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
  in property $ merged1 === merged2

prop_advancePosBy_consistent :: String -> Property
prop_advancePosBy_consistent str =
  let pos1 = advancePosBy str startPos
      pos2 = L.foldl (flip posAfter) startPos str
  in property $ pos1 === pos2

prop_located_preserves_info :: SourcePos -> String -> Property
prop_located_preserves_info pos value =
  let located = locatedAt pos value
  in property $ locatedValue located === value .&&.
             locatedPos located === pos .&&.
             spanStart (locatedSpan located) === pos .&&.
             spanEnd (locatedSpan located) === pos