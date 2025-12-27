{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.SourceLocationAdvancedSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
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

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sort)
import Data.Char (isSpace)

-- ============================================================================
-- Advanced SourceLocation Tests
-- ============================================================================

tests :: TestTree
tests =
  testGroup "SourceLocation Advanced Tests"
    [ testGroup "Position arithmetic properties"
        [ fastProperty "posAfter advances line number for newline" prop_posAfter_newline
        , fastProperty "posAfter advances column for regular chars" prop_posAfter_regular
        , fastProperty "posAfter handles tab expansion correctly" prop_posAfter_tab
        , fastProperty "advancePosBy is consistent with repeated posAfter" prop_advancePosBy_consistency
        , fastProperty "position advancement is monotonic" prop_position_monotonic
        ]

    , testGroup "Span operations"
        [ fastProperty "mergeSpans contains both original spans" prop_mergeSpans_contains
        , fastProperty "mergeSpans is commutative" prop_mergeSpans_commutative
        , fastProperty "mergeSpans is associative" prop_mergeSpans_associative
        , fastProperty "spanBetween creates valid span" prop_spanBetween_valid
        , fastProperty "emptySpan has zero length" prop_emptySpan_zero_length
        ]

    , testGroup "Located values"
        [ fastProperty "mapLocated preserves position" prop_mapLocated_preserves_position
        , fastProperty "locatedAt creates span with same start and end" prop_locatedAt_zero_span
        , fastProperty "locatedWithSpan preserves given span" prop_locatedWithSpan_preserves_span
        ]

    , testGroup "Text processing"
        [ fastProperty "advancePosByText handles Unicode correctly" prop_advancePosByText_unicode
        , fastProperty "advancePosByText handles multiline text" prop_advancePosByText_multiline
        , fastProperty "advancePosByLine preserves column" prop_advancePosByLine_preserves_column
        ]

    , testGroup "Error location conversion"
        [ fastProperty "toErrorLocation preserves line and column" prop_toErrorLocation_preserves
        , fastProperty "toErrorLocationWithSpan preserves range" prop_toErrorLocationWithSpan_preserves
        ]

    , testGroup "Edge cases and robustness"
        [ testCase "handles very large line numbers" test_large_line_numbers
        , testCase "handles very large column numbers" test_large_column_numbers
        , testCase "handles position at start of file" test_start_position
        , testCase "handles empty text advancement" test_empty_text_advancement
        ]
    ]

-- ============================================================================
-- Position Arithmetic Properties
-- ============================================================================

prop_posAfter_newline :: Property
prop_posAfter_newline =
  let pos = posAt 10 5
      newPos = posAfter '\n' pos
  in property $ posLine newPos === 11 .&&. posColumn newPos === 1 .&&. posOffset newPos === posOffset pos + 1

prop_posAfter_regular :: Property
prop_posAfter_regular =
  forAll arbitrary $ \c ->
    c /= '\n' && c /= '\t' ==>
    let pos = posAt 10 5
        newPos = posAfter c pos
    in property $ posLine newPos === 10 .&&. posColumn newPos === 6 .&&. posOffset newPos === posOffset pos + 1

prop_posAfter_tab :: Property
prop_posAfter_tab =
  let testPositions = [(1,1), (1,2), (1,8), (1,9), (1,16), (1,17)]
      results = map (\(line, col) -> posColumn (posAfter '\t' (posAt line col))) testPositions
      expected = [9, 9, 9, 17, 17, 25]
  in property $ results === expected

prop_advancePosBy_consistency :: Property
prop_advancePosBy_consistency =
  forAll arbitrary $ \str ->
    let pos = startPos
        advanced1 = advancePosBy str pos
        advanced2 = foldl (flip posAfter) pos str
    in property $ advanced1 === advanced2

prop_position_monotonic :: Property
prop_position_monotonic =
  forAll arbitrary $ \str ->
    not (null str) ==>
    let positions = scanl (flip posAfter) startPos str
        isMonotonic = all (\(p1, p2) -> posOffset p1 <= posOffset p2) (zip positions (tail positions))
    in property $ isMonotonic

-- ============================================================================
-- Span Operations Properties
-- ============================================================================

prop_mergeSpans_contains :: Property
prop_mergeSpans_contains =
  forAll arbitrary $ \span1 ->
  forAll arbitrary $ \span2 ->
    let merged = mergeSpans span1 span2
        contains1 = spanStart merged <= spanStart span1 && spanEnd merged >= spanEnd span1
        contains2 = spanStart merged <= spanStart span2 && spanEnd merged >= spanEnd span2
    in property $ contains1 .&&. contains2

prop_mergeSpans_commutative :: Property
prop_mergeSpans_commutative =
  forAll arbitrary $ \span1 ->
  forAll arbitrary $ \span2 ->
    let merged1 = mergeSpans span1 span2
        merged2 = mergeSpans span2 span1
    in property $ merged1 === merged2

prop_mergeSpans_associative :: Property
prop_mergeSpans_associative =
  forAll arbitrary $ \span1 ->
  forAll arbitrary $ \span2 ->
  forAll arbitrary $ \span3 ->
    let merged1 = mergeSpans (mergeSpans span1 span2) span3
        merged2 = mergeSpans span1 (mergeSpans span2 span3)
    in property $ merged1 === merged2

prop_spanBetween_valid :: Property
prop_spanBetween_valid =
  forAll arbitrary $ \pos1 ->
  forAll arbitrary $ \pos2 ->
    let span = spanBetween pos1 pos2
        valid = isValidSpan span
    in property $ valid

prop_emptySpan_zero_length :: Property
prop_emptySpan_zero_length =
  forAll arbitrary $ \pos ->
    let span = emptySpan pos
        length = posOffset (spanEnd span) - posOffset (spanStart span)
    in property $ length === 0

-- ============================================================================
-- Located Values Properties
-- ============================================================================

prop_mapLocated_preserves_position :: Property
prop_mapLocated_preserves_position =
  forAll arbitrary $ \pos ->
  forAll arbitrary $ \value ->
    let located = locatedAt pos value
        mapped = mapLocated (+1) located
    in property $ locatedPos located === locatedPos mapped .&&. 
                    locatedSpan located === locatedSpan mapped

prop_locatedAt_zero_span :: Property
prop_locatedAt_zero_span =
  forAll arbitrary $ \pos ->
  forAll arbitrary $ \value ->
    let located = locatedAt pos value
        span = locatedSpan located
    in property $ spanStart span === spanEnd span

prop_locatedWithSpan_preserves_span :: Property
prop_locatedWithSpan_preserves_span =
  forAll arbitrary $ \span ->
  forAll arbitrary $ \value ->
    let located = locatedWithSpan span value
        actualSpan = locatedSpan located
    in property $ span === actualSpan

-- ============================================================================
-- Text Processing Properties
-- ============================================================================

prop_advancePosByText_unicode :: Property
prop_advancePosByText_unicode =
  let unicodeText = "Hello 世界 🚀 Café"
      pos = startPos
      advanced = advancePosByText unicodeText pos
  in property $ posLine advanced === 1 .&&. posColumn advanced === T.length unicodeText + 1

prop_advancePosByText_multiline :: Property
prop_advancePosByText_multiline =
  let multilineText = "Line 1\nLine 2\nLine 3"
      pos = startPos
      advanced = advancePosByText multilineText pos
  in property $ posLine advanced === 3 .&&. 
                    posColumn advanced === 7 .&&.
                    posOffset advanced === T.length multilineText

prop_advancePosByLine_preserves_column :: Property
prop_advancePosByLine_preserves_column =
  forAll arbitrary $ \pos ->
  forAll arbitrary $ \numLines ->
    numLines >= 0 && numLines <= 100 ==>
    let newPos = advancePosByLine numLines pos
    in property $ posColumn newPos === 1

-- ============================================================================
-- Error Location Conversion Properties
-- ============================================================================

prop_toErrorLocation_preserves :: Property
prop_toErrorLocation_preserves =
  forAll arbitrary $ \pos ->
    let errorLoc = toErrorLocation pos
    in property $ line errorLoc === posLine pos .&&. 
                    column errorLoc === posColumn pos

prop_toErrorLocationWithSpan_preserves :: Property
prop_toErrorLocationWithSpan_preserves =
  forAll arbitrary $ \span ->
    let errorLoc = toErrorLocationWithSpan span
    in property $ line errorLoc === posLine (spanStart span) .&&.
                    column errorLoc === posColumn (spanStart span) .&&.
                    endLine errorLoc === Just (posLine (spanEnd span)) .&&.
                    endColumn errorLoc === Just (posColumn (spanEnd span))

-- ============================================================================
-- Edge Cases and Robustness Tests
-- ============================================================================

test_large_line_numbers :: IO ()
test_large_line_numbers = do
  let largeLine = 1000000
      pos = posAt largeLine 50
      span = spanBetween pos (advancePosByLine 10 pos)
  posLine pos @?= largeLine
  isValidSpan span @?= True

test_large_column_numbers :: IO ()
test_large_column_numbers = do
  let largeColumn = 1000000
      pos = posAtLineCol 1 largeColumn
  posColumn pos @?= largeColumn

test_start_position :: IO ()
test_start_position = do
  startPos @?= SourcePos 1 1 0
  let span = emptySpan startPos
  spanStart span @?= startPos
  spanEnd span @?= startPos

test_empty_text_advancement :: IO ()
test_empty_text_advancement = do
  let pos = startPos
      advanced = advancePosByText "" pos
  pos @?= advanced