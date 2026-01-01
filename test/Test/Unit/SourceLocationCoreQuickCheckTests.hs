{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.Unit.SourceLocationCoreQuickCheckTests (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertBool, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf, elements, oneof, suchThat)
import Test.QuickCheck.Arbitrary (Arbitrary(..))

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
import Data.Char (isSpace, isPrint)
import qualified Data.List as Data.List

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 1000)
    column <- choose (1, 1000)
    offset <- choose (0, 100000)
    return $ SourcePos line column offset

instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    -- Ensure end is not before start
    lineOffset <- choose (0, 50)
    colOffset <- choose (0, 100)
    offsetOffset <- choose (0, 1000)
    let end = SourcePos 
          { posLine = posLine start + lineOffset
          , posColumn = if lineOffset == 0 then posColumn start + colOffset else choose (1, 1000)
          , posOffset = posOffset start + offsetOffset
          }
    return $ SourceSpan start end

instance Arbitrary a => Arbitrary (Located a) where
  arbitrary = do
    value <- arbitrary
    pos <- arbitrary
    span <- arbitrary
    return $ Located value pos span

-- ============================================================================
-- Source Position Properties
-- ============================================================================

-- Property: startPos is the reference starting position
prop_startPos_properties :: Property
prop_startPos_properties =
  property $ 
    posLine startPos === 1 .&&.
    posColumn startPos === 1 .&&.
    posOffset startPos === 0

-- Property: posAfter correctly handles newline
prop_posAfter_newline :: SourcePos -> Property
prop_posAfter_newline pos =
  let newPos = posAfter '\n' pos
  in property $
    posLine newPos === posLine pos + 1 .&&.
    posColumn newPos === 1 .&&.
    posOffset newPos === posOffset pos + 1

-- Property: posAfter correctly handles tab
prop_posAfter_tab :: SourcePos -> Property
prop_posAfter_tab pos =
  let newPos = posAfter '\t' pos
      expectedCol = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in property $
    posColumn newPos === expectedCol .&&.
    posOffset newPos === posOffset pos + 1

-- Property: posAfter correctly handles regular characters
prop_posAfter_regular :: SourcePos -> Char -> Property
prop_posAfter_regular pos char =
  char /= '\n' && char /= '\t' ==>
  let newPos = posAfter char pos
  in property $
    posLine newPos === posLine pos .&&.
    posColumn newPos === posColumn pos + 1 .&&.
    posOffset newPos === posOffset pos + 1

-- Property: posAt creates position with correct line L.and column
prop_posAt_creation :: Int -> Int -> Property
prop_posAt_creation line col =
  line > 0 && col > 0 ==>
  let pos = posAt line col
  in property $
    posLine pos === line .&&.
    posColumn pos === col .&&.
    posOffset pos === 0

-- Property: posAtLineCol creates position with L.all fields
prop_posAtLineCol_creation :: Int -> Int -> Int -> Property
prop_posAtLineCol_creation line col offset =
  line > 0 && col > 0 && offset >= 0 ==>
  let pos = posAtLineCol line col offset
  in property $
    posLine pos === line .&&.
    posColumn pos === col .&&.
    posOffset pos === offset

-- Property: advancePos is same as posAfter
prop_advancePos_consistency :: SourcePos -> Char -> Property
prop_advancePos_consistency pos char =
  advancePos char pos === posAfter char pos

-- Property: advancePosBy correctly processes multiple characters
prop_advancePosBy_multiple :: SourcePos -> String -> Property
prop_advancePosBy_multiple pos chars =
  let singleAdvances = L.foldl (flip posAfter) pos chars
      multiAdvance = advancePosBy chars pos
  in property $ singleAdvances === multiAdvance

-- Property: advancePosByText works with Text
prop_advancePosByText_text :: SourcePos -> String -> Property
prop_advancePosByText_text pos str =
  let text = T.pack str
      byString = advancePosBy str pos
      byText = advancePosByText text pos
  in property $ byString === byText

-- Property: advancePosByLine correctly advances by whole lines
prop_advancePosByLine_correct :: SourcePos -> Int -> Property
prop_advancePosByLine_correct pos numLines =
  numLines >= 0 ==>
  let newPos = advancePosByLine numLines pos
  in property $
    posLine newPos === posLine pos + numLines .&&.
    posColumn newPos === 1

-- ============================================================================
-- Source Span Properties
-- ============================================================================

-- Property: emptySpan creates span with same start L.and end
prop_emptySpan_properties :: SourcePos -> Property
prop_emptySpan_properties pos =
  let span = emptySpan pos
  in property $
    spanStart span === pos .&&.
    spanEnd span === pos

-- Property: spanFrom is same as emptySpan
prop_spanFrom_consistency :: SourcePos -> Property
prop_spanFrom_consistency pos =
  spanFrom pos === emptySpan pos

-- Property: spanTo creates span ending at position
prop_spanTo_properties :: SourcePos -> Property
prop_spanTo_properties pos =
  let span = spanTo pos
  in property $
    spanStart span === pos .&&.
    spanEnd span === pos

-- Property: spanBetween creates correct span
prop_spanBetween_correct :: SourcePos -> SourcePos -> Property
prop_spanBetween_correct start end =
  let span = spanBetween start end
  in property $
    spanStart span === start .&&.
    spanEnd span === end

-- Property: mergeSpans creates span covering both spans
prop_mergeSpans_correct :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_correct span1 span2 =
  let merged = mergeSpans span1 span2
  in property $
    spanStart merged === min (spanStart span1) (spanStart span2) .&&.
    spanEnd merged === max (spanEnd span1) (spanEnd span2)

-- Property: mergeSpans is commutative
prop_mergeSpans_commutative :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_commutative span1 span2 =
  mergeSpans span1 span2 === mergeSpans span2 span1

-- Property: mergeSpans is associative
prop_mergeSpans_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_mergeSpans_associative span1 span2 span3 =
  mergeSpans (mergeSpans span1 span2) span3 === mergeSpans span1 (mergeSpans span2 span3)

-- Property: isValidSpan correctly validates spans
prop_isValidSpan_correct :: SourcePos -> SourcePos -> Property
prop_isValidSpan_correct start end =
  let span = spanBetween start end
      valid = start <= end
  in property $ isValidSpan span === valid

-- ============================================================================
-- Located Value Properties
-- ============================================================================

-- Property: locatedAt creates located value at position
prop_locatedAt_properties :: SourcePos -> String -> Property
prop_locatedAt_properties pos value =
  let located = locatedAt pos value
      expectedSpan = emptySpan pos
  in property $
    locatedValue located === value .&&.
    locatedPos located === pos .&&.
    locatedSpan located === expectedSpan

-- Property: locatedWithSpan creates located value with span
prop_locatedWithSpan_properties :: SourceSpan -> String -> Property
prop_locatedWithSpan_properties span value =
  let located = locatedWithSpan span value
  in property $
    locatedValue located === value .&&.
    locatedPos located === spanStart span .&&.
    locatedSpan located === span

-- Property: mapLocated correctly maps function over value
prop_mapLocated_properties :: SourceSpan -> String -> String -> Property
prop_mapLocated_properties span input output =
  let located = locatedWithSpan span input
      mapped = mapLocated (++ output) located
  in property $
    locatedValue mapped === input ++ output .&&.
    locatedPos mapped === locatedPos located .&&.
    locatedSpan mapped === locatedSpan located

-- Property: mapLocated preserves position L.and span
prop_mapLocated_preserves_location :: SourceSpan -> Int -> Property
prop_mapLocated_preserves_location span value =
  let located = locatedWithSpan span value
      mapped = mapLocated (*2) located
  in property $
    locatedPos mapped === locatedPos located .&&.
    locatedSpan mapped === locatedSpan located

-- ============================================================================
-- Error Location Conversion Properties
-- ============================================================================

-- Property: toErrorLocation creates correct error location
prop_toErrorLocation_correct :: SourcePos -> Property
prop_toErrorLocation_correct pos =
  let errLoc = toErrorLocation pos
  in property $
    line errLoc === posLine pos .&&.
    column errLoc === posColumn pos .&&.
    filePath errLoc === Nothing .&&.
    endLine errLoc === Nothing .&&.
    endColumn errLoc === Nothing

-- Property: toErrorLocationWithSpan creates correct error location with range
prop_toErrorLocationWithSpan_correct :: SourceSpan -> Property
prop_toErrorLocationWithSpan_correct span =
  let errLoc = toErrorLocationWithSpan span
      start = spanStart span
      end = spanEnd span
  in property $
    line errLoc === posLine start .&&.
    column errLoc === posColumn start .&&.
    endLine errLoc === Just (posLine end) .&&.
    endColumn errLoc === Just (posColumn end) .&&.
    filePath errLoc === Nothing

-- ============================================================================
-- Advanced Properties
-- ============================================================================

-- Property: Position advancement is consistent with offset
prop_advancePos_offset_consistency :: SourcePos -> String -> Property
prop_advancePos_offset_consistency pos str =
  let finalPos = advancePosBy str pos
      expectedOffset = posOffset pos + L.length str
  in property $ posOffset finalPos === expectedOffset

-- Property: Span merging with empty spans
prop_mergeSpans_with_empty :: SourcePos -> SourcePos -> Property
prop_mergeSpans_with_empty pos1 pos2 =
  let empty1 = emptySpan pos1
      empty2 = emptySpan pos2
      span1 = spanBetween pos1 pos2
      merged1 = mergeSpans empty1 span1
      merged2 = mergeSpans span1 empty2
  in property $
    merged1 === span1 .&&.
    merged2 === span1

-- Property: Located value roundtrip
prop_located_roundtrip :: SourceSpan -> Int -> Property
prop_located_roundtrip span value =
  let located = locatedWithSpan span value
      extractedValue = locatedValue located
      extractedPos = locatedPos located
      extractedSpan = locatedSpan located
      reconstructed = locatedWithSpan extractedSpan extractedValue
  in property $
    located === reconstructed

-- Property: Position ordering consistency
prop_position_ordering_consistent :: SourcePos -> SourcePos -> Property
prop_position_ordering_consistent pos1 pos2 =
  let span1 = spanBetween pos1 pos2
      span2 = spanBetween pos2 pos1
      merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
  in property $ merged1 === merged2

-- Property: Text advancement preserves Unicode characters
prop_advancePos_unicode :: SourcePos -> String -> Property
prop_advancePos_unicode pos str =
  let hasUnicode = L.any (> '\127') str
      advancedPos = advancePosBy str pos
      offsetDiff = posOffset advancedPos - posOffset pos
  in classify hasUnicode "contains Unicode" $
     property $ offsetDiff === L.length str

-- Property: Span validity after operations
prop_span_operations_preserve_validity :: SourceSpan -> SourceSpan -> Property
prop_span_operations_preserve_validity span1 span2 =
  let valid1 = isValidSpan span1
      valid2 = isValidSpan span2
      merged = mergeSpans span1 span2
      mergedValid = isValidSpan merged
  in property $
    (valid1 && valid2) ==> mergedValid

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "SourceLocation Core QuickCheck Tests"
  [ testGroup "Source Position Properties"
    [ fastProperty "startPos has correct values" prop_startPos_properties
    , fastProperty "posAfter handles newline correctly" prop_posAfter_newline
    , fastProperty "posAfter handles tab correctly" prop_posAfter_tab
    , fastProperty "posAfter handles regular characters" prop_posAfter_regular
    , fastProperty "posAt creates position correctly" prop_posAt_creation
    , fastProperty "posAtLineCol creates position with L.all fields" prop_posAtLineCol_creation
    , fastProperty "advancePos is consistent with posAfter" prop_advancePos_consistency
    , fastProperty "advancePosBy processes multiple characters" prop_advancePosBy_multiple
    , fastProperty "advancePosByText works with Text" prop_advancePosByText_text
    , fastProperty "advancePosByLine advances by whole lines" prop_advancePosByLine_correct
    ]

  , testGroup "Source Span Properties"
    [ fastProperty "emptySpan creates span with same start L.and end" prop_emptySpan_properties
    , fastProperty "spanFrom is consistent with emptySpan" prop_spanFrom_consistency
    , fastProperty "spanTo creates span ending at position" prop_spanTo_properties
    , fastProperty "spanBetween creates correct span" prop_spanBetween_correct
    , fastProperty "mergeSpans creates span covering both spans" prop_mergeSpans_correct
    , fastProperty "mergeSpans is commutative" prop_mergeSpans_commutative
    , fastProperty "mergeSpans is associative" prop_mergeSpans_associative
    , fastProperty "isValidSpan correctly validates spans" prop_isValidSpan_correct
    ]

  , testGroup "Located Value Properties"
    [ fastProperty "locatedAt creates located value at position" prop_locatedAt_properties
    , fastProperty "locatedWithSpan creates located value with span" prop_locatedWithSpan_properties
    , fastProperty "mapLocated correctly maps function over value" prop_mapLocated_properties
    , fastProperty "mapLocated preserves position L.and span" prop_mapLocated_preserves_location
    , fastProperty "located value roundtrip" prop_located_roundtrip
    ]

  , testGroup "Error Location Conversion Properties"
    [ fastProperty "toErrorLocation creates correct error location" prop_toErrorLocation_correct
    , fastProperty "toErrorLocationWithSpan creates correct error location with range" prop_toErrorLocationWithSpan_correct
    ]

  , testGroup "Advanced Properties"
    [ fastProperty "position advancement is consistent with offset" prop_advancePos_offset_consistency
    , fastProperty "span merging with empty spans" prop_mergeSpans_with_empty
    , fastProperty "position ordering consistency" prop_position_ordering_consistent
    , fastProperty "text advancement preserves Unicode characters" prop_advancePos_unicode
    , fastProperty "span validity after operations" prop_span_operations_preserve_validity
    ]
  ]