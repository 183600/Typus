{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.SourceLocationMathSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Arbitrary(..), Gen, suchThat, listOf1, elements
  , frequency, oneof, sized, resize, Positive(..), NonEmptyList(..)
  , choose, getPositive
  )

import SourceLocation
  ( SourcePos(..), SourceSpan(..), Located(..)
  , startPos, posAfter, posAt, posAtLineCol
  , emptySpan, spanFrom, spanTo, spanBetween, mergeSpans, isValidSpan
  , locatedAt, locatedWithSpan, locatedValue, locatedSpan, locatedPos, mapLocated
  , advancePos, advancePosBy, advancePosByText, advancePosByLine
  , toErrorLocation, toErrorLocationWithSpan
  , comparePos, _isPosInSpan, _doSpansOverlap, _spanLength
  , _minPos, _maxPos, _spanCovering, _spanContains, _spansOverlap
  , _posDistance, _lineDistance, _posAtLine, _posAtLineEnd, _spanToRangeDesc
  )

import Data.Char (isSpace)
import Data.List (sort)
import qualified Data.Text as T
import Compiler.Errors.Core (ErrorLocation(..))

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary SourcePos where
  arbitrary = do
    line <- getPositive <$> arbitrary
    column <- getPositive <$> arbitrary
    offset <- getPositive <$> arbitrary
    return $ SourcePos (line + 1) (column + 1) offset

instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    let endLine = posLine start + getPositive (snd arbitrary)
        endCol = if endLine == posLine start 
                 then max (posColumn start) (posColumn start + getPositive (snd arbitrary))
                 else getPositive (snd arbitrary) + 1
        endOffset = posOffset start + getPositive (snd arbitrary) + 1
        end = SourcePos endLine endCol endOffset
    return $ SourceSpan start end

instance Arbitrary a => Arbitrary (Located a) where
  arbitrary = do
    value <- arbitrary
    span <- arbitrary
    return $ Located value (spanStart span) span

-- ============================================================================
-- Source Position Properties
-- ============================================================================

-- Property: startPos has correct values
prop_startPos_values :: Property
prop_startPos_values =
  posLine startPos === 1 .&&.
  posColumn startPos === 1 .&&.
  posOffset startPos === 0

-- Property: posAfter newline increments line and resets column
prop_posAfter_newline :: Positive Int -> Property
prop_posAfter_newline (Positive lineNum) =
  let pos = posAt lineNum 5
      newPos = posAfter '\n' pos
  in posLine newPos === posLine pos + 1 .&&.
     posColumn newPos === 1 .&&.
     posOffset newPos === posOffset pos + 1

-- Property: posAfter tab advances to next tab stop
prop_posAfter_tab :: Positive Int -> Positive Int -> Property
prop_posAfter_tab (Positive lineNum) (Positive col) =
  let pos = posAt lineNum col
      newPos = posAfter '\t' pos
      expectedCol = ((col - 1) `div` 8 + 1) * 8 + 1
  in posColumn newPos === expectedCol .&&.
     posLine newPos === posLine pos .&&.
     posOffset newPos === posOffset pos + 1

-- Property: posAfter regular char increments column and offset
prop_posAfter_regular :: Positive Int -> Positive Int -> Char -> Property
prop_posAfter_regular (Positive lineNum) (Positive col) c =
  let notNewlineTab = c `notElem` ['\n', '\t']
      pos = posAt lineNum col
      newPos = posAfter c pos
  in notNewlineTab ==>
     posColumn newPos === posColumn pos + 1 .&&.
     posLine newPos === posLine pos .&&.
     posOffset newPos === posOffset pos + 1

-- Property: posAt creates position with correct line and column
prop_posAt_correct :: Positive Int -> Positive Int -> Property
prop_posAt_correct (Positive line) (Positive col) =
  let pos = posAt line col
  in posLine pos === line .&&. posColumn pos === col

-- Property: posAtLineCol creates position with correct values
prop_posAtLineCol_correct :: Positive Int -> Positive Int -> Positive Int -> Property
prop_posAtLineCol_correct (Positive line) (Positive col) (Positive offset) =
  let pos = posAtLineCol line col offset
  in posLine pos === line .&&. posColumn pos === col .&&. posOffset pos === offset

-- ============================================================================
-- Source Span Properties
-- ============================================================================

-- Property: emptySpan creates span with same start and end
prop_emptySpan_same_start_end :: SourcePos -> Property
prop_emptySpan_same_start_end pos =
  let span = emptySpan pos
  in spanStart span === pos .&&. spanEnd span === pos

-- Property: spanFrom creates empty span at position
prop_spanFrom_empty :: SourcePos -> Property
prop_spanFrom_empty pos =
  let span = spanFrom pos
  in spanStart span === pos .&&. spanEnd span === pos

-- Property: spanTo creates empty span at position
prop_spanTo_empty :: SourcePos -> Property
prop_spanTo_empty pos =
  let span = spanTo pos
  in spanStart span === pos .&&. spanEnd span === pos

-- Property: spanBetween creates span with given start and end
prop_spanBetween_correct :: SourcePos -> SourcePos -> Property
prop_spanBetween_correct start end =
  let span = spanBetween start end
  in spanStart span === start .&&. spanEnd span === end

-- Property: mergeSpans creates span covering both spans
prop_mergeSpans_covers :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_covers span1 span2 =
  let merged = mergeSpans span1 span2
  in spanStart merged === min (spanStart span1) (spanStart span2) .&&.
     spanEnd merged === max (spanEnd span1) (spanEnd span2)

-- Property: mergeSpans is idempotent
prop_mergeSpans_idempotent :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_idempotent span1 span2 =
  let merged1 = mergeSpans span1 span2
      merged2 = mergeSpans merged1 span2
  in merged1 === merged2

-- Property: mergeSpans is commutative
prop_mergeSpans_commutative :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_commutative span1 span2 =
  mergeSpans span1 span2 === mergeSpans span2 span1

-- Property: isValidSpan correctly identifies valid spans
prop_isValidSpan_correct :: SourceSpan -> Property
prop_isValidSpan_correct span =
  let start = spanStart span
      end = spanEnd span
      expected = start <= end
  in isValidSpan span === expected

-- ============================================================================
-- Located Values Properties
-- ============================================================================

-- Property: locatedAt creates located value with correct position
prop_locatedAt_correct :: SourcePos -> Int -> Property
prop_locatedAt_correct pos value =
  let located = locatedAt pos value
  in locatedValue located === value .&&.
     locatedPos located === pos .&&.
     spanStart (locatedSpan located) === pos .&&.
     spanEnd (locatedSpan located) === pos

-- Property: locatedWithSpan creates located value with correct span
prop_locatedWithSpan_correct :: SourceSpan -> String -> Property
prop_locatedWithSpan_correct span value =
  let located = locatedWithSpan span value
  in locatedValue located === value .&&.
     locatedSpan located === span .&&.
     locatedPos located === spanStart span

-- Property: mapLocated preserves location but transforms value
prop_mapLocated_preserves_location :: SourceSpan -> String -> Property
prop_mapLocated_preserves_location span value =
  let located = locatedWithSpan span value
      mapped = mapLocated length located
  in locatedSpan mapped === locatedSpan located .&&.
     locatedValue mapped === length value

-- ============================================================================
-- Position Advancement Properties
-- ============================================================================

-- Property: advancePos is same as posAfter
prop_advancePos_equals_posAfter :: SourcePos -> Char -> Property
prop_advancePos_equals_posAfter pos c =
  advancePos c pos === posAfter c pos

-- Property: advancePosBy empty string returns original position
prop_advancePosBy_empty :: SourcePos -> Property
prop_advancePosBy_empty pos =
  advancePosBy "" pos === pos

-- Property: advancePosBy is consistent with repeated advancePos
prop_advancePosBy_consistent :: SourcePos -> String -> Property
prop_advancePosBy_consistent pos s =
  advancePosBy s pos === foldl (flip advancePos) pos s

-- Property: advancePosByText is consistent with advancePosBy
prop_advancePosByText_consistent :: SourcePos -> String -> Property
prop_advancePosByText_consistent pos s =
  advancePosByText (T.pack s) pos === advancePosBy s pos

-- Property: advancePosByLine only changes line number and resets column
prop_advancePosByLine_correct :: SourcePos -> Positive Int -> Property
prop_advancePosByLine_correct pos (Positive lines) =
  let newPos = advancePosByLine lines pos
  in posLine newPos === posLine pos + lines .&&.
     posColumn newPos === 1

-- ============================================================================
-- Error Location Properties
-- ============================================================================

-- Property: toErrorLocation creates correct ErrorLocation
prop_toErrorLocation_correct :: SourcePos -> Property
prop_toErrorLocation_correct pos =
  let errLoc = toErrorLocation pos
  in line errLoc === posLine pos .&&.
     column errLoc === posColumn pos .&&.
     filePath errLoc === Nothing .&&.
     endLine errLoc === Nothing .&&.
     endColumn errLoc === Nothing

-- Property: toErrorLocationWithSpan creates correct ErrorLocation with range
prop_toErrorLocationWithSpan_correct :: SourceSpan -> Property
prop_toErrorLocationWithSpan_correct span =
  let errLoc = toErrorLocationWithSpan span
      start = spanStart span
      end = spanEnd span
  in line errLoc === posLine start .&&.
     column errLoc === posColumn start .&&.
     endLine errLoc === Just (posLine end) .&&.
     endColumn errLoc === Just (posColumn end) .&&.
     filePath errLoc === Nothing

-- ============================================================================
-- Position and Span Math Properties
-- ============================================================================

-- Property: _minPos returns the smaller position
prop_minPos_correct :: SourcePos -> SourcePos -> Property
prop_minPos_correct p1 p2 =
  let minPos = _minPos p1 p2
  in (minPos == p1 && p1 <= p2) || (minPos == p2 && p2 <= p1)

-- Property: _maxPos returns the larger position
prop_maxPos_correct :: SourcePos -> SourcePos -> Property
prop_maxPos_correct p1 p2 =
  let maxPos = _maxPos p1 p2
  in (maxPos == p1 && p1 >= p2) || (maxPos == p2 && p2 >= p1)

-- Property: _spanCovering creates span that contains both positions
prop_spanCovering_contains :: SourcePos -> SourcePos -> Property
prop_spanCovering_contains p1 p2 =
  let span = _spanCovering p1 p2
  in _spanContains span p1 .&&. _spanContains span p2

-- Property: _posDistance is non-negative
prop_posDistance_nonnegative :: SourcePos -> SourcePos -> Property
prop_posDistance_nonnegative p1 p2 =
  let distance = _posDistance p1 p2
  in distance >= 0

-- Property: _posDistance is zero for same position
prop_posDistance_zero_same :: SourcePos -> Property
prop_posDistance_zero_same pos =
  _posDistance pos pos === 0

-- Property: _posDistance is symmetric
prop_posDistance_symmetric :: SourcePos -> SourcePos -> Property
prop_posDistance_symmetric p1 p2 =
  _posDistance p1 p2 === _posDistance p2 p1

-- Property: _lineDistance is non-negative
prop_lineDistance_nonnegative :: SourcePos -> SourcePos -> Property
prop_lineDistance_nonnegative p1 p2 =
  let distance = _lineDistance p1 p2
  in distance >= 0

-- Property: _spanLength is non-negative for valid spans
prop_spanLength_nonnegative :: SourceSpan -> Property
prop_spanLength_nonnegative span =
  isValidSpan span ==> _spanLength span >= 0

-- Property: _spanLength is zero for empty spans
prop_spanLength_zero_empty :: SourcePos -> Property
prop_spanLength_zero_empty pos =
  let span = emptySpan pos
  in _spanLength span === 0

-- Property: _isPosInSpan correctly identifies containment
prop_isPosInSpan_correct :: SourceSpan -> SourcePos -> Property
prop_isPosInSpan_correct span pos =
  let start = spanStart span
      end = spanEnd span
      expected = pos >= start && pos <= end
  in _isPosInSpan pos span === expected

-- Property: _spansOverlap is symmetric
prop_spansOverlap_symmetric :: SourceSpan -> SourceSpan -> Property
prop_spansOverlap_symmetric span1 span2 =
  _spansOverlap span1 span2 === _spansOverlap span2 span1

-- Property: _spansOverlap correctly identifies overlapping spans
prop_spansOverlap_correct :: SourceSpan -> SourceSpan -> Property
prop_spansOverlap_correct span1 span2 =
  let start1 = spanStart span1
      end1 = spanEnd span1
      start2 = spanStart span2
      end2 = spanEnd span2
      expected = start1 <= end2 && end1 >= start2
  in _spansOverlap span1 span2 === expected

-- ============================================================================
-- Special Position Properties
-- ============================================================================

-- Property: _posAtLine creates position with column 1
prop_posAtLine_column_one :: Positive Int -> Property
prop_posAtLine_column_one (Positive line) =
  let pos = _posAtLine line
  in posLine pos === line .&&. posColumn pos === 1

-- Property: _posAtLineEnd creates position with large column
prop_posAtLineEnd_large_column :: Positive Int -> Property
prop_posAtLineEnd_large_column (Positive line) =
  let pos = _posAtLineEnd line
  in posLine pos === line .&&. posColumn pos === 100000

-- ============================================================================
-- Text and Range Properties
-- ============================================================================

-- Property: _spanToRangeDesc produces non-empty string
prop_spanToRangeDesc_nonempty :: SourceSpan -> Property
prop_spanToRangeDesc_nonempty span =
  let desc = _spanToRangeDesc span
  in not (null desc)

-- Property: _spanToRangeDesc contains line information
prop_spanToRangeDesc_contains_line :: SourceSpan -> Property
prop_spanToRangeDesc_contains_line span =
  let desc = _spanToRangeDesc span
      startLine = show (posLine (spanStart span))
  in startLine `isInfixOf` desc

-- Test collection
tests :: TestTree
tests = testGroup "SourceLocation Math Properties"
  [ testGroup "Source Position"
    [ fastProperty "startPos has correct values" prop_startPos_values
    , fastProperty "posAfter newline increments line" prop_posAfter_newline
    , fastProperty "posAfter tab advances to tab stop" prop_posAfter_tab
    , fastProperty "posAfter regular char increments" prop_posAfter_regular
    , fastProperty "posAt creates correct position" prop_posAt_correct
    , fastProperty "posAtLineCol creates correct position" prop_posAtLineCol_correct
    ]
  , testGroup "Source Span"
    [ fastProperty "emptySpan has same start and end" prop_emptySpan_same_start_end
    , fastProperty "spanFrom creates empty span" prop_spanFrom_empty
    , fastProperty "spanTo creates empty span" prop_spanTo_empty
    , fastProperty "spanBetween creates correct span" prop_spanBetween_correct
    , fastProperty "mergeSpans covers both spans" prop_mergeSpans_covers
    , fastProperty "mergeSpans is idempotent" prop_mergeSpans_idempotent
    , fastProperty "mergeSpans is commutative" prop_mergeSpans_commutative
    , fastProperty "isValidSpan is correct" prop_isValidSpan_correct
    ]
  , testGroup "Located Values"
    [ fastProperty "locatedAt creates correct located value" prop_locatedAt_correct
    , fastProperty "locatedWithSpan creates correct located value" prop_locatedWithSpan_correct
    , fastProperty "mapLocated preserves location" prop_mapLocated_preserves_location
    ]
  , testGroup "Position Advancement"
    [ fastProperty "advancePos equals posAfter" prop_advancePos_equals_posAfter
    , fastProperty "advancePosBy empty string" prop_advancePosBy_empty
    , fastProperty "advancePosBy is consistent" prop_advancePosBy_consistent
    , fastProperty "advancePosByText is consistent" prop_advancePosByText_consistent
    , fastProperty "advancePosByLine is correct" prop_advancePosByLine_correct
    ]
  , testGroup "Error Location"
    [ fastProperty "toErrorLocation creates correct location" prop_toErrorLocation_correct
    , fastProperty "toErrorLocationWithSpan creates correct location" prop_toErrorLocationWithSpan_correct
    ]
  , testGroup "Position and Span Math"
    [ fastProperty "_minPos returns smaller position" prop_minPos_correct
    , fastProperty "_maxPos returns larger position" prop_maxPos_correct
    , fastProperty "_spanCovering contains both positions" prop_spanCovering_contains
    , fastProperty "_posDistance is non-negative" prop_posDistance_nonnegative
    , fastProperty "_posDistance zero for same position" prop_posDistance_zero_same
    , fastProperty "_posDistance is symmetric" prop_posDistance_symmetric
    , fastProperty "_lineDistance is non-negative" prop_lineDistance_nonnegative
    , fastProperty "_spanLength is non-negative" prop_spanLength_nonnegative
    , fastProperty "_spanLength zero for empty spans" prop_spanLength_zero_empty
    , fastProperty "_isPosInSpan is correct" prop_isPosInSpan_correct
    , fastProperty "_spansOverlap is symmetric" prop_spansOverlap_symmetric
    , fastProperty "_spansOverlap is correct" prop_spansOverlap_correct
    ]
  , testGroup "Special Positions"
    [ fastProperty "_posAtLine creates position with column 1" prop_posAtLine_column_one
    , fastProperty "_posAtLineEnd creates position with large column" prop_posAtLineEnd_large_column
    ]
  , testGroup "Text and Range"
    [ fastProperty "_spanToRangeDesc produces non-empty string" prop_spanToRangeDesc_nonempty
    , fastProperty "_spanToRangeDesc contains line information" prop_spanToRangeDesc_contains_line
    ]
  ]