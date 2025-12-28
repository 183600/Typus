{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewSourceLocationPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, suchThat
  , choose, frequency, sized, resize, Positive(..), NonEmpty(..)
  )

import SourceLocation
  ( SourcePos(..), SourceSpan(..), Located(..), HasLocation(..)
  , startPos, posAfter, posAt, posAtLineCol
  , emptySpan, spanFrom, spanTo, spanBetween, mergeSpans, isValidSpan
  , locatedAt, locatedWithSpan, locatedValue, locatedSpan, locatedPos, mapLocated
  , runLocationTracker, getCurrentPos, setCurrentPos, markSpanStart, markSpanEnd
  , withLocationTracking, advancePos, advancePosBy, advancePosByText, advancePosByLine
  , toErrorLocation, toErrorLocationWithSpan
  )

import Compiler.Errors.Core (ErrorLocation(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sort)

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
    end <- arbitrary
    -- Ensure we get valid spans most of the time
    if start <= end
    then return $ SourceSpan start end
    else return $ SourceSpan end start

instance Arbitrary a => Arbitrary (Located a) where
  arbitrary = do
    value <- arbitrary
    start <- arbitrary
    end <- arbitrary
    let spanStartPos = if start <= end then start else end
        spanEndPos = if start <= end then end else start
    return $ Located value spanStartPos (SourceSpan spanStartPos spanEndPos)

-- ============================================================================
-- Source Position Properties
-- ============================================================================

-- Property: startPos has correct initial values
prop_startPos_values :: Property
prop_startPos_values =
  posLine startPos === 1 .&&.
  posColumn startPos === 1 .&&.
  posOffset startPos === 0

-- Property: posAfter with newline increments line and resets column
prop_posAfter_newline :: SourcePos -> Property
prop_posAfter_newline pos =
  let newPos = posAfter '\n' pos
  in posLine newPos === posLine pos + 1 .&&.
     posColumn newPos === 1 .&&.
     posOffset newPos === posOffset pos + 1

-- Property: posAfter with tab advances to next tab stop
prop_posAfter_tab :: SourcePos -> Property
prop_posAfter_tab pos =
  let newPos = posAfter '\t' pos
      expectedCol = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in posColumn newPos === expectedCol .&&.
     posOffset newPos === posOffset pos + 1

-- Property: posAfter with regular character increments column and offset
prop_posAfter_regular :: SourcePos -> Char -> Property
prop_posAfter_regular pos c =
  (c /= '\n' && c /= '\t') ==> 
  let newPos = posAfter c pos
  in posColumn newPos === posColumn pos + 1 .&&.
     posOffset newPos === posOffset pos + 1

-- Property: posAt creates position with correct line and column
prop_posAt_creates_correct :: Int -> Int -> Property
prop_posAt_creates_correct line col =
  line > 0 && col > 0 ==>
  let pos = posAt line col
  in posLine pos === line .&&.
     posColumn pos === col .&&.
     posOffset pos === 0

-- Property: posAtLineCol creates position with all fields set
prop_posAtLineCol_creates_correct :: Int -> Int -> Int -> Property
prop_posAtLineCol_creates_correct line col offset =
  line > 0 && col > 0 && offset >= 0 ==>
  let pos = posAtLineCol line col offset
  in posLine pos === line .&&.
     posColumn pos === col .&&.
     posOffset pos === offset

-- ============================================================================
-- Source Span Properties
-- ============================================================================

-- Property: emptySpan creates span with same start and end
prop_emptySpan_same_start_end :: SourcePos -> Property
prop_emptySpan_same_start_end pos =
  let span = emptySpan pos
  in spanStart span === pos .&&.
     spanEnd span === pos

-- Property: spanFrom creates empty span at position
prop_spanFrom_equals_emptySpan :: SourcePos -> Property
prop_spanFrom_equals_emptySpan pos = spanFrom pos === emptySpan pos

-- Property: spanTo creates empty span at position
prop_spanTo_equals_emptySpan :: SourcePos -> Property
prop_spanTo_equals_emptySpan pos = spanTo pos === emptySpan pos

-- Property: spanBetween creates span with correct start and end
prop_spanBetween_correct_start_end :: SourcePos -> SourcePos -> Property
prop_spanBetween_correct_start_end start end =
  let span = spanBetween start end
  in spanStart span === start .&&.
     spanEnd span === end

-- Property: mergeSpans creates span covering both spans
prop_mergeSpans_covers_both :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_covers_both span1 span2 =
  let merged = mergeSpans span1 span2
  in spanStart merged === min (spanStart span1) (spanStart span2) .&&.
     spanEnd merged === max (spanEnd span1) (spanEnd span2)

-- Property: isValidSpan checks start <= end
prop_isValidSpan_correct :: SourcePos -> SourcePos -> Property
prop_isValidSpan_correct start end =
  let span = SourceSpan start end
  in isValidSpan span === (start <= end)

-- ============================================================================
-- Located Value Properties
-- ============================================================================

-- Property: locatedAt creates located value at position
prop_locatedAt_correct_position :: SourcePos -> Int -> Property
prop_locatedAt_correct_position pos value =
  let located = locatedAt pos value
  in locatedPos located === pos .&&.
     locatedValue located === value .&&.
     spanStart (locatedSpan located) === pos .&&.
     spanEnd (locatedSpan located) === pos

-- Property: locatedWithSpan creates located value with span
prop_locatedWithSpan_correct_span :: SourceSpan -> String -> Property
prop_locatedWithSpan_correct_span span value =
  let located = locatedWithSpan span value
  in locatedSpan located === span .&&.
     locatedValue located === value .&&.
     locPos located === spanStart span

-- Property: locatedValue returns the wrapped value
prop_locatedValue_returns_value :: Int -> SourcePos -> Property
prop_locatedValue_returns_value value pos =
  let located = locatedAt pos value
  in locatedValue located === value

-- Property: mapLocated applies function to wrapped value
prop_mapLocated_applies_function :: Int -> SourcePos -> Property
prop_mapLocated_applies_function value pos =
  let located = locatedAt pos value
      mapped = mapLocated (*2) located
  in locatedValue mapped === value * 2

-- ============================================================================
-- Position Advancement Properties
-- ============================================================================

-- Property: advancePos equals posAfter
prop_advancePos_equals_posAfter :: SourcePos -> Char -> Property
prop_advancePos_equals_posAfter pos c = advancePos c pos === posAfter c pos

-- Property: advancePosBy with empty string returns same position
prop_advancePosBy_empty :: SourcePos -> Property
prop_advancePosBy_empty pos = advancePosBy "" pos === pos

-- Property: advancePosBy with multiple chars equals sequential advancePos
prop_advancePosBy_sequential :: SourcePos -> String -> Property
prop_advancePosBy_sequential pos chars =
  advancePosBy chars pos === foldl (flip advancePos) pos chars

-- Property: advancePosByText equals advancePosBy with unpacked text
prop_advancePosByText_equals_advancePosBy :: SourcePos -> Text -> Property
prop_advancePosByText_equals_advancePosBy pos text =
  advancePosByText text pos === advancePosBy (T.unpack text) pos

-- Property: advancePosByLine increments line and resets column
prop_advancePosByLine_increments_line :: SourcePos -> Int -> Property
prop_advancePosByLine_increments_line pos n =
  n > 0 ==>
  let newPos = advancePosByLine n pos
  in posLine newPos === posLine pos + n .&&.
     posColumn newPos === 1

-- ============================================================================
-- Location Tracker Properties
-- ============================================================================

-- Property: runLocationTracker starts at startPos
prop_runLocationTracker_starts_at_startPos :: Property
prop_runLocationTracker_starts_at_startPos =
  runLocationTracker getCurrentPos === startPos

-- Property: setCurrentPos changes current position
prop_setCurrentPos_changes_position :: SourcePos -> SourcePos -> Property
prop_setCurrentPos_changes_position initial new =
  withLocationTracking initial (do
    setCurrentPos new
    getCurrentPos
  ) === ((), new)

-- Property: markSpanStart returns current position
prop_markSpanStart_returns_current :: SourcePos -> Property
prop_markSpanStart_returns_current pos =
  withLocationTracking pos (do
    start <- markSpanStart
    return start
  ) === ((), pos)

-- ============================================================================
-- Error Location Properties
-- ============================================================================

-- Property: toErrorLocation creates correct ErrorLocation from position
prop_toErrorLocation_correct :: SourcePos -> Property
prop_toErrorLocation_correct pos =
  let errLoc = toErrorLocation pos
  in line errLoc === posLine pos .&&.
     column errLoc === posColumn pos .&&.
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
     endColumn errLoc === Just (posColumn end)

-- ============================================================================
-- Combined Properties
-- ============================================================================

-- Property: locatedAt and locatedWithSpan are consistent
prop_locatedAt_locatedWithSpan_consistent :: SourcePos -> Int -> Property
prop_locatedAt_locatedWithSpan_consistent pos value =
  let atLocated = locatedAt pos value
      withLocated = locatedWithSpan (emptySpan pos) value
  in atLocated === withLocated

-- Property: mergeSpans is commutative
prop_mergeSpans_commutative :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_commutative span1 span2 =
  mergeSpans span1 span2 === mergeSpans span2 span1

-- Property: mergeSpans is associative
prop_mergeSpans_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_mergeSpans_associative span1 span2 span3 =
  mergeSpans span1 (mergeSpans span2 span3) === mergeSpans (mergeSpans span1 span2) span3

-- Property: isValidSpan after mergeSpans is always true
prop_mergeSpans_valid_result :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_valid_result span1 span2 =
  let merged = mergeSpans span1 span2
  in isValidSpan merged === True

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New SourceLocation Properties"
  [ fastProperty "startPos values" prop_startPos_values
  , fastProperty "posAfter newline" prop_posAfter_newline
  , fastProperty "posAfter tab" prop_posAfter_tab
  , fastProperty "posAfter regular char" prop_posAfter_regular
  , fastProperty "posAt creates correct" prop_posAt_creates_correct
  , fastProperty "posAtLineCol creates correct" prop_posAtLineCol_creates_correct
  , fastProperty "emptySpan same start end" prop_emptySpan_same_start_end
  , fastProperty "spanFrom equals emptySpan" prop_spanFrom_equals_emptySpan
  , fastProperty "spanTo equals emptySpan" prop_spanTo_equals_emptySpan
  , fastProperty "spanBetween correct start end" prop_spanBetween_correct_start_end
  , fastProperty "mergeSpans covers both" prop_mergeSpans_covers_both
  , fastProperty "isValidSpan correct" prop_isValidSpan_correct
  , fastProperty "locatedAt correct position" prop_locatedAt_correct_position
  , fastProperty "locatedWithSpan correct span" prop_locatedWithSpan_correct_span
  , fastProperty "locatedValue returns value" prop_locatedValue_returns_value
  , fastProperty "mapLocated applies function" prop_mapLocated_applies_function
  , fastProperty "advancePos equals posAfter" prop_advancePos_equals_posAfter
  , fastProperty "advancePosBy empty" prop_advancePosBy_empty
  , fastProperty "advancePosBy sequential" prop_advancePosBy_sequential
  , fastProperty "advancePosByText equals advancePosBy" prop_advancePosByText_equals_advancePosBy
  , fastProperty "advancePosByLine increments line" prop_advancePosByLine_increments_line
  , fastProperty "runLocationTracker starts at startPos" prop_runLocationTracker_starts_at_startPos
  , fastProperty "setCurrentPos changes position" prop_setCurrentPos_changes_position
  , fastProperty "markSpanStart returns current" prop_markSpanStart_returns_current
  , fastProperty "toErrorLocation correct" prop_toErrorLocation_correct
  , fastProperty "toErrorLocationWithSpan correct" prop_toErrorLocationWithSpan_correct
  , fastProperty "locatedAt locatedWithSpan consistent" prop_locatedAt_locatedWithSpan_consistent
  , fastProperty "mergeSpans commutative" prop_mergeSpans_commutative
  , fastProperty "mergeSpans associative" prop_mergeSpans_associative
  , fastProperty "mergeSpans valid result" prop_mergeSpans_valid_result

  , testCase "startPos specific values" $ do
      posLine startPos @?= 1
      posColumn startPos @?= 1
      posOffset startPos @?= 0
      
  , testCase "posAfter specific characters" $ do
      let pos = SourcePos 5 10 20
      posAfter '\n' pos @?= SourcePos 6 1 21
      posAfter '\t' pos @?= SourcePos 5 17 21  -- ((10-1) div 8 + 1) * 8 + 1 = 17
      posAfter 'a' pos @?= SourcePos 5 11 21
      
  , testCase "span operations" $ do
      let pos1 = SourcePos 1 1 0
      let pos2 = SourcePos 1 5 4
      let span1 = SourceSpan pos1 pos2
      let span2 = SourceSpan pos2 pos1
      emptySpan pos1 @?= SourceSpan pos1 pos1
      spanBetween pos1 pos2 @?= SourceSpan pos1 pos2
      mergeSpans span1 span2 @?= span1  -- span1 should already cover both
      
  , testCase "located values" $ do
      let pos = SourcePos 3 7 15
      let located = locatedAt pos "test"
      locatedValue located @?= "test"
      locatedPos located @?= pos
      let mapped = mapLocated (++ " modified") located
      locatedValue mapped @?= "test modified"
  ]