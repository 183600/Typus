{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.SourceLocationCoreQuickCheckSpec (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import SourceLocation
import Compiler.Errors.Core (ErrorLocation(..))
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.State (evalState)

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 1000)
    column <- choose (1, 1000)
    offset <- choose (0, 10000)
    return $ SourcePos line column offset

instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    -- Ensure start <= end for valid spans
    let validEnd = if start <= end then end else start
    return $ SourceSpan start validEnd

instance Arbitrary a => Arbitrary (Located a) where
  arbitrary = do
    value <- arbitrary
    pos <- arbitrary
    span <- arbitrary
    return $ Located value pos span

-- ============================================================================
-- Source Position Properties
-- ============================================================================

prop_startPosProperties :: Property
prop_startPosProperties =
  let pos = startPos
  in counterexample "startPos should be (1,1,0)" $
    posLine pos === 1 .&.
    posColumn pos === 1 .&.
    posOffset pos === 0

prop_posAfterRoundtrip :: Char -> SourcePos -> Property
prop_posAfterRoundtrip c pos =
  let newPos = posAfter c pos
  in counterexample ("posAfter should advance position for character: " ++ show c) $
    posOffset newPos === posOffset pos + 1 .&.
    (if c == '\n'
     then posLine newPos === posLine pos + 1 .&. posColumn newPos === 1
     else if c == '\t'
          then posColumn newPos === ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
          else posColumn newPos === posColumn pos + 1)

prop_posAtProperties :: Positive Int -> Positive Int -> Property
prop_posAtProperties (Positive line) (Positive col) =
  let pos = posAt line col
  in counterexample ("posAt should create position at (" ++ show line ++ "," ++ show col ++ ")") $
    posLine pos === line .&.
    posColumn pos === col .&.
    posOffset pos === 0

prop_posAtLineColProperties :: Positive Int -> Positive Int -> Positive Int -> Property
prop_posAtLineColProperties (Positive line) (Positive col) (Positive offset) =
  let pos = posAtLineCol line col offset
  in counterexample ("posAtLineCol should create position at (" ++ show line ++ "," ++ show col ++ "," ++ show offset ++ ")") $
    posLine pos === line .&.
    posColumn pos === col .&.
    posOffset pos === offset

-- ============================================================================
-- Source Span Properties
-- ============================================================================

prop_emptySpanProperties :: SourcePos -> Property
prop_emptySpanProperties pos =
  let span = emptySpan pos
  in counterexample "emptySpan should have same start and end" $
    spanStart span === pos .&.
    spanEnd span === pos .&.
    isValidSpan span === True

prop_spanFromProperties :: SourcePos -> Property
prop_spanFromProperties pos =
  let span = spanFrom pos
  in counterexample "spanFrom should create empty span at position" $
    spanStart span === pos .&.
    spanEnd span === pos

prop_spanToProperties :: SourcePos -> Property
prop_spanToProperties pos =
  let span = spanTo pos
  in counterexample "spanTo should create empty span at position" $
    spanStart span === pos .&.
    spanEnd span === pos

prop_spanBetweenProperties :: SourcePos -> SourcePos -> Property
prop_spanBetweenProperties pos1 pos2 =
  let span = spanBetween pos1 pos2
      expectedStart = min pos1 pos2
      expectedEnd = max pos1 pos2
  in counterexample "spanBetween should create span covering both positions" $
    spanStart span === expectedStart .&.
    spanEnd span === expectedEnd

prop_mergeSpansProperties :: SourceSpan -> SourceSpan -> Property
prop_mergeSpansProperties span1 span2 =
  let merged = mergeSpans span1 span2
  in counterexample "mergeSpans should create span covering both input spans" $
    spanStart merged === min (spanStart span1) (spanStart span2) .&.
    spanEnd merged === max (spanEnd span1) (spanEnd span2) .&.
    isValidSpan merged === True

prop_mergeSpansIdempotent :: SourceSpan -> SourceSpan -> Property
prop_mergeSpansIdempotent span1 span2 =
  let merged1 = mergeSpans span1 span2
      merged2 = mergeSpans merged1 span2
  in counterexample "mergeSpans should be idempotent when merging with contained span" $
    merged1 === merged2

-- ============================================================================
-- Located Values Properties
-- ============================================================================

prop_locatedAtProperties :: SourcePos -> String -> Property
prop_locatedAtProperties pos value =
  let located = locatedAt pos value
  in counterexample "locatedAt should create located value at position" $
    locatedValue located === value .&.
    locatedPos located === pos .&.
    locatedSpan located === emptySpan pos

prop_locatedWithSpanProperties :: SourceSpan -> Int -> Property
prop_locatedWithSpanProperties span value =
  let located = locatedWithSpan span value
  in counterexample "locatedWithSpan should create located value with span" $
    locatedValue located === value .&.
    locatedSpan located === span .&.
    locatedPos located === spanStart span

prop_mapLocatedProperties :: SourceSpan -> String -> Property
prop_mapLocatedProperties span value =
  let located = locatedWithSpan span value
      mapped = mapLocated length located
  in counterexample "mapLocated should apply function to value" $
    locatedValue mapped === length value .&.
    locatedSpan mapped === span

-- ============================================================================
-- Position Advancement Properties
-- ============================================================================

prop_advancePosConsistency :: Char -> SourcePos -> Property
prop_advancePosConsistency c pos =
  let advanced = advancePos c pos
      manualAdvanced = posAfter c pos
  in counterexample "advancePos should be consistent with posAfter" $
    advanced === manualAdvanced

prop_advancePosByTextProperties :: String -> SourcePos -> Property
prop_advancePosByTextProperties text pos =
  let text' = T.pack text
      advanced = advancePosByText text' pos
      manualAdvanced = advancePosBy text pos
  in counterexample "advancePosByText should be consistent with advancePosBy" $
    advanced === manualAdvanced

prop_advancePosByLineProperties :: Positive Int -> SourcePos -> Property
prop_advancePosByLineProperties (Positive numLines) pos =
  let advanced = advancePosByLine numLines pos
  in counterexample "advancePosByLine should advance line number" $
    posLine advanced === posLine pos + numLines .&.
    posColumn advanced === 1

-- ============================================================================
-- Error Location Conversion Properties
-- ============================================================================

prop_toErrorLocationProperties :: SourcePos -> Property
prop_toErrorLocationProperties pos =
  let errLoc = toErrorLocation pos
  in counterexample "toErrorLocation should convert position to error location" $
    line errLoc === posLine pos .&.
    column errLoc === posColumn pos .&.
    filePath errLoc === Nothing .&.
    endLine errLoc === Nothing .&.
    endColumn errLoc === Nothing

prop_toErrorLocationWithSpanProperties :: SourceSpan -> Property
prop_toErrorLocationWithSpanProperties span =
  let errLoc = toErrorLocationWithSpan span
  in counterexample "toErrorLocationWithSpan should convert span to error location with range" $
    line errLoc === posLine (spanStart span) .&.
    column errLoc === posColumn (spanStart span) .&.
    endLine errLoc === Just (posLine (spanEnd span)) .&.
    endColumn errLoc === Just (posColumn (spanEnd span))

-- ============================================================================
-- Location Tracker Monad Properties
-- ============================================================================

prop_locationTrackerStartPos :: Property
prop_locationTrackerStartPos =
  let result = runLocationTracker getCurrentPos
  in counterexample "LocationTracker should start at startPos" $
    result === startPos

prop_locationTrackerSetGet :: SourcePos -> Property
prop_locationTrackerSetGet pos =
  let result = evalState (do
          setCurrentPos pos
          getCurrentPos) startPos
  in counterexample "LocationTracker set/get should be consistent" $
    result === pos

prop_locationTrackerSpanMarking :: SourcePos -> SourcePos -> Property
prop_locationTrackerSpanMarking start end =
  let result = evalState (do
          setCurrentPos start
          spanStart <- markSpanStart
          setCurrentPos end
          markSpanEnd spanStart) startPos
  in counterexample "LocationTracker span marking should work correctly" $
    result === spanBetween start end

-- ============================================================================
-- Utility Function Properties
-- ============================================================================

prop_isValidSpanProperties :: SourcePos -> SourcePos -> Property
prop_isValidSpanProperties pos1 pos2 =
  let span = SourceSpan pos1 pos2
      expected = pos1 <= pos2
  in counterexample "isValidSpan should check start <= end" $
    isValidSpan span === expected

prop_spanLengthProperties :: SourcePos -> Positive Int -> Property
prop_spanLengthProperties start (Positive offset) =
  let end = start { posOffset = posOffset start + offset }
      span = SourceSpan start end
  in counterexample "span length should equal offset difference" $
    _spanLength span === offset

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "SourceLocation Core QuickCheck Tests"
  [ testProperty "startPos has correct initial values" prop_startPosProperties
  , testProperty "posAfter advances position correctly" prop_posAfterRoundtrip
  , testProperty "posAt creates position at correct line/column" prop_posAtProperties
  , testProperty "posAtLineCol creates position at correct line/column/offset" prop_posAtLineColProperties
  , testProperty "emptySpan has same start and end" prop_emptySpanProperties
  , testProperty "spanFrom creates empty span at position" prop_spanFromProperties
  , testProperty "spanTo creates empty span at position" prop_spanToProperties
  , testProperty "spanBetween covers both positions" prop_spanBetweenProperties
  , testProperty "mergeSpans creates span covering both spans" prop_mergeSpansProperties
  , testProperty "mergeSpans is idempotent" prop_mergeSpansIdempotent
  , testProperty "locatedAt creates located value at position" prop_locatedAtProperties
  , testProperty "locatedWithSpan creates located value with span" prop_locatedWithSpanProperties
  , testProperty "mapLocated applies function to value" prop_mapLocatedProperties
  , testProperty "advancePos is consistent with posAfter" prop_advancePosConsistency
  , testProperty "advancePosByText is consistent with advancePosBy" prop_advancePosByTextProperties
  , testProperty "advancePosByLine advances line number" prop_advancePosByLineProperties
  , testProperty "toErrorLocation converts position correctly" prop_toErrorLocationProperties
  , testProperty "toErrorLocationWithSpan converts span correctly" prop_toErrorLocationWithSpanProperties
  , testProperty "LocationTracker starts at startPos" prop_locationTrackerStartPos
  , testProperty "LocationTracker set/get is consistent" prop_locationTrackerSetGet
  , testProperty "LocationTracker span marking works correctly" prop_locationTrackerSpanMarking
  , testProperty "isValidSpan checks start <= end" prop_isValidSpanProperties
  , testProperty "span length equals offset difference" prop_spanLengthProperties
  ]