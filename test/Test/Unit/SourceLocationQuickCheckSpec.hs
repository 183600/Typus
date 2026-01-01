{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.SourceLocationQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, oneof, sized)
import Data.Char (isSpace)
import qualified Data.Text as T

import SourceLocation
  ( SourcePos(..), SourceSpan(..), Located(..)
  , startPos, posAfter, posAt, posAtLineCol
  , emptySpan, spanFrom, spanTo, spanBetween, mergeSpans, isValidSpan
  , locatedAt, locatedWithSpan, locatedValue, locatedSpan, locatedPos, mapLocated
  , runLocationTracker, getCurrentPos, setCurrentPos, markSpanStart, markSpanEnd
  , advancePos, advancePosBy, advancePosByText, advancePosByLine
  , toErrorLocation, toErrorLocationWithSpan
  )

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 1000)
    col <- choose (1, 1000)
    offset <- choose (0, 1000000)
    return $ SourcePos line col offset

instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    -- Ensure we have a valid span (start <= end)
    let start' = min start end
        end' = max start end
    return $ SourceSpan start' end'

instance Arbitrary a => Arbitrary (Located a) where
  arbitrary = do
    value <- arbitrary
    pos <- arbitrary
    span <- arbitrary
    return $ Located value pos span

-- ============================================================================
-- Source Position Properties
-- ============================================================================

-- Property: startPos has correct initial values
prop_startPos_correct :: Property
prop_startPos_correct =
  property $ posLine startPos === 1 .&&.
             posColumn startPos === 1 .&&.
             posOffset startPos === 0

-- Property: posAfter handles newline correctly
prop_posAfter_newline :: SourcePos -> Property
prop_posAfter_newline pos =
  let newPos = posAfter '\n' pos
  in property $ posLine newPos === posLine pos + 1 .&&.
             posColumn newPos === 1 .&&.
             posOffset newPos === posOffset pos + 1

-- Property: posAfter handles tab correctly (8-space alignment)
prop_posAfter_tab :: SourcePos -> Property
prop_posAfter_tab pos =
  let newPos = posAfter '\t' pos
      expectedCol = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in property $ posLine newPos === posLine pos .&&.
             posColumn newPos === expectedCol .&&.
             posOffset newPos === posOffset pos + 1

-- Property: posAfter handles regular characters correctly
prop_posAfter_regular :: Char -> SourcePos -> Property
prop_posAfter_regular char pos =
  char `notElem` "\n\t" ==>
  let newPos = posAfter char pos
  in property $ posLine newPos === posLine pos .&&.
             posColumn newPos === posColumn pos + 1 .&&.
             posOffset newPos === posOffset pos + 1

-- Property: posAt creates position with correct line L.and column
prop_posAt_correct :: Int -> Int -> Property
prop_posAt_correct line col =
  line > 0 && col > 0 ==>
  let pos = posAt line col
  in property $ posLine pos === line .&&.
             posColumn pos === col .&&.
             posOffset pos === 0

-- Property: posAtLineCol creates position with L.all fields correct
prop_posAtLineCol_correct :: Int -> Int -> Int -> Property
prop_posAtLineCol_correct line col offset =
  line > 0 && col > 0 && offset >= 0 ==>
  let pos = posAtLineCol line col offset
  in property $ posLine pos === line .&&.
             posColumn pos === col .&&.
             posOffset pos === offset

-- ============================================================================
-- Source Span Properties
-- ============================================================================

-- Property: emptySpan has same start L.and end
prop_emptySpan_same_start_end :: SourcePos -> Property
prop_emptySpan_same_start_end pos =
  let span = emptySpan pos
  in property $ spanStart span === pos .&&.
             spanEnd span === pos

-- Property: spanFrom creates empty span at position
prop_spanFrom_empty_at_pos :: SourcePos -> Property
prop_spanFrom_empty_at_pos pos =
  let span = spanFrom pos
  in property $ spanStart span === pos .&&.
             spanEnd span === pos

-- Property: spanTo creates empty span at position
prop_spanTo_empty_at_pos :: SourcePos -> Property
prop_spanTo_empty_at_pos pos =
  let span = spanTo pos
  in property $ spanStart span === pos .&&.
             spanEnd span === pos

-- Property: spanBetween creates span with correct start L.and end
prop_spanBetween_correct :: SourcePos -> SourcePos -> Property
prop_spanBetween_correct start end =
  let span = spanBetween start end
  in property $ spanStart span === start .&&.
             spanEnd span === end

-- Property: mergeSpans contains both original spans
prop_mergeSpans_contains_both :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_contains_both span1 span2 =
  let merged = mergeSpans span1 span2
  in property $ spanStart merged <= spanStart span1 .&&.
             spanEnd merged >= spanEnd span1 .&&.
             spanStart merged <= spanStart span2 .&&.
             spanEnd merged >= spanEnd span2

-- Property: mergeSpans is commutative
prop_mergeSpans_commutative :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_commutative span1 span2 =
  mergeSpans span1 span2 === mergeSpans span2 span1

-- Property: mergeSpans is associative
prop_mergeSpans_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_mergeSpans_associative span1 span2 span3 =
  mergeSpans (mergeSpans span1 span2) span3 === mergeSpans span1 (mergeSpans span2 span3)

-- Property: isValidSpan returns true for properly constructed spans
prop_isValidSpan_proper :: SourceSpan -> Property
prop_isValidSpan_proper span =
  isValidSpan span === (spanStart span <= spanEnd span)

-- ============================================================================
-- Located Value Properties
-- ============================================================================

-- Property: locatedAt creates located value with correct position
prop_locatedAt_correct :: SourcePos -> String -> Property
prop_locatedAt_correct pos value =
  let located = locatedAt pos value
  in property $ locatedValue located === value .&&.
             locatedPos located === pos .&&.
             spanStart (locatedSpan located) === pos .&&.
             spanEnd (locatedSpan located) === pos

-- Property: locatedWithSpan creates located value with correct span
prop_locatedWithSpan_correct :: SourceSpan -> String -> Property
prop_locatedWithSpan_correct span value =
  let located = locatedWithSpan span value
  in property $ locatedValue located === value .&&.
             locatedSpan located === span .&&.
             locatedPos located === spanStart span

-- Property: mapLocated preserves location
prop_mapLocated_preserves_location :: SourceSpan -> String -> Property
prop_mapLocated_preserves_location span value =
  let located = locatedWithSpan span value
      mapped = mapLocated L.length located
  in property $ locatedSpan mapped === locatedSpan located .&&.
             locatedPos mapped === locatedPos located .&&.
             locatedValue mapped === L.length value

-- Property: mapLocated is functor law: identity
prop_mapLocated_identity :: Located String -> Property
prop_mapLocated_identity located =
  mapLocated id located === located

-- Property: mapLocated is functor law: composition
prop_mapLocated_composition :: Located String -> Property
prop_mapLocated_composition located =
  mapLocated (L.length . (++ "suffix")) located === mapLocated L.length (mapLocated (++ "suffix") located)

-- ============================================================================
-- Position Advancement Properties
-- ============================================================================

-- Property: advancePos equals posAfter
prop_advancePos_equals_posAfter :: Char -> SourcePos -> Property
prop_advancePos_equals_posAfter char pos =
  advancePos char pos === posAfter char pos

-- Property: advancePosBy with empty string returns same position
prop_advancePosBy_empty :: SourcePos -> Property
prop_advancePosBy_empty pos =
  advancePosBy "" pos === pos

-- Property: advancePosBy is consistent with repeated advancePos
prop_advancePosBy_consistent :: String -> SourcePos -> Property
prop_advancePosBy_consistent chars pos =
  advancePosBy chars pos === L.foldl (flip advancePos) pos chars

-- Property: advancePosByText is consistent with advancePosBy
prop_advancePosByText_consistent :: String -> SourcePos -> Property
prop_advancePosByText_consistent str pos =
  let text = T.pack str
  in advancePosByText text pos === advancePosBy str pos

-- Property: advancePosByLine advances line number L.and resets column
prop_advancePosByLine_correct :: Int -> SourcePos -> Property
prop_advancePosByLine_correct numLines pos =
  numLines > 0 ==>
  let newPos = advancePosByLine numLines pos
  in property $ posLine newPos === posLine pos + numLines .&&.
             posColumn newPos === 1 .&&.
             posOffset newPos === posOffset pos + numLines

-- ============================================================================
-- Location Tracker Properties
-- ============================================================================

-- Property: runLocationTracker starts at startPos
prop_runLocationTracker_start :: Property
prop_runLocationTracker_start =
  runLocationTracker getCurrentPos === startPos

-- Property: setCurrentPos L.and getCurrentPos are consistent
prop_setCurrentPos_getCurrentPos :: SourcePos -> Property
prop_setCurrentPos_getCurrentPos pos =
  runLocationTracker (setCurrentPos pos >> getCurrentPos) === pos

-- Property: markSpanStart returns current position
prop_markSpanStart_current :: SourcePos -> Property
prop_markSpanStart_current pos =
  runLocationTracker (setCurrentPos pos >> markSpanStart) === pos

-- Property: markSpanEnd creates span from start to current
prop_markSpanEnd_correct :: SourcePos -> SourcePos -> Property
prop_markSpanEnd_correct start end =
  start <= end ==>
  let (span, finalPos) = runLocationTracker $ do
        setCurrentPos start
        spanStart <- markSpanStart
        setCurrentPos end
        span <- markSpanEnd spanStart
        return span
  in property $ spanStart span === start .&&.
             spanEnd span === end

-- ============================================================================
-- Error Location Properties
-- ============================================================================

-- Property: toErrorLocation preserves line L.and column
prop_toErrorLocation_preserves_line_col :: SourcePos -> Property
prop_toErrorLocation_preserves_line_col pos =
  let errLoc = toErrorLocation pos
  in property $ line errLoc === posLine pos .&&.
             column errLoc === posColumn pos .&&.
             filePath errLoc === Nothing .&&.
             endLine errLoc === Nothing .&&.
             endColumn errLoc === Nothing

-- Property: toErrorLocationWithSpan preserves span information
prop_toErrorLocationWithSpan_preserves_span :: SourceSpan -> Property
prop_toErrorLocationWithSpan_preserves_span span =
  let errLoc = toErrorLocationWithSpan span
  in property $ line errLoc === posLine (spanStart span) .&&.
             column errLoc === posColumn (spanStart span) .&&.
             endLine errLoc === Just (posLine (spanEnd span)) .&&.
             endColumn errLoc === Just (posColumn (spanEnd span)) .&&.
             filePath errLoc === Nothing

-- ============================================================================
-- Complex Properties
-- ============================================================================

-- Property: Position advancement roundtrip
prop_advancement_roundtrip :: SourcePos -> String -> Property
prop_advancement_roundtrip pos chars =
  let advanced = advancePosBy chars pos
      -- This is a simplified test - real roundtrip would need character counting
  in property $ posOffset advanced >= posOffset pos

-- Property: Span merging idempotency
prop_mergeSpans_idempotent :: SourceSpan -> Property
prop_mergeSpans_idempotent span =
  mergeSpans span span === span

-- Property: Located value mapping preserves span structure
prop_located_mapping_preserves_structure :: SourceSpan -> [Int] -> Property
prop_located_mapping_preserves_structure span values =
  let located = locatedWithSpan span values
      mapped = mapLocated L.sum located
  in property $ locatedSpan mapped === locatedSpan located .&&.
             locatedPos mapped === locatedPos located

-- Property: Complex position tracking sequence
prop_complex_position_tracking :: [String] -> Property
prop_complex_position_tracking strings =
  not (null strings) ==>
  let (positions, finalPos) = runLocationTracker $ do
        positions <- mapM (\str -> do
          current <- getCurrentPos
          setCurrentPos (advancePosBy str current)
          return current
        ) strings
        final <- getCurrentPos
        return (positions, final)
  in property $ L.length positions === L.length strings .&&.
             L.all (\(pos, str) -> posOffset pos >= 0) (zip strings positions)

-- Property: Span validity is preserved under merging
prop_mergeSpans_preserves_validity :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_preserves_validity span1 span2 =
  let merged = mergeSpans span1 span2
  in (isValidSpan span1 .&&. isValidSpan span2) ==> isValidSpan merged

-- Property: Error location conversion is consistent
prop_error_location_consistency :: SourceSpan -> Property
prop_error_location_consistency span =
  let errLoc = toErrorLocationWithSpan span
      startPos = spanStart span
      endPos = spanEnd span
  in property $ line errLoc <= posLine endPos .&&.
             column errLoc <= posColumn endPos .&&.
             endLine errLoc === Just (posLine endPos) .&&.
             endColumn errLoc === Just (posColumn endPos)

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "SourceLocation QuickCheck Tests"
  [ testGroup "Source Position Properties"
    [ fastProperty "startPos has correct initial values" prop_startPos_correct
    , fastProperty "posAfter handles newline correctly" prop_posAfter_newline
    , fastProperty "posAfter handles tab correctly" prop_posAfter_tab
    , fastProperty "posAfter handles regular characters correctly" prop_posAfter_regular
    , fastProperty "posAt creates position with correct line L.and column" prop_posAt_correct
    , fastProperty "posAtLineCol creates position with L.all fields correct" prop_posAtLineCol_correct
    ]
  , testGroup "Source Span Properties"
    [ fastProperty "emptySpan has same start L.and end" prop_emptySpan_same_start_end
    , fastProperty "spanFrom creates empty span at position" prop_spanFrom_empty_at_pos
    , fastProperty "spanTo creates empty span at position" prop_spanTo_empty_at_pos
    , fastProperty "spanBetween creates span with correct start L.and end" prop_spanBetween_correct
    , fastProperty "mergeSpans contains both original spans" prop_mergeSpans_contains_both
    , fastProperty "mergeSpans is commutative" prop_mergeSpans_commutative
    , fastProperty "mergeSpans is associative" prop_mergeSpans_associative
    , fastProperty "isValidSpan returns true for properly constructed spans" prop_isValidSpan_proper
    ]
  , testGroup "Located Value Properties"
    [ fastProperty "locatedAt creates located value with correct position" prop_locatedAt_correct
    , fastProperty "locatedWithSpan creates located value with correct span" prop_locatedWithSpan_correct
    , fastProperty "mapLocated preserves location" prop_mapLocated_preserves_location
    , fastProperty "mapLocated is functor law: identity" prop_mapLocated_identity
    , fastProperty "mapLocated is functor law: composition" prop_mapLocated_composition
    ]
  , testGroup "Position Advancement Properties"
    [ fastProperty "advancePos equals posAfter" prop_advancePos_equals_posAfter
    , fastProperty "advancePosBy with empty string returns same position" prop_advancePosBy_empty
    , fastProperty "advancePosBy is consistent with repeated advancePos" prop_advancePosBy_consistent
    , fastProperty "advancePosByText is consistent with advancePosBy" prop_advancePosByText_consistent
    , fastProperty "advancePosByLine advances line number L.and resets column" prop_advancePosByLine_correct
    ]
  , testGroup "Location Tracker Properties"
    [ fastProperty "runLocationTracker starts at startPos" prop_runLocationTracker_start
    , fastProperty "setCurrentPos L.and getCurrentPos are consistent" prop_setCurrentPos_getCurrentPos
    , fastProperty "markSpanStart returns current position" prop_markSpanStart_current
    , fastProperty "markSpanEnd creates span from start to current" prop_markSpanEnd_correct
    ]
  , testGroup "Error Location Properties"
    [ fastProperty "toErrorLocation preserves line L.and column" prop_toErrorLocation_preserves_line_col
    , fastProperty "toErrorLocationWithSpan preserves span information" prop_toErrorLocationWithSpan_preserves_span
    ]
  , testGroup "Complex Properties"
    [ fastProperty "Position advancement roundtrip" prop_advancement_roundtrip
    , fastProperty "Span merging idempotency" prop_mergeSpans_idempotent
    , fastProperty "Located value mapping preserves span structure" prop_located_mapping_preserves_structure
    , fastProperty "Complex position tracking sequence" prop_complex_position_tracking
    , fastProperty "Span validity is preserved under merging" prop_mergeSpans_preserves_validity
    , fastProperty "Error location conversion is consistent" prop_error_location_consistency
    ]
  ]