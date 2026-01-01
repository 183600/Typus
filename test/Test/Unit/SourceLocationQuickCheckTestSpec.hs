{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.SourceLocationQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf1, elements, oneof)

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , HasLocation(..)
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
import Data.Char (isSpace)
import Control.Monad.State (runState)

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

-- Generate valid source positions
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 1000)
    col <- choose (1, 1000)
    offset <- choose (0, 1000000)
    return $ SourcePos line col offset

-- Generate valid source spans
instance Arbitrary SourceSpan where
  arbitrary = do
    startLine <- choose (1, 100)
    startCol <- choose (1, 100)
    startOffset <- choose (0, 10000)
    let start = SourcePos startLine startCol startOffset
    
    -- Ensure end is after start
    endLineDelta <- choose (0, 10)
    endColDelta <- choose (0, 50)
    endOffsetDelta <- choose (0, 1000)
    
    let endLine = if endLineDelta == 0 && endColDelta == 0 then startLine else startLine + endLineDelta
        endCol = if endLineDelta == 0 then startCol + endColDelta else max 1 endColDelta
        endOffset = startOffset + max 0 endOffsetDelta
        end = SourcePos endLine endCol endOffset
    
    return $ SourceSpan start end

-- Generate located values
instance Arbitrary a => Arbitrary (Located a) where
  arbitrary = do
    value <- arbitrary
    span <- arbitrary
    return $ Located value (spanStart span) span

-- ============================================================================
-- Property Tests for SourcePos
-- ============================================================================

-- Property: startPos is always valid
prop_startPos_valid :: Property
prop_startPos_valid =
  let pos = startPos
  in property $ posLine pos >= 1 .&&. posColumn pos >= 1 .&&. posOffset pos >= 0

-- Property: posAfter correctly handles newline
prop_posAfter_newline :: SourcePos -> Property
prop_posAfter_newline pos =
  let newPos = posAfter '\n' pos
  in property $ posLine newPos === posLine pos + 1 .&&.
     posColumn newPos === 1 .&&.
     posOffset newPos === posOffset pos + 1

-- Property: posAfter correctly handles tab
prop_posAfter_tab :: SourcePos -> Property  
prop_posAfter_tab pos =
  let newPos = posAfter '\t' pos
      expectedCol = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in property $ posLine newPos === posLine pos .&&.
     posColumn newPos === expectedCol .&&.
     posOffset newPos === posOffset pos + 1

-- Property: posAfter correctly handles regular characters
prop_posAfter_regular_char :: SourcePos -> Char -> Property
prop_posAfter_regular_char pos char =
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
  in property $ posLine pos === line .&&. posColumn pos === col .&&. posOffset pos === 0

-- Property: posAtLineCol creates position with L.all fields
prop_posAtLineCol_correct :: Int -> Int -> Int -> Property
prop_posAtLineCol_correct line col offset =
  line > 0 && col > 0 && offset >= 0 ==>
  let pos = posAtLineCol line col offset
  in property $ posLine pos === line .&&. posColumn pos === col .&&. posOffset pos === offset

-- ============================================================================
-- Property Tests for SourceSpan
-- ============================================================================

-- Property: emptySpan has same start L.and end
prop_emptySpan_same_start_end :: SourcePos -> Property
prop_emptySpan_same_start_end pos =
  let span = emptySpan pos
  in property $ spanStart span === spanEnd span .&&. spanStart span === pos

-- Property: spanFrom creates empty span
prop_spanFrom_empty :: SourcePos -> Property
prop_spanFrom_empty pos =
  let span = spanFrom pos
  in property $ spanStart span === spanEnd span .&&. spanStart span === pos

-- Property: spanTo creates empty span
prop_spanTo_empty :: SourcePos -> Property
prop_spanTo_empty pos =
  let span = spanTo pos
  in property $ spanStart span === spanEnd span .&&. spanEnd span === pos

-- Property: spanBetween creates correct span
prop_spanBetween_correct :: SourcePos -> SourcePos -> Property
prop_spanBetween_correct start end =
  let span = spanBetween start end
  in property $ spanStart span === start .&&. spanEnd span === end

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

-- Property: isValidSpan correctly validates spans
prop_isValidSpan_correct :: SourceSpan -> Property
prop_isValidSpan_correct span =
  let start = spanStart span
      end = spanEnd span
      expected = start <= end
  in property $ isValidSpan span === expected

-- ============================================================================
-- Property Tests for Located Values
-- ============================================================================

-- Property: locatedAt creates located value at position
prop_locatedAt_correct :: SourcePos -> Int -> Property
prop_locatedAt_correct pos value =
  let located = locatedAt pos value
  in property $ locatedValue located === value .&&.
     locatedPos located === pos .&&.
     spanStart (locatedSpan located) === pos .&&.
     spanEnd (locatedSpan located) === pos

-- Property: locatedWithSpan creates located value with span
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

-- Property: HasLocation instance works correctly
prop_hasLocation_correct :: SourceSpan -> Int -> Property
prop_hasLocation_correct span value =
  let located = locatedWithSpan span value
  in property $ getLocation located === span

-- ============================================================================
-- Property Tests for Position Advancement
-- ============================================================================

-- Property: advancePos equals posAfter
prop_advancePos_equals_posAfter :: SourcePos -> Char -> Property
prop_advancePos_equals_posAfter pos char =
  advancePos char pos === posAfter char pos

-- Property: advancePosBy advances by each character
prop_advancePosBy_sequential :: SourcePos -> String -> Property
prop_advancePosBy_sequential pos chars =
  let finalPos = advancePosBy chars pos
      expectedPos = L.foldl (flip advancePos) pos chars
  in property $ finalPos === expectedPos

-- Property: advancePosByText works with Text
prop_advancePosByText_equals_string :: SourcePos -> String -> Property
prop_advancePosByText_equals_string pos str =
  let text = T.pack str
      textPos = advancePosByText text pos
      stringPos = advancePosBy str pos
  in property $ textPos === stringPos

-- Property: advancePosByLine changes line L.and resets column
prop_advancePosByLine_correct :: SourcePos -> Int -> Property
prop_advancePosByLine_correct pos numLines =
  numLines >= 0 ==>
  let newPos = advancePosByLine numLines pos
  in property $ posLine newPos === posLine pos + numLines .&&.
     posColumn newPos === 1

-- Property: advancePosByLine with zero lines preserves line
prop_advancePosByLine_zero :: SourcePos -> Property
prop_advancePosByLine_zero pos =
  advancePosByLine 0 pos === pos

-- ============================================================================
-- Property Tests for Error Location Conversion
-- ============================================================================

-- Property: toErrorLocation preserves line L.and column
prop_toErrorLocation_preserves_pos :: SourcePos -> Property
prop_toErrorLocation_preserves_pos pos =
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
      start = spanStart span
      end = spanEnd span
  in property $ line errLoc === posLine start .&&.
     column errLoc === posColumn start .&&.
     endLine errLoc === Just (posLine end) .&&.
     endColumn errLoc === Just (posColumn end) .&&.
     filePath errLoc === Nothing

-- ============================================================================
-- Property Tests for Complex Scenarios
-- ============================================================================

-- Property: Position advancement is consistent
prop_position_advancement_consistent :: SourcePos -> String -> Property
prop_position_advancement_consistent pos text =
  let advanced1 = advancePosBy text pos
      advanced2 = L.foldl (flip advancePos) pos text
  in property $ advanced1 === advanced2

-- Property: Span merging preserves containment
prop_span_merging_preserves_containment :: SourceSpan -> SourceSpan -> Property
prop_span_merging_preserves_containment span1 span2 =
  let merged = mergeSpans span1 span2
  in property $ spanStart merged <= spanStart span1 .&&.
     spanEnd merged >= spanEnd span1 .&&.
     spanStart merged <= spanStart span2 .&&.
     spanEnd merged >= spanEnd span2

-- Property: Located value mapping preserves structure
prop_located_mapping_preserves_structure :: SourceSpan -> [Int] -> Property
prop_located_mapping_preserves_structure span values =
  let located = locatedWithSpan span values
      mapped = mapLocated L.sum located
  in property $ locatedSpan mapped === locatedSpan located .&&.
     locatedPos mapped === locatedPos located

-- Property: Position advancement with newlines
prop_position_advancement_newlines :: SourcePos -> Int -> Property
prop_position_advancement_newlines pos numNewlines =
  numNewlines >= 0 && numNewlines <= 100 ==>
  let newlines = replicate numNewlines '\n'
      advanced = advancePosBy newlines pos
  in property $ posLine advanced === posLine pos + numNewlines .&&.
     posColumn advanced === 1

-- Property: Span validity after merging
prop_span_validity_after_merging :: SourceSpan -> SourceSpan -> Property
prop_span_validity_after_merging span1 span2 =
  let merged = mergeSpans span1 span2
  in property $ isValidSpan merged ==> spanStart merged <= spanEnd merged

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "SourceLocation QuickCheck Tests"
  [ testGroup "SourcePos Properties"
    [ fastProperty "startPos is valid" prop_startPos_valid
    , fastProperty "posAfter handles newline" prop_posAfter_newline
    , fastProperty "posAfter handles tab" prop_posAfter_tab
    , fastProperty "posAfter handles regular characters" prop_posAfter_regular_char
    , fastProperty "posAt creates correct position" prop_posAt_correct
    , fastProperty "posAtLineCol creates correct position" prop_posAtLineCol_correct
    ]
  , testGroup "SourceSpan Properties"
    [ fastProperty "emptySpan has same start L.and end" prop_emptySpan_same_start_end
    , fastProperty "spanFrom creates empty span" prop_spanFrom_empty
    , fastProperty "spanTo creates empty span" prop_spanTo_empty
    , fastProperty "spanBetween creates correct span" prop_spanBetween_correct
    , fastProperty "mergeSpans contains both spans" prop_mergeSpans_contains_both
    , fastProperty "mergeSpans is commutative" prop_mergeSpans_commutative
    , fastProperty "mergeSpans is associative" prop_mergeSpans_associative
    , fastProperty "isValidSpan validates correctly" prop_isValidSpan_correct
    ]
  , testGroup "Located Value Properties"
    [ fastProperty "locatedAt creates correct located value" prop_locatedAt_correct
    , fastProperty "locatedWithSpan creates correct located value" prop_locatedWithSpan_correct
    , fastProperty "mapLocated preserves location" prop_mapLocated_preserves_location
    , fastProperty "HasLocation works correctly" prop_hasLocation_correct
    ]
  , testGroup "Position Advancement Properties"
    [ fastProperty "advancePos equals posAfter" prop_advancePos_equals_posAfter
    , fastProperty "advancePosBy advances sequentially" prop_advancePosBy_sequential
    , fastProperty "advancePosByText works with Text" prop_advancePosByText_equals_string
    , fastProperty "advancePosByLine changes line L.and column" prop_advancePosByLine_correct
    , fastProperty "advancePosByLine with zero preserves" prop_advancePosByLine_zero
    ]
  , testGroup "Error Location Properties"
    [ fastProperty "toErrorLocation preserves position" prop_toErrorLocation_preserves_pos
    , fastProperty "toErrorLocationWithSpan preserves span" prop_toErrorLocationWithSpan_preserves_span
    ]
  , testGroup "Complex Scenario Properties"
    [ fastProperty "Position advancement is consistent" prop_position_advancement_consistent
    , fastProperty "Span merging preserves containment" prop_span_merging_preserves_containment
    , fastProperty "Located mapping preserves structure" prop_located_mapping_preserves_structure
    , fastProperty "Position advancement with newlines" prop_position_advancement_newlines
    , fastProperty "Span validity after merging" prop_span_validity_after_merging
    ]
  ]