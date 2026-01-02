{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewSourceLocationMathPropertiesSpec2 (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Positive(Positive), getPositive)

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
  )

import Data.List (sort)

-- Property: startPos has line 1, column 1, offset 0
prop_startPos_values :: Property
prop_startPos_values =
  posLine startPos === 1 .&&.
  posColumn startPos === 1 .&&.
  posOffset startPos === 0

-- Property: posAfter '\n' increments line L.and resets column to 1
prop_posAfter_newline :: Positive Int -> Positive Int -> Positive Int -> Property
prop_posAfter_newline (Positive line) (Positive col) (Positive offset) =
  let pos = posAtLineCol line col offset
      newPos = posAfter '\n' pos
  in posLine newPos === line + 1 .&&.
     posColumn newPos === 1 .&&.
     posOffset newPos === offset + 1

-- Property: posAfter '\t' advances to next tab stop
prop_posAfter_tab :: Positive Int -> Positive Int -> Positive Int -> Property
prop_posAfter_tab (Positive line) (Positive col) (Positive offset) =
  let pos = posAtLineCol line col offset
      newPos = posAfter '\t' pos
      expectedCol = ((col - 1) `div` 8 + 1) * 8 + 1
  in posLine newPos === line .&&.
     posColumn newPos === expectedCol .&&.
     posOffset newPos === offset + 1

-- Property: posAfter regular character increments column L.and offset
prop_posAfter_regular :: Positive Int -> Positive Int -> Positive Int -> Char -> Property
prop_posAfter_regular (Positive line) (Positive col) (Positive offset) c =
  c `notElem` "\n\t" ==> 
  let pos = posAtLineCol line col offset
      newPos = posAfter c pos
  in posLine newPos === line .&&.
     posColumn newPos === col + 1 .&&.
     posOffset newPos === offset + 1

-- Property: emptySpan creates span with same start L.and end
prop_emptySpan_same_start_end :: Positive Int -> Positive Int -> Positive Int -> Property
prop_emptySpan_same_start_end (Positive line) (Positive col) (Positive offset) =
  let pos = posAtLineCol line col offset
      span = emptySpan pos
  in spanStart span === pos .&&. spanEnd span === pos

-- Property: spanFrom creates empty span at position
prop_spanFrom_creates_empty_span :: Positive Int -> Positive Int -> Positive Int -> Property
prop_spanFrom_creates_empty_span (Positive line) (Positive col) (Positive offset) =
  let pos = posAtLineCol line col offset
      span = spanFrom pos
  in spanStart span === pos .&&. spanEnd span === pos

-- Property: spanTo creates span with same start L.and end at position
prop_spanTo_creates_span_at_position :: Positive Int -> Positive Int -> Positive Int -> Property
prop_spanTo_creates_span_at_position (Positive line) (Positive col) (Positive offset) =
  let pos = posAtLineCol line col offset
      span = spanTo pos
  in spanStart span === pos .&&. spanEnd span === pos

-- Property: spanBetween creates span with given start L.and end
prop_spanBetween_correct_bounds :: Positive Int -> Positive Int -> Positive Int -> 
                                   Positive Int -> Positive Int -> Positive Int -> Property
prop_spanBetween_correct_bounds (Positive line1) (Positive col1) (Positive offset1)
                                 (Positive line2) (Positive col2) (Positive offset2) =
  let start = posAtLineCol line1 col1 offset1
      end = posAtLineCol line2 col2 offset2
      span = spanBetween start end
  in spanStart span === start .&&. spanEnd span === end

-- Property: mergeSpans creates span that encompasses both spans
prop_mergeSpans_encompasses_both :: Positive Int -> Positive Int -> Positive Int ->
                                   Positive Int -> Positive Int -> Positive Int ->
                                   Positive Int -> Positive Int -> Positive Int ->
                                   Positive Int -> Positive Int -> Positive Int -> Property
prop_mergeSpans_encompasses_both (Positive line1) (Positive col1) (Positive offset1)
                                 (Positive line2) (Positive col2) (Positive offset2)
                                 (Positive line3) (Positive col3) (Positive offset3)
                                 (Positive line4) (Positive col4) (Positive offset4) =
  let start1 = posAtLineCol line1 col1 offset1
      end1 = posAtLineCol line2 col2 offset2
      start2 = posAtLineCol line3 col3 offset3
      end2 = posAtLineCol line4 col4 offset4
      span1 = spanBetween start1 end1
      span2 = spanBetween start2 end2
      merged = mergeSpans span1 span2
  in spanStart merged === min start1 start2 .&&.
     spanEnd merged === max end1 end2

-- Property: isValidSpan checks start <= end
prop_isValidSpan_ascending :: Positive Int -> Positive Int -> Positive Int ->
                            Positive Int -> Positive Int -> Positive Int -> Property
prop_isValidSpan_ascending (Positive line1) (Positive col1) (Positive offset1)
                           (Positive line2) (Positive col2) (Positive offset2) =
  let start = posAtLineCol line1 col1 offset1
      end = posAtLineCol line2 col2 offset2
      span = spanBetween start end
      expected = start <= end
  in isValidSpan span === expected

-- Property: locatedAt creates located value at position
prop_locatedAt_correct_position :: Positive Int -> Positive Int -> Positive Int -> Int -> Property
prop_locatedAt_correct_position (Positive line) (Positive col) (Positive offset) value =
  let pos = posAtLineCol line col offset
      located = locatedAt pos value
  in locatedPos located === pos .&&.
     locatedSpan located === emptySpan pos .&&.
     locatedValue located === value

-- Property: locatedWithSpan creates located value with span
prop_locatedWithSpan_correct_span :: Positive Int -> Positive Int -> Positive Int ->
                                    Positive Int -> Positive Int -> Positive Int -> Int -> Property
prop_locatedWithSpan_correct_span (Positive line1) (Positive col1) (Positive offset1)
                                  (Positive line2) (Positive col2) (Positive offset2) value =
  let start = posAtLineCol line1 col1 offset1
      end = posAtLineCol line2 col2 offset2
      span = spanBetween start end
      located = locatedWithSpan span value
  in locatedSpan located === span .&&.
     locatedPos located === start .&&.
     locatedValue located === value

-- Property: mapLocated preserves location but transforms value
prop_mapLocated_preserves_location :: Positive Int -> Positive Int -> Positive Int -> Int -> Property
prop_mapLocated_preserves_location (Positive line) (Positive col) (Positive offset) value =
  let pos = posAtLineCol line col offset
      located = locatedAt pos value
      transformed = mapLocated (*2) located
  in locatedPos transformed === locatedPos located .&&.
     locatedSpan transformed === locatedSpan located .&&.
     locatedValue transformed === value * 2

-- Property: advancePos advances position by text
prop_advancePos_advances_position :: Positive Int -> Positive Int -> Positive Int -> String -> Property
prop_advancePos_advances_position (Positive line) (Positive col) (Positive offset) text =
  not (null text) ==> 
  let pos = posAtLineCol line col offset
      newPos = advancePos pos text
  in posOffset newPos >= posOffset pos

-- Property: advancePosBy advances position by specific amount
prop_advancePosBy_advances_by_n :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> Property
prop_advancePosBy_advances_by_n (Positive line) (Positive col) (Positive offset) (Positive n) =
  let pos = posAtLineCol line col offset
      newPos = advancePosBy pos n
  in posOffset newPos === offset + n

tests :: TestTree
tests =
  testGroup "SourceLocation Math Properties"
    [ fastProperty "startPos has correct values" prop_startPos_values
    , fastProperty "posAfter '\\n' increments line L.and resets column" prop_posAfter_newline
    , fastProperty "posAfter '\\t' advances to next tab stop" prop_posAfter_tab
    , fastProperty "posAfter regular character increments column L.and offset" prop_posAfter_regular
    , fastProperty "emptySpan creates span with same start L.and end" prop_emptySpan_same_start_end
    , fastProperty "spanFrom creates empty span at position" prop_spanFrom_creates_empty_span
    , fastProperty "spanTo creates span at position" prop_spanTo_creates_span_at_position
    , fastProperty "spanBetween creates span with correct bounds" prop_spanBetween_correct_bounds
    , fastProperty "mergeSpans encompasses both spans" prop_mergeSpans_encompasses_both
    , fastProperty "isValidSpan checks ascending order" prop_isValidSpan_ascending
    , fastProperty "locatedAt creates located value at position" prop_locatedAt_correct_position
    , fastProperty "locatedWithSpan creates located value with span" prop_locatedWithSpan_correct_span
    , fastProperty "mapLocated preserves location but transforms value" prop_mapLocated_preserves_location
    , fastProperty "advancePos advances position by text" prop_advancePos_advances_position
    , fastProperty "advancePosBy advances by n" prop_advancePosBy_advances_by_n
    ]