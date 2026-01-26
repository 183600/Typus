{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans  -Wno-unused-imports -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Unit.SourceLocationCorePropertiesSpec where


import Test.Tasty
import Test.Tasty.QuickCheck



import Test.Tasty
import Test.Tasty.QuickCheck

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
  )

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

-- Arbitrary instance for SourcePos is now defined in SourceLocation module


-- Arbitrary instance for SourceSpan is now defined in SourceLocation module


instance Arbitrary a => Arbitrary (Located a) where
  arbitrary = do
    value <- arbitrary
    pos <- arbitrary
    span <- arbitrary
    return $ Located value pos span

-- ============================================================================
-- SourcePos Properties
-- ============================================================================

-- Property: startPos should have line 1, column 1, offset 0
prop_startPos_properties :: Property
prop_startPos_properties = 
  posLine startPos === 1 .&&.
  posColumn startPos === 1 .&&.
  posOffset startPos === 0

-- Property: posAfter newline increments line and resets column
prop_posAfter_newline :: Positive Int -> Property
prop_posAfter_newline (Positive lineNum) = 
  let pos = posAt lineNum 5
      newPos = posAfter '\n' pos
  in posLine newPos === lineNum + 1 .&&. posColumn newPos === 1

-- Property: posAfter tab aligns to next 8-column boundary
prop_posAfter_tab :: Positive Int -> Property
prop_posAfter_tab (Positive col) = 
  let pos = posAt 1 col
      newPos = posAfter '\t' pos
      expectedCol = ((col - 1) `div` 8 + 1) * 8 + 1
  in posColumn newPos === expectedCol

-- Property: posAfter regular char increments column
prop_posAfter_regular_char :: Positive Int -> Property
prop_posAfter_regular_char (Positive col) = 
  let pos = posAt 1 col
      newPos = posAfter 'x' pos
  in posColumn newPos === col + 1 .&&. posLine newPos === 1

-- Property: posAt creates position with correct line and column
prop_posAt_correct :: Positive Int -> Positive Int -> Property
prop_posAt_correct (Positive line) (Positive col) = 
  let pos = posAt line col
  in posLine pos === line .&&. posColumn pos === col

-- ============================================================================
-- SourceSpan Properties
-- ============================================================================

-- Property: emptySpan creates span where start equals end
prop_emptySpan_properties :: SourcePos -> Property
prop_emptySpan_properties pos = 
  let span = emptySpan pos
  in spanStart span === pos .&&. spanEnd span === pos

-- Property: spanFrom creates empty span at position
prop_spanFrom_properties :: SourcePos -> Property
prop_spanFrom_properties pos = 
  let span = spanFrom pos
  in spanStart span === pos .&&. spanEnd span === pos

-- Property: spanTo creates empty span at position
prop_spanTo_properties :: SourcePos -> Property
prop_spanTo_properties pos = 
  let span = spanTo pos
  in spanStart span === pos .&&. spanEnd span === pos

-- Property: spanBetween creates span with correct start and end
prop_spanBetween_correct :: SourcePos -> SourcePos -> Property
prop_spanBetween_correct start end = 
  let span = spanBetween start end
  in spanStart span === start .&&. spanEnd span === end

-- Property: mergeSpans creates span covering both spans
prop_mergeSpans_correct :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_correct span1 span2 = 
  let merged = mergeSpans span1 span2
  in spanStart merged === min (spanStart span1) (spanStart span2) .&&.
     spanEnd merged === max (spanEnd span1) (spanEnd span2)

-- Property: isValidSpan returns True when start <= end
prop_isValidSpan_correct :: SourceSpan -> Property
prop_isValidSpan_correct span = 
  isValidSpan span === (spanStart span <= spanEnd span)

-- ============================================================================
-- Located Properties
-- ============================================================================

-- Property: locatedAt creates located value with empty span
prop_locatedAt_properties :: SourcePos -> String -> Property
prop_locatedAt_properties pos value = 
  let located = locatedAt pos value
  in locatedValue located === value .&&.
     locatedPos located === pos .&&.
     spanStart (locatedSpan located) === pos .&&.
     spanEnd (locatedSpan located) === pos

-- Property: locatedWithSpan creates located value with given span
prop_locatedWithSpan_properties :: SourceSpan -> String -> Property
prop_locatedWithSpan_properties span value = 
  let located = locatedWithSpan span value
  in locatedValue located === value .&&.
     locatedSpan located === span .&&.
     locatedPos located === spanStart span

-- Property: mapLocated applies function to value
prop_mapLocated_properties :: SourceSpan -> String -> Property
prop_mapLocated_properties span value = 
  let located = locatedWithSpan span value
      mapped = mapLocated (++ " suffix") located
  in locatedValue mapped === value ++ " suffix" .&&.
     locatedSpan mapped === span

-- ============================================================================
-- Position Advancement Properties
-- ============================================================================

-- Property: advancePos and posAfter are equivalent
prop_advancePos_equivalent :: SourcePos -> Char -> Property
prop_advancePos_equivalent pos char = 
  advancePos char pos === posAfter char pos

-- Property: advancePosBy empty string returns same position
prop_advancePosBy_empty :: SourcePos -> Property
prop_advancePosBy_empty pos = 
  advancePosBy "" pos === pos

-- Property: advancePosBy is consistent with successive advancePos calls
prop_advancePosBy_consistent :: SourcePos -> String -> Property
prop_advancePosBy_consistent pos chars = 
  advancePosBy chars pos === foldl (flip advancePos) pos chars

-- Property: advancePosByLine increments line and resets column
prop_advancePosByLine_correct :: SourcePos -> Positive Int -> Property
prop_advancePosByLine_correct pos (Positive lines) = 
  let newPos = advancePosByLine lines pos
  in posLine newPos === posLine pos + lines .&&. posColumn newPos === 1

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "SourceLocation Core Properties Tests"
  [ testGroup "SourcePos Properties"
    [ testProperty "startPos has correct initial values" prop_startPos_properties
    , testProperty "posAfter newline increments line and resets column" prop_posAfter_newline
    , testProperty "posAfter tab aligns to 8-column boundary" prop_posAfter_tab
    , testProperty "posAfter regular char increments column" prop_posAfter_regular_char
    , testProperty "posAt creates position with correct line and column" prop_posAt_correct
    ]
  , testGroup "SourceSpan Properties"
    [ testProperty "emptySpan creates span where start equals end" prop_emptySpan_properties
    , testProperty "spanFrom creates empty span at position" prop_spanFrom_properties
    , testProperty "spanTo creates empty span at position" prop_spanTo_properties
    , testProperty "spanBetween creates span with correct start and end" prop_spanBetween_correct
    , testProperty "mergeSpans creates span covering both spans" prop_mergeSpans_correct
    , testProperty "isValidSpan returns True when start <= end" prop_isValidSpan_correct
    ]
  , testGroup "Located Properties"
    [ testProperty "locatedAt creates located value with empty span" prop_locatedAt_properties
    , testProperty "locatedWithSpan creates located value with given span" prop_locatedWithSpan_properties
    , testProperty "mapLocated applies function to value" prop_mapLocated_properties
    ]
  , testGroup "Position Advancement Properties"
    [ testProperty "advancePos and posAfter are equivalent" prop_advancePos_equivalent
    , testProperty "advancePosBy empty string returns same position" prop_advancePosBy_empty
    , testProperty "advancePosBy is consistent with successive advancePos calls" prop_advancePosBy_consistent
    , testProperty "advancePosByLine increments line and resets column" prop_advancePosByLine_correct
    ]
  ]