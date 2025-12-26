{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Unit.SourceLocationCoreQuickCheckTests (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperties, (===), Property, forAll, Gen, Arbitrary(..), oneof, elements, listOf, listOf1, resize, suchThat)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)

import SourceLocation 
  ( SourcePos(..), SourceSpan(..), Located(..), HasLocation(..)
  , startPos, posAfter, posAt, posAtLineCol
  , emptySpan, spanFrom, spanTo, spanBetween, mergeSpans, isValidSpan
  , locatedAt, locatedWithSpan, locatedValue, locatedSpan, locatedPos, mapLocated
  , advancePos, advancePosBy
  )

import Data.Semigroup ((<>))

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary SourcePos where
  arbitrary = SourcePos <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary SourceSpan where
  arbitrary = do
    startLine <- arbitrary
    startCol <- arbitrary
    endLine <- arbitrary `suchThat` (>= startLine)
    endCol <- if endLine == startLine 
                then arbitrary `suchThat` (>= startCol)
                else arbitrary
    return $ SourceSpan (SourcePos startLine startCol 0) (SourcePos endLine endCol 0)

instance (Arbitrary a) => Arbitrary (Located a) where
  arbitrary = Located <$> arbitrary <*> arbitrary

-- ============================================================================
-- QuickCheck Properties for SourceLocation Module
-- ============================================================================

-- | startPos: should have line and column 1
prop_startPos_properties :: Bool
prop_startPos_properties = 
    let pos = startPos
    in sourceLine pos == 1 && sourceColumn pos == 1

-- | posAfter: moving to next character should increment column
prop_posAfter_same_line :: SourcePos -> Bool
prop_posAfter_same_line pos = 
    let nextPos = posAfter pos
    in sourceLine nextPos == sourceLine pos && 
       sourceColumn nextPos == sourceColumn pos + 1

-- | posAt: creating position at specific line and column
prop_posAt_correctness :: Int -> Int -> Bool
prop_posAt_correctness line col = 
    let pos = posAt line col
    in sourceLine pos == line && sourceColumn pos == col

-- | posAtLineCol: should be equivalent to posAt
prop_posAtLineCol_equivalent :: Int -> Int -> Bool
prop_posAtLineCol_equivalent line col = 
    posAt line col == posAtLineCol line col

-- | emptySpan: should have same start and end positions
prop_emptySpan_properties :: SourcePos -> Bool
prop_emptySpan_properties pos = 
    let span = emptySpan pos
    in spanStart span == spanEnd span && spanStart span == pos

-- | spanFrom: should create span from position to itself
prop_spanFrom_properties :: SourcePos -> Bool
prop_spanFrom_properties pos = 
    let span = spanFrom pos
    in spanStart span == pos && spanEnd span == pos

-- | spanTo: should create span from position to position
prop_spanTo_properties :: SourcePos -> SourcePos -> Bool
prop_spanTo_properties start end = 
    let span = spanTo start end
    in spanStart span == start && spanEnd span == end

-- | spanBetween: should create span between two positions
prop_spanBetween_correctness :: SourcePos -> SourcePos -> Bool
prop_spanBetween_correctness pos1 pos2 = 
    let span = spanBetween pos1 pos2
    in (spanStart span == pos1 && spanEnd span == pos2) ||
       (spanStart span == pos2 && spanEnd span == pos1)

-- | isValidSpan: span with start <= end should be valid
prop_isValidSpan_ordered :: SourceSpan -> Bool
prop_isValidSpan_ordered span = 
    let start = spanStart span
        end = spanEnd span
        lineStart = sourceLine start
        lineEnd = sourceLine end
        colStart = sourceColumn start
        colEnd = sourceColumn end
    in if lineStart < lineEnd
       then isValidSpan span
       else if lineStart == lineEnd
            then colStart <= colEnd && isValidSpan span
            else not (isValidSpan span)

-- | locatedAt: should create located value at position
prop_locatedAt_properties :: Int -> String -> Bool
prop_locatedAt_properties line value = 
    let located = locatedAt line value
        span = locatedSpan located
    in locatedValue located == value &&
       sourceLine (spanStart span) == line &&
       sourceLine (spanEnd span) == line

-- | locatedWithSpan: should create located value with specific span
prop_locatedWithSpan_properties :: SourceSpan -> String -> Bool
prop_locatedWithSpan_properties span value = 
    let located = locatedWithSpan span value
    in locatedValue located == value && locatedSpan located == span

-- | locatedPos: should return start position of span
prop_locatedPos_correctness :: Located String -> Bool
prop_locatedPos_correctness located = 
    locatedPos located == spanStart (locatedSpan located)

-- | mapLocated: should apply function to value while preserving location
prop_mapLocated_preserves_location :: Located String -> Bool
prop_mapLocated_preserves_location located = 
    let f = reverse
        mapped = mapLocated f located
    in locatedSpan mapped == locatedSpan located &&
       locatedValue mapped == f (locatedValue located)

-- | advancePos: advancing by 0 should return same position
prop_advancePos_zero :: SourcePos -> Bool
prop_advancePos_zero pos = advancePos pos '\0' == pos

-- | advancePosBy: advancing by 0 should return same position
prop_advancePosBy_zero :: SourcePos -> Bool
prop_advancePosBy_zero pos = advancePosBy pos 0 == pos

-- | mergeSpans: merging with empty span should return original
prop_mergeSpans_empty :: SourceSpan -> Bool
prop_mergeSpans_empty span = 
    let empty = emptySpan (spanStart span)
    in mergeSpans span empty == span

-- | mergeSpans: should be commutative
prop_mergeSpans_commutative :: SourceSpan -> SourceSpan -> Bool
prop_mergeSpans_commutative span1 span2 = 
    mergeSpans span1 span2 == mergeSpans span2 span1

-- | mergeSpans: should be associative
prop_mergeSpans_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Bool
prop_mergeSpans_associative span1 span2 span3 = 
    mergeSpans (mergeSpans span1 span2) span3 == mergeSpans span1 (mergeSpans span2 span3)

-- | Located Functor laws: identity
prop_located_functor_identity :: Located String -> Bool
prop_located_functor_identity located = 
    fmap id located == located

-- | Located Functor laws: composition
prop_located_functor_composition :: Located String -> String -> String -> Bool
prop_located_functor_composition located f g = 
    fmap (f . g) located == fmap f (fmap g located)

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "SourceLocation Core QuickCheck Tests"
  [ testProperties "Source Position Properties"
    [ ("startPos properties", prop_startPos_properties)
    , ("posAfter same line", prop_posAfter_same_line)
    , ("posAt correctness", prop_posAt_correctness)
    , ("posAtLineCol equivalent", prop_posAtLineCol_equivalent)
    ]

  , testProperties "Source Span Properties"
    [ ("emptySpan properties", prop_emptySpan_properties)
    , ("spanFrom properties", prop_spanFrom_properties)
    , ("spanTo properties", prop_spanTo_properties)
    , ("spanBetween correctness", prop_spanBetween_correctness)
    , ("isValidSpan ordered", prop_isValidSpan_ordered)
    ]

  , testProperties "Located Value Properties"
    [ ("locatedAt properties", prop_locatedAt_properties)
    , ("locatedWithSpan properties", prop_locatedWithSpan_properties)
    , ("locatedPos correctness", prop_locatedPos_correctness)
    , ("mapLocated preserves location", prop_mapLocated_preserves_location)
    , ("located functor identity", prop_located_functor_identity)
    , ("located functor composition", prop_located_functor_composition)
    ]

  , testProperties "Position Advancement Properties"
    [ ("advancePos zero", prop_advancePos_zero)
    , ("advancePosBy zero", prop_advancePosBy_zero)
    ]

  , testProperties "Span Merging Properties"
    [ ("mergeSpans empty", prop_mergeSpans_empty)
    , ("mergeSpans commutative", prop_mergeSpans_commutative)
    , ("mergeSpans associative", prop_mergeSpans_associative)
    ]
  ]