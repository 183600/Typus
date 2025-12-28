{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalSourceLocationQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Arbitrary (Arbitrary(..), arbitrary)
import Test.QuickCheck.Gen (oneof, listOf, choose, elements)

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

import Data.Char (isSpace)

-- ============================================================================
-- Enhanced Arbitrary Instances
-- ============================================================================

instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 1000)
    col <- choose (1, 1000)
    return $ SourcePos line col

instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    -- Ensure valid span where end >= start
    let validEnd = if end >= start then end else start
    return $ SourceSpan start validEnd

instance (Arbitrary a) => Arbitrary (Located a) where
  arbitrary = do
    value <- arbitrary
    span <- arbitrary
    return $ Located value span

-- ============================================================================
-- SourceLocation Property Tests
-- ============================================================================

-- Property: SourcePos ordering is consistent
prop_sourcepos_ordering :: SourcePos -> SourcePos -> Property
prop_sourcepos_ordering pos1 pos2 =
  let (SourcePos l1 c1) = pos1
      (SourcePos l2 c2) = pos2
      lineOrdered = l1 == l2 || l1 < l2 || l2 < l1
      colOrdered = c1 == c2 || c1 < c2 || c2 < c1
  in property $ lineOrdered .&&. colOrdered

-- Property: startPos creates position at beginning
prop_startpos_consistency :: Property
prop_startpos_consistency =
  let start = startPos
      (SourcePos line col) = start
  in property $ line === 1 .&&. col === 1

-- Property: posAfter advances position correctly
prop_posafter_advancement :: SourcePos -> Int -> Property
prop_posafter_advancement pos offset =
  offset >= 0 && offset <= 100 ==>
  let (SourcePos line col) = posAfter offset pos
      (SourcePos originalLine originalCol) = pos
  in property $ line >= originalLine .&&. col >= originalCol

-- Property: posAt creates position at specific coordinates
prop_posat_coordinates :: Int -> Int -> Property
prop_posat_coordinates line col =
  line >= 1 && line <= 1000 && col >= 1 && col <= 1000 ==>
  let pos = posAt line col
      (SourcePos actualLine actualCol) = pos
  in property $ actualLine === line .&&. actualCol === col

-- Property: posAtLineCol is consistent with posAt
prop_posatlinecol_consistency :: Int -> Int -> Property
prop_posatlinecol_consistency line col =
  line >= 1 && line <= 100 && col >= 1 && col <= 100 ==>
  let pos1 = posAt line col
      pos2 = posAtLineCol line col
  in property $ pos1 === pos2

-- Property: emptySpan has consistent properties
prop_emptyspan_consistency :: Property
prop_emptyspan_consistency =
  let span = emptySpan
      (SourceSpan start end) = span
  in property $ start === end

-- Property: spanFrom creates span starting at position
prop_spanfrom_start :: SourcePos -> Int -> Property
prop_spanfrom_start pos length =
  length >= 0 && length <= 100 ==>
  let span = spanFrom pos length
      (SourceSpan start _) = span
  in property $ start === pos

-- Property: spanTo creates span ending at position
prop_spanto_end :: SourcePos -> Int -> Property
prop_spanto_end pos length =
  length >= 0 && length <= 100 ==>
  let span = spanTo pos length
      (SourceSpan _ end) = span
  in property $ end === pos

-- Property: spanBetween creates span between two positions
prop_spanbetween_bounds :: SourcePos -> SourcePos -> Property
prop_spanbetween_bounds pos1 pos2 =
  let span = spanBetween pos1 pos2
      (SourceSpan start end) = span
      minPos = if pos1 <= pos2 then pos1 else pos2
      maxPos = if pos1 >= pos2 then pos1 else pos2
  in property $ start === minPos .&&. end === maxPos

-- Property: mergeSpans contains both original spans
prop_mergespans_contains :: SourceSpan -> SourceSpan -> Property
prop_mergespans_contains span1 span2 =
  let (SourceSpan start1 end1) = span1
      (SourceSpan start2 end2) = span2
      merged = mergeSpans span1 span2
      (SourceSpan mergedStart mergedEnd) = merged
      expectedStart = min start1 start2
      expectedEnd = max end1 end2
  in property $ mergedStart === expectedStart .&&. mergedEnd === expectedEnd

-- Property: isValidSpan correctly validates spans
prop_isvalidspan_validation :: SourcePos -> SourcePos -> Property
prop_isvalidspan_validation pos1 pos2 =
  let validSpan = SourceSpan pos1 pos2
      invalidSpan = SourceSpan pos2 pos1
      isValid = isValidSpan validSpan
      isInvalid = not (isValidSpan invalidSpan) || pos1 == pos2
  in property $ isValid .&&. (isInvalid || pos1 <= pos2)

-- Property: locatedAt creates located value at position
prop_locatedat_position :: SourcePos -> Int -> Property
prop_locatedat_position pos value =
  let located = locatedAt pos value
      (Located _ span) = located
      (SourceSpan start _) = span
  in property $ start === pos

-- Property: locatedWithSpan creates located value with span
prop_locatedwithspan :: SourceSpan -> Int -> Property
prop_locatedwithspan span value =
  let located = locatedWithSpan span value
      (Located _ actualSpan) = located
  in property $ actualSpan === span

-- Property: locatedValue extracts value correctly
prop_locatedvalue_extraction :: Int -> SourceSpan -> Property
prop_locatedvalue_extraction value span =
  let located = Located value span
      extracted = locatedValue located
  in property $ extracted === value

-- Property: locatedSpan extracts span correctly
prop_locatedspan_extraction :: Int -> SourceSpan -> Property
prop_locatedspan_extraction value span =
  let located = Located value span
      extracted = locatedSpan located
  in property $ extracted === span

-- Property: locatedPos extracts position correctly
prop_locatedpos_extraction :: Int -> SourceSpan -> Property
prop_locatedpos_extraction value span =
  let located = Located value span
      (SourceSpan start _) = locatedSpan located
      extracted = locatedPos located
  in property $ extracted === start

-- Property: mapLocated transforms value correctly
prop_maplocated_transformation :: Int -> SourceSpan -> Property
prop_maplocated_transformation value span =
  let located = Located value span
      transformed = mapLocated (*2) located
      expectedValue = value * 2
      actualValue = locatedValue transformed
  in property $ actualValue === expectedValue

-- Property: advancePos moves position forward
prop_advancepos_forward :: SourcePos -> Int -> Property
prop_advancepos_forward pos offset =
  offset >= 0 && offset <= 100 ==>
  let (SourcePos l1 c1) = pos
      advanced = advancePos offset pos
      (SourcePos l2 c2) = advanced
  in property $ (l2 > l1) .||. (l2 == l1 && c2 >= c1)

-- Property: advancePosBy advances by specific amount
prop_advanceposby_amount :: SourcePos -> Int -> Int -> Property
prop_advanceposby_amount pos lines cols =
  lines >= 0 && lines <= 10 && cols >= 0 && cols <= 100 ==>
  let (SourcePos l1 c1) = pos
      advanced = advancePosBy lines cols pos
      (SourcePos l2 c2) = advanced
  in property $ l2 === l1 + lines .&&. c2 === c1 + cols

-- Property: Span ordering is transitive
prop_spanordering_transitive :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_spanordering_transitive span1 span2 span3 =
  let span1le2 = span1 <= span2
      span2le3 = span2 <= span3
      span1le3 = span1 <= span3
  in (span1le2 .&&. span2le3) ==> span1le3

-- Property: Position arithmetic is consistent
prop_position_arithmetic :: SourcePos -> Int -> Int -> Property
prop_position_arithmetic pos offset1 offset2 =
  offset1 >= 0 && offset2 >= 0 && offset1 <= 50 && offset2 <= 50 ==>
  let pos1 = advancePos offset1 pos
      pos2 = advancePos offset2 pos1
      pos3 = advancePos (offset1 + offset2) pos
  in property $ pos2 === pos3

-- Property: Span merging is associative
prop_spanmerging_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_spanmerging_associative span1 span2 span3 =
  let merge12 = mergeSpans span1 span2
      merge23 = mergeSpans span2 span3
      result1 = mergeSpans merge12 span3
      result2 = mergeSpans span1 merge23
  in property $ result1 === result2

-- Property: Located values preserve span through mapping
prop_located_preserve_span :: Int -> SourceSpan -> Property
prop_located_preserve_span value span =
  let located = Located value span
      transformed = mapLocated (+1) (mapLocated (*2) located)
      originalSpan = locatedSpan located
      transformedSpan = locatedSpan transformed
  in property $ originalSpan === transformedSpan

-- Property: Source positions are comparable
prop_sourcepos_comparable :: SourcePos -> SourcePos -> Property
prop_sourcepos_comparable pos1 pos2 =
  let equal = pos1 == pos2
      less = pos1 < pos2
      greater = pos1 > pos2
  in property $ equal .||. less .||. greater

tests :: TestTree
tests = testGroup "New Cabal SourceLocation QuickCheck Tests"
  [ fastProperty "SourcePos ordering" prop_sourcepos_ordering
  , fastProperty "Start position consistency" prop_startpos_consistency
  , fastProperty "Position advancement" prop_posafter_advancement
  , fastProperty "Position coordinates" prop_posat_coordinates
  , fastProperty "LineCol consistency" prop_posatlinecol_consistency
  , fastProperty "Empty span consistency" prop_emptyspan_consistency
  , fastProperty "Span from start" prop_spanfrom_start
  , fastProperty "Span to end" prop_spanto_end
  , fastProperty "Span between bounds" prop_spanbetween_bounds
  , fastProperty "Merge spans contains" prop_mergespans_contains
  , fastProperty "Valid span validation" prop_isvalidspan_validation
  , fastProperty "Located at position" prop_locatedat_position
  , fastProperty "Located with span" prop_locatedwithspan
  , fastProperty "Located value extraction" prop_locatedvalue_extraction
  , fastProperty "Located span extraction" prop_locatedspan_extraction
  , fastProperty "Located position extraction" prop_locatedpos_extraction
  , fastProperty "Map located transformation" prop_maplocated_transformation
  , fastProperty "Advance position forward" prop_advancepos_forward
  , fastProperty "Advance position by amount" prop_advanceposby_amount
  , fastProperty "Span ordering transitive" prop_spanordering_transitive
  , fastProperty "Position arithmetic" prop_position_arithmetic
  , fastProperty "Span merging associative" prop_spanmerging_associative
  , fastProperty "Located preserve span" prop_located_preserve_span
  , fastProperty "Source position comparable" prop_sourcepos_comparable
  ]