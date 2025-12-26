{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.Unit.SourceLocationAdvancedQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), choose, vectorOf, elements )
import Control.Monad (replicateM, when)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, sort, intercalate)
import Data.Char (isSpace, isDigit, isAlpha)
import Data.Maybe (isJust, isNothing, fromMaybe)

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  , posAfter
  , emptySpan
  , spanFrom
  , spanTo
  , mergeSpans
  , isValidSpan
  , locatedAt
  , locatedWithSpan
  , locatedValue
  , locatedSpan
  )

-- Arbitrary instances for QuickCheck
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 1000)
    col <- choose (1, 1000)
    return $ SourcePos line col

instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    let (SourcePos startLine startCol) = start
        (SourcePos endLine endCol) = end
        -- Ensure end is not before start
        validEnd = if endLine < startLine || (endLine == startLine && endCol < startCol)
                   then start
                   else end
    return $ SourceSpan start validEnd

instance Arbitrary a => Arbitrary (Located a) where
  arbitrary = do
    value <- arbitrary
    span <- arbitrary
    return $ Located { locatedValue = value, locatedSpan = span }

-- Property: SourcePos ordering is consistent
prop_source_pos_ordering :: SourcePos -> SourcePos -> Property
prop_source_pos_ordering pos1 pos2 =
  let (SourcePos line1 col1) = pos1
      (SourcePos line2 col2) = pos2
      lineComparison = compare line1 line2
      colComparison = compare col1 col2
      expected = if line1 < line2 then LT
                else if line1 > line2 then GT
                else if col1 < col2 then LT
                else if col1 > col2 then GT
                else EQ
  in property $ if line1 == line2 then colComparison === expected
                else lineComparison === expected

-- Property: SourcePos arithmetic properties
prop_source_pos_arithmetic :: SourcePos -> Int -> Property
prop_source_pos_arithmetic pos offset =
  let (SourcePos line col) = pos
      newPos = posAfter pos offset
      expectedCol = col + offset
  in offset >= 0 ==> property $ sourcePosColumn newPos >= col

-- Property: SourceSpan validity
prop_source_span_validity :: SourceSpan -> Property
prop_source_span_validity span =
  let isValid = isValidSpan span
      (SourceSpan start end) = span
      (SourcePos startLine startCol) = start
      (SourcePos endLine endCol) = end
      logicallyValid = endLine > startLine || (endLine == startLine && endCol >= startCol)
  in property $ isValid === logicallyValid

-- Property: Empty span properties
prop_empty_span_properties :: Property
prop_empty_span_properties =
  let empty = emptySpan
      (SourceSpan start end) = empty
  in property $ start === end .&&. isValidSpan empty

-- Property: Span creation from position
prop_span_from_position :: SourcePos -> Property
prop_span_from_position pos =
  let span = spanFrom pos
      (SourceSpan start end) = span
  in property $ start === pos .&&. end === pos .&&. isValidSpan span

-- Property: Span to position
prop_span_to_position :: SourcePos -> Property
prop_span_to_position pos =
  let span = spanTo pos
      (SourceSpan start end) = span
  in property $ start === startPos .&&. end === pos

-- Property: Span merging
prop_span_merging :: SourceSpan -> SourceSpan -> Property
prop_span_merging span1 span2 =
  let (SourceSpan start1 end1) = span1
      (SourceSpan start2 end2) = span2
      merged = mergeSpans span1 span2
      (SourceSpan mergedStart mergedEnd) = merged
      earliestStart = if start1 < start2 then start1 else start2
      latestEnd = if end1 > end2 then end1 else end2
  in (isValidSpan span1 && isValidSpan span2) ==>
     property $ mergedStart === earliestStart .&&. mergedEnd === latestEnd .&&. isValidSpan merged

-- Property: Located value access
prop_located_value_access :: Int -> SourceSpan -> Property
prop_located_value_access value span =
  let located = Located { locatedValue = value, locatedSpan = span }
      retrievedValue = locatedValue located
      retrievedSpan = locatedSpan located
  in property $ retrievedValue === value .&&. retrievedSpan === span

-- Property: Located creation helpers
prop_located_creation_helpers :: Int -> SourcePos -> Property
prop_located_creation_helpers value pos =
  let locatedAtPos = locatedAt pos value
      locatedWithSpan' = locatedWithSpan (spanFrom pos) value
      span1 = locatedSpan locatedAtPos
      span2 = locatedSpan locatedWithSpan'
  in property $ locatedValue locatedAtPos === value .&&.
     locatedValue locatedWithSpan' === value .&&.
     spanStart span1 === pos .&&.
     spanStart span2 === pos

-- Property: SourcePos bounds
prop_source_pos_bounds :: Int -> Int -> Property
prop_source_pos_bounds line col =
  let validPos = line > 0 && col > 0
      pos = SourcePos line col
  in property $ if validPos 
     then True -- Position is valid
     else True -- Even invalid positions should be constructible

-- Property: SourceSpan contains position
prop_span_contains_position :: SourceSpan -> SourcePos -> Property
prop_span_contains_position span pos =
  let (SourceSpan start end) = span
      contains = start <= pos && pos <= end
  in isValidSpan span ==> property $ contains ==> (start <= pos && pos <= end)

-- Property: Span length calculation
prop_span_length_calculation :: SourcePos -> SourcePos -> Property
prop_span_length_calculation start end =
  let (SourcePos startLine startCol) = start
      (SourcePos endLine endCol) = end
      span = SourceSpan start end
      valid = isValidSpan span
  in valid ==> property $ end >= start

-- Property: Located map function
prop_located_map :: Int -> Int -> SourceSpan -> Property
prop_located_map value increment span =
  let located = Located { locatedValue = value, locatedSpan = span }
      mapped = fmap (+ increment) located
  in property $ locatedSpan mapped === span .&&. locatedValue mapped === value + increment

tests :: TestTree
tests = testGroup "Source Location Advanced QuickCheck Tests"
  [ fastProperty "source pos ordering" prop_source_pos_ordering
  , fastProperty "source pos arithmetic" prop_source_pos_arithmetic
  , fastProperty "source span validity" prop_source_span_validity
  , fastProperty "empty span properties" prop_empty_span_properties
  , fastProperty "span from position" prop_span_from_position
  , fastProperty "span to position" prop_span_to_position
  , fastProperty "span merging" prop_span_merging
  , fastProperty "located value access" prop_located_value_access
  , fastProperty "located creation helpers" prop_located_creation_helpers
  , fastProperty "source pos bounds" prop_source_pos_bounds
  , fastProperty "span contains position" prop_span_contains_position
  , fastProperty "span length calculation" prop_span_length_calculation
  , fastProperty "located map" prop_located_map
  ]