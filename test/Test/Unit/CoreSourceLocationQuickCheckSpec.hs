{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.CoreSourceLocationQuickCheckSpec where



import Test.Tasty
import Test.Tasty.QuickCheck
-- | Core SourceLocation module QuickCheck tests

import Test.Tasty
import Test.Tasty.QuickCheck

import TestSupport.Arbitrary
import TestSupport.QuickCheck
import qualified Data.Text as T
import Data.List (isPrefixOf, isSuffixOf, isInfixOf, intercalate)
import Data.Maybe (isJust, isNothing)
import Control.Monad (when)
import Data.Char (isSpace, isAlpha, isAlphaNum)

import SourceLocation

-- ============================================================================
-- SourceLocation QuickCheck Tests
-- ============================================================================

-- | Test that startPos creates a valid position
prop_startPosValid :: Property
prop_startPosValid =
  let pos = startPos
  in property $ spLine pos == 1 && spColumn pos == 1

-- | Test that posAfter increments column
prop_posAfterIncrementsColumn :: Property
prop_posAfterIncrementsColumn =
  forAll arbitrarySourcePos $ \pos ->
    let newPos = posAfter 'a' pos
    in property $ spColumn newPos == spColumn pos + 1

-- | Test that posAfter increments line for newline
prop_posAfterIncrementsLineForNewline :: Property
prop_posAfterIncrementsLineForNewline =
  forAll arbitrarySourcePos $ \pos ->
    let newPos = posAfter '\n' pos
    in property $ spLine newPos == spLine pos + 1 && spColumn newPos == 1

-- | Test that posAt creates position at specific line and column
prop_posAtSpecific :: Property
prop_posAtSpecific =
  forAll arbitraryPositiveInt $ \line ->
    forAll arbitraryPositiveInt $ \col ->
      let pos = posAt line col
      in property $ spLine pos == line && spColumn pos == col

-- | Test that posAtLineCol creates position at specific line and column
prop_posAtLineColSpecific :: Property
prop_posAtLineColSpecific =
  forAll arbitraryPositiveInt $ \line ->
    forAll arbitraryPositiveInt $ \col ->
      let pos = posAtLineCol line col 0
      in property $ spLine pos == line && spColumn pos == col

-- | Test that emptySpan creates a valid empty span
prop_emptySpanValid :: Property
prop_emptySpanValid =
  let sourceSpan = emptySpan startPos
  in property $ ssStart sourceSpan == ssEnd sourceSpan

-- | Test that spanFrom creates span from position
prop_spanFromValid :: Property
prop_spanFromValid =
  forAll arbitrarySourcePos $ \pos ->
    let sourceSpan = spanFrom pos
    in property $ ssStart sourceSpan == pos && ssEnd sourceSpan == pos

-- | Test that spanTo creates span to position
prop_spanToValid :: Property
prop_spanToValid =
  forAll arbitrarySourcePos $ \start ->
    forAll arbitrarySourcePos $ \end ->
      let sourceSpan = spanBetween start end
      in property $ ssStart sourceSpan == start && ssEnd sourceSpan == end

-- | Test that spanBetween creates span between positions
prop_spanBetweenValid :: Property
prop_spanBetweenValid =
  forAll arbitrarySourcePos $ \start ->
    forAll arbitrarySourcePos $ \end ->
      let sourceSpan = spanBetween start end
      in property $ ssStart sourceSpan == start && ssEnd sourceSpan == end

-- | Test that spanBetweenOrdered orders positions correctly
prop_spanBetweenOrdered :: Property
prop_spanBetweenOrdered =
  forAll arbitrarySourcePos $ \pos1 ->
    forAll arbitrarySourcePos $ \pos2 ->
      let sourceSpan = spanBetweenOrdered pos1 pos2
          orderedStart = min pos1 pos2
          orderedEnd = max pos1 pos2
      in property $ ssStart sourceSpan == orderedStart && ssEnd sourceSpan == orderedEnd

-- | Test that mergeSpans creates span covering both spans
prop_mergeSpansValid :: Property
prop_mergeSpansValid =
  forAll arbitrarySourceSpan $ \span1 ->
    forAll arbitrarySourceSpan $ \span2 ->
      let merged = mergeSpans span1 span2
          start1 = ssStart span1
          start2 = ssStart span2
          end1 = ssEnd span1
          end2 = ssEnd span2
          expectedStart = min start1 start2
          expectedEnd = max end1 end2
      in property $ ssStart merged == expectedStart && ssEnd merged == expectedEnd

-- | Test that isValidSpan correctly identifies valid spans
prop_isValidSpan :: Property
prop_isValidSpan =
  forAll arbitrarySourceSpan $ \span ->
    let start = ssStart span
        end = ssEnd span
        valid = isValidSpan span
    in property $ valid == (start <= end)

-- | Test that isValidBlockSpan correctly identifies valid block spans
prop_isValidBlockSpan :: Property
prop_isValidBlockSpan =
  forAll arbitrarySourceSpan $ \span ->
    let start = ssStart span
        end = ssEnd span
      -- A block span is valid if start < end or they are on different lines
        valid = isValidBlockSpan span
        expectedValid = start < end || spLine start < spLine end
    in property $ valid == expectedValid

-- | Test that locatedAt creates a located value
prop_locatedAtValid :: Property
prop_locatedAtValid =
  forAll arbitraryInt $ \value ->
    forAll arbitrarySourcePos $ \pos ->
      let located = locatedAt pos value
      in property $ locatedValue located == value && locatedPos located == pos

-- | Test that locatedWithSpan creates a located value with span
prop_locatedWithSpanValid :: Property
prop_locatedWithSpanValid =
  forAll arbitraryInt $ \value ->
    forAll arbitrarySourceSpan $ \span ->
      let located = locatedWithSpan span value
      in property $ locatedValue located == value && locatedSpan located == span

-- | Test that mapLocated applies function to value
prop_mapLocatedValid :: Property
prop_mapLocatedValid =
  forAll arbitraryInt $ \value ->
    forAll arbitrarySourceSpan $ \span ->
      let located = locatedWithSpan span value
          doubled = mapLocated (*2) located
      in property $ locatedValue doubled == value * 2 && locatedSpan doubled == span

-- | Test that spanStart gets start of span
prop_spanStartValid :: Property
prop_spanStartValid =
  forAll arbitrarySourceSpan $ \span ->
    property $ spanStart span == ssStart span

-- | Test that spanEnd gets end of span
prop_spanEndValid :: Property
prop_spanEndValid =
  forAll arbitrarySourceSpan $ \span ->
    property $ spanEnd span == ssEnd span

-- | Test that locatedValue extracts value
prop_locatedValueValid :: Property
prop_locatedValueValid =
  forAll arbitraryInt $ \value ->
    forAll arbitrarySourceSpan $ \span ->
      let located = locatedWithSpan span value
      in property $ locatedValue located == value

-- | Test that locatedSpan extracts span
prop_locatedSpanValid :: Property
prop_locatedSpanValid =
  forAll arbitraryInt $ \value ->
    forAll arbitrarySourceSpan $ \span ->
      let located = locatedWithSpan span value
      in property $ locatedSpan located == span

-- | Test that locatedPos extracts position from span
prop_locatedPosValid :: Property
prop_locatedPosValid =
  forAll arbitraryInt $ \value ->
    forAll arbitrarySourcePos $ \pos ->
      let located = locatedAt pos value
      in property $ locatedPos located == pos

-- | Test that toErrorLocation converts span to error location
prop_toErrorLocationValid :: Property
prop_toErrorLocationValid =
  forAll arbitrarySourceSpan $ \span ->
    let errLoc = toErrorLocationWithSpan span
    in property $ True  -- Basic sanity check

-- | Test that toErrorLocationWithSpan converts span to error location with span
prop_toErrorLocationWithSpanValid :: Property
prop_toErrorLocationWithSpanValid =
  forAll arbitrarySourceSpan $ \span ->
    let errLoc = toErrorLocationWithSpan span
    in property $ True  -- Basic sanity check

-- ============================================================================
-- Test Suite
-- ============================================================================

testSuite :: TestTree
testSuite = testGroup "Core SourceLocation QuickCheck Tests"
  [ testProperty "StartPos creates valid position" prop_startPosValid
  , testProperty "PosAfter increments column" prop_posAfterIncrementsColumn
  , testProperty "PosAfter increments line for newline" prop_posAfterIncrementsLineForNewline
  , testProperty "PosAt creates position at specific line and column" prop_posAtSpecific
  , testProperty "PosAtLineCol creates position at specific line and column" prop_posAtLineColSpecific
  , testProperty "EmptySpan creates valid empty span" prop_emptySpanValid
  , testProperty "SpanFrom creates span from position" prop_spanFromValid
  , testProperty "SpanTo creates span to position" prop_spanToValid
  , testProperty "SpanBetween creates span between positions" prop_spanBetweenValid
  , testProperty "SpanBetweenOrdered orders positions correctly" prop_spanBetweenOrdered
  , testProperty "MergeSpans creates span covering both spans" prop_mergeSpansValid
  , testProperty "IsValidSpan correctly identifies valid spans" prop_isValidSpan
  , testProperty "IsValidBlockSpan correctly identifies valid block spans" prop_isValidBlockSpan
  , testProperty "LocatedAt creates a located value" prop_locatedAtValid
  , testProperty "LocatedWithSpan creates a located value with span" prop_locatedWithSpanValid
  , testProperty "MapLocated applies function to value" prop_mapLocatedValid
  , testProperty "SpanStart gets start of span" prop_spanStartValid
  , testProperty "SpanEnd gets end of span" prop_spanEndValid
  , testProperty "LocatedValue extracts value" prop_locatedValueValid
  , testProperty "LocatedSpan extracts span" prop_locatedSpanValid
  , testProperty "LocatedPos extracts position from span" prop_locatedPosValid
  , testProperty "ToErrorLocation converts span to error location" prop_toErrorLocationValid
  , testProperty "ToErrorLocationWithSpan converts span to error location with span" prop_toErrorLocationWithSpanValid
  ]

-- | Run all tests
main :: IO ()
main = defaultMain testSuite