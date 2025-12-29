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
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, oneof, elements)
import qualified Test.QuickCheck as QC

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
  , runLocationTracker
  , getCurrentPos
  , setCurrentPos
  , markSpanStart
  , markSpanEnd
  , advancePos
  , advancePosBy
  , advancePosByText
  , advancePosByLine
  , toErrorLocation
  , toErrorLocationWithSpan
  )

-- Arbitrary instances for SourceLocation types
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 1000)
    col <- choose (1, 1000)
    offset <- choose (0, 10000)
    pure $ SourcePos line col offset

instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    pure $ spanBetween start end

-- ============================================================================
-- Generators for SourceLocation data types
-- ============================================================================

-- Generate positive integers for positions
genPositiveInt :: Gen Int
genPositiveInt = choose (1, 1000)

-- Generate non-negative integers for offsets
genNonNegativeInt :: Gen Int
genNonNegativeInt = choose (0, 10000)

-- Generate source position
genSourcePos :: Gen SourcePos
genSourcePos = do
  line <- genPositiveInt
  column <- genPositiveInt
  offset <- genNonNegativeInt
  pure $ SourcePos line column offset

-- Generate source span
genSourceSpan :: Gen SourceSpan
genSourceSpan = do
  start <- genSourcePos
  end <- genSourcePos
  pure $ SourceSpan start end

-- Generate valid source span (where start <= end)
genValidSourceSpan :: Gen SourceSpan
genValidSourceSpan = do
  line1 <- genPositiveInt
  line2 <- choose (line1, line1 + 100)
  column1 <- genPositiveInt
  column2 <- if line2 == line1 then choose (column1, column1 + 100) else genPositiveInt
  offset1 <- genNonNegativeInt
  offset2 <- choose (offset1, offset1 + 1000)
  let start = SourcePos line1 column1 offset1
      end = SourcePos line2 column2 offset2
  pure $ SourceSpan start end

-- Generate located value
genLocated :: Gen a -> Gen (Located a)
genLocated genA = do
  value <- genA
  pos <- genSourcePos
  span <- genValidSourceSpan
  pure $ Located value pos span

-- Generate character for position advancement
genChar :: Gen Char
genChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n\r!@#$%^&*()_+-=[]{}|;':\",./<>?"

-- Generate string for position advancement
genString :: Gen String
genString = listOf genChar

-- ============================================================================
-- Property-based tests for SourceLocation module
-- ============================================================================

-- Property: startPos has correct values
prop_startPos :: Property
prop_startPos =
  startPos === SourcePos 1 1 0

-- Property: posAfter handles newline correctly
prop_posAfter_newline :: SourcePos -> Property
prop_posAfter_newline pos =
  let result = posAfter '\n' pos
      expectedLine = posLine pos + 1
      expectedColumn = 1
      expectedOffset = posOffset pos + 1
  in property $ posLine result === expectedLine .&&.
             posColumn result === expectedColumn .&&.
             posOffset result === expectedOffset

-- Property: posAfter handles tab correctly (8-space tab width)
prop_posAfter_tab :: SourcePos -> Property
prop_posAfter_tab pos =
  let result = posAfter '\t' pos
      expectedColumn = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
      expectedOffset = posOffset pos + 1
  in property $ posLine result === posLine pos .&&.
             posColumn result === expectedColumn .&&.
             posOffset result === expectedOffset

-- Property: posAfter handles regular characters correctly
prop_posAfter_regular :: Char -> SourcePos -> Property
prop_posAfter_regular char pos =
  char /= '\n' && char /= '\t' ==>
  let result = posAfter char pos
      expectedColumn = posColumn pos + 1
      expectedOffset = posOffset pos + 1
  in property $ posLine result === posLine pos .&&.
             posColumn result === expectedColumn .&&.
             posOffset result === expectedOffset

-- Property: posAt creates correct position
prop_posAt :: Int -> Int -> Property
prop_posAt line col =
  line > 0 && col > 0 ==>
  let result = posAt line col
  in property $ posLine result === line .&&.
             posColumn result === col .&&.
             posOffset result === 0

-- Property: posAtLineCol creates correct position
prop_posAtLineCol :: Int -> Int -> Int -> Property
prop_posAtLineCol line col offset =
  line > 0 && col > 0 && offset >= 0 ==>
  let result = posAtLineCol line col offset
  in property $ posLine result === line .&&.
             posColumn result === col .&&.
             posOffset result === offset

-- Property: emptySpan creates span with same start and end
prop_emptySpan :: SourcePos -> Property
prop_emptySpan pos =
  let result = emptySpan pos
  in property $ spanStart result === pos .&&. spanEnd result === pos

-- Property: spanFrom creates empty span at position
prop_spanFrom :: SourcePos -> Property
prop_spanFrom pos =
  let result = spanFrom pos
  in property $ spanStart result === pos .&&. spanEnd result === pos

-- Property: spanTo creates empty span at position
prop_spanTo :: SourcePos -> Property
prop_spanTo pos =
  let result = spanTo pos
  in property $ spanStart result === pos .&&. spanEnd result === pos

-- Property: spanBetween creates correct span
prop_spanBetween :: SourcePos -> SourcePos -> Property
prop_spanBetween start end =
  let result = spanBetween start end
  in property $ spanStart result === start .&&. spanEnd result === end

-- Property: mergeSpans contains both original spans
prop_mergeSpans :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans span1 span2 =
  let result = mergeSpans span1 span2
      start1 = spanStart span1
      end1 = spanEnd span1
      start2 = spanStart span2
      end2 = spanEnd span2
      resultStart = spanStart result
      resultEnd = spanEnd result
  in property $ resultStart <= start1 .&&. resultStart <= start2 .&&.
             resultEnd >= end1 .&&. resultEnd >= end2

-- Property: mergeSpans is commutative
prop_mergeSpans_commutative :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_commutative span1 span2 =
  mergeSpans span1 span2 === mergeSpans span2 span1

-- Property: mergeSpans is associative
prop_mergeSpans_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_mergeSpans_associative span1 span2 span3 =
  mergeSpans span1 (mergeSpans span2 span3) === mergeSpans (mergeSpans span1 span2) span3

-- Property: isValidSpan correctly identifies valid spans
prop_isValidSpan :: SourcePos -> SourcePos -> Property
prop_isValidSpan start end =
  let span = spanBetween start end
      expected = start <= end
  in property $ isValidSpan span === expected

-- Property: locatedAt creates located value with correct position
prop_locatedAt :: SourcePos -> Int -> Property
prop_locatedAt pos value =
  let result = locatedAt pos value
  in property $ locValue result === value .&&.
             locPos result === pos .&&.
             spanStart (locSpan result) === pos .&&.
             spanEnd (locSpan result) === pos

-- Property: locatedWithSpan creates located value with correct span
prop_locatedWithSpan :: SourceSpan -> String -> Property
prop_locatedWithSpan span value =
  let result = locatedWithSpan span value
  in property $ locValue result === value .&&.
             locSpan result === span .&&.
             locPos result === spanStart span

-- Property: locatedValue extracts correct value
prop_locatedValue :: Int -> SourcePos -> SourceSpan -> Property
prop_locatedValue value pos span =
  let located = Located value pos span
  in property $ locatedValue located === value

-- Property: locatedSpan extracts correct span
prop_locatedSpan :: Int -> SourcePos -> SourceSpan -> Property
prop_locatedSpan value pos span =
  let located = Located value pos span
  in property $ locatedSpan located === span

-- Property: locatedPos extracts correct position
prop_locatedPos :: Int -> SourcePos -> SourceSpan -> Property
prop_locatedPos value pos span =
  let located = Located value pos span
  in property $ locatedPos located === pos

-- Property: mapLocated applies function correctly
prop_mapLocated :: Int -> SourcePos -> SourceSpan -> Property
prop_mapLocated value pos span =
  let located = Located value pos span
      result = mapLocated (*2) located
  in property $ locValue result === value * 2 .&&.
             locPos result === pos .&&.
             locSpan result === span

-- Property: runLocationTracker starts at startPos
prop_runLocationTracker :: Property
prop_runLocationTracker =
  let result = runLocationTracker getCurrentPos
  in property $ result === startPos

-- Property: advancePos advances correctly for newline
prop_advancePos_newline :: SourcePos -> Property
prop_advancePos_newline pos =
  let result = advancePos '\n' pos
  in property $ result === posAfter '\n' pos

-- Property: advancePos advances correctly for tab
prop_advancePos_tab :: SourcePos -> Property
prop_advancePos_tab pos =
  let result = advancePos '\t' pos
  in property $ result === posAfter '\t' pos

-- Property: advancePos advances correctly for regular characters
prop_advancePos_regular :: Char -> SourcePos -> Property
prop_advancePos_regular char pos =
  char /= '\n' && char /= '\t' ==>
  let result = advancePos char pos
  in property $ result === posAfter char pos

-- Property: advancePosBy advances correctly for empty string
prop_advancePosBy_empty :: SourcePos -> Property
prop_advancePosBy_empty pos =
  let result = advancePosBy "" pos
  in property $ result === pos

-- Property: advancePosBy advances correctly for multiple characters
prop_advancePosBy_multiple :: String -> SourcePos -> Property
prop_advancePosBy_multiple chars pos =
  let result = advancePosBy chars pos
      expected = foldl (flip advancePos) pos chars
  in property $ result === expected

-- Property: advancePosBy handles empty string
prop_advancePosBy_empty :: Property
prop_advancePosBy_empty =
  let pos = SourcePos 1 1 0
      newPos = advancePosBy "" pos
  in newPos === pos

-- Property: advancePosBy handles strings
prop_advancePosBy_strings :: Property
prop_advancePosBy_strings =
  forAll genString $ \str ->
    let pos = SourcePos 1 1 0
      newPos = advancePosBy str pos
    in property $ posOffset newPos >= posOffset pos

-- Property: advancePosByLine advances line correctly
prop_advancePosByLine :: Int -> SourcePos -> Property
prop_advancePosByLine numLines pos =
  numLines >= 0 ==>
  let result = advancePosByLine numLines pos
      expectedLine = posLine pos + numLines
      expectedColumn = 1
  in property $ posLine result === expectedLine .&&.
             posColumn result === expectedColumn .&&.
             posOffset result === posOffset pos

-- Property: toErrorLocation converts position correctly
prop_toErrorLocation :: SourcePos -> Property
prop_toErrorLocation pos =
  let result = toErrorLocation pos
  in property $ case result of
    ErrorLocation {..} -> line === posLine pos .&&.
                         column === posColumn pos .&&.
                         filePath === Nothing .&&.
                         endLine === Nothing .&&.
                         endColumn === Nothing

-- Property: toErrorLocationWithSpan converts span correctly
prop_toErrorLocationWithSpan :: SourceSpan -> Property
prop_toErrorLocationWithSpan span =
  let start = spanStart span
      end = spanEnd span
      result = toErrorLocationWithSpan span
  in property $ case result of
    ErrorLocation {..} -> line === posLine start .&&.
                         column === posColumn start .&&.
                         endLine === Just (posLine end) .&&.
                         endColumn === Just (posColumn end)

-- Property: position advancement is consistent
prop_advancePos_consistency :: Char -> SourcePos -> Property
prop_advancePos_consistency char pos =
  let result1 = advancePos char pos
      result2 = posAfter char pos
  in property $ result1 === result2

-- Property: span merging preserves validity
prop_mergeSpans_preserves_validity :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_preserves_validity span1 span2 =
  let result = mergeSpans span1 span2
  in property $ isValidSpan result

-- Property: located values preserve span validity
prop_located_preserves_validity :: SourceSpan -> Int -> Property
prop_located_preserves_validity span value =
  let located = locatedWithSpan span value
  in property $ isValidSpan (locSpan located)

-- ============================================================================
-- Test suite
-- ============================================================================

tests :: TestTree
tests = testGroup "New Cabal SourceLocation QuickCheck Tests"
  [ fastProperty "startPos has correct values" prop_startPos
  , fastProperty "posAfter handles newline correctly" prop_posAfter_newline
  , fastProperty "posAfter handles tab correctly" prop_posAfter_tab
  , fastProperty "posAfter handles regular characters correctly" prop_posAfter_regular
  , fastProperty "posAt creates correct position" prop_posAt
  , fastProperty "posAtLineCol creates correct position" prop_posAtLineCol
  , fastProperty "emptySpan creates span with same start and end" prop_emptySpan
  , fastProperty "spanFrom creates empty span at position" prop_spanFrom
  , fastProperty "spanTo creates empty span at position" prop_spanTo
  , fastProperty "spanBetween creates correct span" prop_spanBetween
  , fastProperty "mergeSpans contains both original spans" prop_mergeSpans
  , fastProperty "mergeSpans is commutative" prop_mergeSpans_commutative
  , fastProperty "mergeSpans is associative" prop_mergeSpans_associative
  , fastProperty "isValidSpan correctly identifies valid spans" prop_isValidSpan
  , fastProperty "locatedAt creates located value with correct position" prop_locatedAt
  , fastProperty "locatedWithSpan creates located value with correct span" prop_locatedWithSpan
  , fastProperty "locatedValue extracts correct value" prop_locatedValue
  , fastProperty "locatedSpan extracts correct span" prop_locatedSpan
  , fastProperty "locatedPos extracts correct position" prop_locatedPos
  , fastProperty "mapLocated applies function correctly" prop_mapLocated
  , fastProperty "runLocationTracker starts at startPos" prop_runLocationTracker
  , fastProperty "advancePos advances correctly for newline" prop_advancePos_newline
  , fastProperty "advancePos advances correctly for tab" prop_advancePos_tab
  , fastProperty "advancePos advances correctly for regular characters" prop_advancePos_regular
  , fastProperty "advancePosBy advances correctly for empty string" prop_advancePosBy_empty
  , fastProperty "advancePosBy advances correctly for multiple characters" prop_advancePosBy_multiple
  , fastProperty "advancePosByText advances correctly for empty text" prop_advancePosByText_empty
  , fastProperty "advancePosByText advances correctly for text" prop_advancePosByText_text
  , fastProperty "advancePosByLine advances line correctly" prop_advancePosByLine
  , fastProperty "toErrorLocation converts position correctly" prop_toErrorLocation
  , fastProperty "toErrorLocationWithSpan converts span correctly" prop_toErrorLocationWithSpan
  , fastProperty "position advancement is consistent" prop_advancePos_consistency
  , fastProperty "span merging preserves validity" prop_mergeSpans_preserves_validity
  , fastProperty "located values preserve span validity" prop_located_preserves_validity
  ]