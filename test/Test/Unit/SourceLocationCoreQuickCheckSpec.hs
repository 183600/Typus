{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.SourceLocationCoreQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Positive(..), NonNegative(..))

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
  , toErrorLocation
  , toErrorLocationWithSpan
  )

import Data.Word (Word32)

-- Property: SourcePos ordering is consistent with line and column numbers
prop_sourcepos_ordering_consistent :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> Property
prop_sourcepos_ordering_consistent (Positive line1) (Positive col1) (Positive line2) (Positive col2) =
  let pos1 = SourcePos (fromIntegral line1) (fromIntegral col1)
      pos2 = SourcePos (fromIntegral line2) (fromIntegral col2)
      lineComparison = compare line1 line2
      colComparison = compare col1 col2
      posComparison = compare pos1 pos2
  in classify (line1 < line2) "different lines" $
     classify (line1 == line2 && col1 < col2) "same line, different columns" $
     classify (line1 == line2 && col1 == col2) "same position" $
     property $ 
       if line1 /= line2 
       then posComparison === lineComparison
       else posComparison === colComparison

-- Property: posAfter moves to next character in same line
prop_posAfter_moves_to_next_char :: Positive Int -> Positive Int -> Property
prop_posAfter_moves_to_next_char (Positive line) (Positive col) =
  let pos = SourcePos (fromIntegral line) (fromIntegral col)
      nextPos = posAfter pos
  in classify (col == maxBound) "at column limit" $
     property $ 
       if col < maxBound
       then posLine nextPos === fromIntegral line .&&. posCol nextPos === fromIntegral col + 1
       else nextPos === pos

-- Property: posAtLineCol creates correct position
prop_posAtLineCol_creates_correct_position :: Positive Int -> Positive Int -> Property
prop_posAtLineCol_creates_correct_position (Positive line) (Positive col) =
  let pos = posAtLineCol (fromIntegral line) (fromIntegral col)
  in property $ posLine pos === fromIntegral line .&&. posCol pos === fromIntegral col

-- Property: emptySpan has start and end at same position
prop_emptySpan_same_position :: Positive Int -> Positive Int -> Property
prop_emptySpan_same_position (Positive line) (Positive col) =
  let pos = SourcePos (fromIntegral line) (fromIntegral col)
      span = emptySpan pos
  in property $ spanStart span === pos .&&. spanEnd span === pos

-- Property: spanFrom creates span from start position
prop_spanFrom_creates_span_from_start :: Positive Int -> Positive Int -> Property
prop_spanFrom_creates_span_from_start (Positive line) (Positive col) =
  let start = SourcePos (fromIntegral line) (fromIntegral col)
      span = spanFrom start
  in property $ spanStart span === start .&&. spanEnd span === start

-- Property: spanTo creates span ending at position
prop_spanTo_creates_span_to_end :: Positive Int -> Positive Int -> Property
prop_spanTo_creates_span_to_end (Positive line) (Positive col) =
  let end = SourcePos (fromIntegral line) (fromIntegral col)
      span = spanTo end
  in property $ spanStart span === end .&&. spanEnd span === end

-- Property: spanBetween creates correct span
prop_spanBetween_creates_correct_span :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> Property
prop_spanBetween_creates_correct_span (Positive line1) (Positive col1) (Positive line2) (Positive col2) =
  let start = SourcePos (fromIntegral line1) (fromIntegral col1)
      end = SourcePos (fromIntegral line2) (fromIntegral col2)
      span = spanBetween start end
  in property $ spanStart span === start .&&. spanEnd span === end

-- Property: isValidSpan correctly validates spans
prop_isValidSpan_validates_spans :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> Property
prop_isValidSpan_validates_spans (Positive line1) (Positive col1) (Positive line2) (Positive col2) =
  let start = SourcePos (fromIntegral line1) (fromIntegral col1)
      end = SourcePos (fromIntegral line2) (fromIntegral col2)
      span = spanBetween start end
      shouldBeValid = line1 < line2 || (line1 == line2 && col1 <= col2)
  in classify shouldBeValid "should be valid" $
     classify (not shouldBeValid) "should be invalid" $
     property $ isValidSpan span === shouldBeValid

-- Property: locatedAt creates located value at position
prop_locatedAt_creates_located_value :: Positive Int -> Positive Int -> String -> Property
prop_locatedAt_creates_located_value (Positive line) (Positive col) value =
  let pos = SourcePos (fromIntegral line) (fromIntegral col)
      located = locatedAt pos value
      span = emptySpan pos
  in property $ locatedValue located === value .&&. locatedSpan located === span

-- Property: locatedWithSpan creates located value with span
prop_locatedWithSpan_creates_located_with_span :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> String -> Property
prop_locatedWithSpan_creates_located_with_span (Positive line1) (Positive col1) (Positive line2) (Positive col2) value =
  let start = SourcePos (fromIntegral line1) (fromIntegral col1)
      end = SourcePos (fromIntegral line2) (fromIntegral col2)
      span = spanBetween start end
      located = locatedWithSpan span value
  in property $ locatedValue located === value .&&. locatedSpan located === span

-- Property: mapLocated preserves span but transforms value
prop_mapLocated_preserves_span :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> String -> Property
prop_mapLocated_preserves_span (Positive line1) (Positive col1) (Positive line2) (Positive col2) value =
  let start = SourcePos (fromIntegral line1) (fromIntegral col1)
      end = SourcePos (fromIntegral line2) (fromIntegral col2)
      span = spanBetween start end
      located = locatedWithSpan span value
      transformed = mapLocated (reverse . map toUpper) located
  in property $ locatedSpan transformed === span .&&. locatedValue transformed === reverse (map toUpper value)

-- Property: advancePos moves position correctly
prop_advancePos_moves_correctly :: Positive Int -> Positive Int -> Char -> Property
prop_advancePos_moves_correctly (Positive line) (Positive col) ch =
  let pos = SourcePos (fromIntegral line) (fromIntegral col)
      advanced = advancePos pos ch
  in classify (ch == '\n') "newline" $
     classify (ch /= '\n') "non-newline" $
     property $ 
       if ch == '\n'
       then posLine advanced === fromIntegral line + 1 .&&. posCol advanced === 1
       else posLine advanced === fromIntegral line .&&. posCol advanced === fromIntegral col + 1

-- Property: advancePosBy moves position by multiple characters
prop_advancePosBy_moves_by_multiple :: Positive Int -> Positive Int -> String -> Property
prop_advancePosBy_moves_by_multiple (Positive line) (Positive col) text =
  let pos = SourcePos (fromIntegral line) (fromIntegral col)
      advanced = advancePosBy pos text
      newlineCount = length $ filter (== '\n') text
      lastLineLength = length $ takeWhile (/= '\n') $ reverse text
  in classify (null text) "empty string" $
     classify (newlineCount > 0) "has newlines" $
     classify (newlineCount == 0) "no newlines" $
     property $ 
       if null text
       then advanced === pos
       else if newlineCount > 0
            then posLine advanced === fromIntegral line + fromIntegral newlineCount .&&. posCol advanced === fromIntegral lastLineLength + 1
            else posLine advanced === fromIntegral line .&&. posCol advanced === fromIntegral col + length text

-- Helper function for toUpper conversion
toUpper :: Char -> Char
toUpper c
  | 'a' <= c && c <= 'z' = toEnum (fromEnum c - fromEnum 'a' + fromEnum 'A')
  | otherwise = c

tests :: TestTree
tests =
  testGroup "SourceLocation Core QuickCheck Tests"
    [ fastProperty "SourcePos ordering is consistent" prop_sourcepos_ordering_consistent
    , fastProperty "posAfter moves to next character" prop_posAfter_moves_to_next_char
    , fastProperty "posAtLineCol creates correct position" prop_posAtLineCol_creates_correct_position
    , fastProperty "emptySpan has same start and end" prop_emptySpan_same_position
    , fastProperty "spanFrom creates span from start" prop_spanFrom_creates_span_from_start
    , fastProperty "spanTo creates span to end" prop_spanTo_creates_span_to_end
    , fastProperty "spanBetween creates correct span" prop_spanBetween_creates_correct_span
    , fastProperty "isValidSpan validates spans correctly" prop_isValidSpan_validates_spans
    , fastProperty "locatedAt creates located value at position" prop_locatedAt_creates_located_value
    , fastProperty "locatedWithSpan creates located value with span" prop_locatedWithSpan_creates_located_with_span
    , fastProperty "mapLocated preserves span" prop_mapLocated_preserves_span
    , fastProperty "advancePos moves position correctly" prop_advancePos_moves_correctly
    , fastProperty "advancePosBy moves by multiple characters" prop_advancePosBy_moves_by_multiple
    ]