{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.SourceLocationAdditionalQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose, suchThat)
import Data.Char (isSpace)
import qualified Data.Text as T

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
  , spanBetween
  , mergeSpans
  , isValidSpan
  , locatedAt
  , locatedWithSpan
  , locatedValue
  , locatedPos
  , mapLocated
  , advancePos
  , advancePosBy
  , advancePosByText
  , advancePosByLine
  , toErrorLocation
  , toErrorLocationWithSpan
  , spanStart
  , spanEnd
  )

import Compiler.Errors.Core (ErrorLocation(..))

-- Arbitrary instances for SourceLocation types
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 1000)
    column <- choose (1, 1000)
    offset <- choose (0, 10000)
    return $ SourcePos line column offset

instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    -- Ensure end is not before start
    lineOffset <- choose (0, 100)
    columnOffset <- choose (0, 100)
    let endLine = posLine start + lineOffset
        endColumn = if lineOffset == 0 then posColumn start + columnOffset else columnOffset + 1
        endOffset = posOffset start + choose (0, 1000)
    return $ SourceSpan start (SourcePos endLine endColumn endOffset)

instance Arbitrary a => Arbitrary (Located a) where
  arbitrary = do
    value <- arbitrary
    span <- arbitrary
    return $ locatedWithSpan span value

-- Property: posAfter '\n' increments line L.and resets column
prop_posAfter_newline :: SourcePos -> Property
prop_posAfter_newline pos =
  let result = posAfter '\n' pos
  in posLine result === posLine pos + 1 .&&.
     posColumn result === 1 .&&.
     posOffset result === posOffset pos + 1

-- Property: posAfter '\t' aligns to next tab stop (8-column)
prop_posAfter_tab :: SourcePos -> Property
prop_posAfter_tab pos =
  let result = posAfter '\t' pos
      expectedColumn = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in posColumn result === expectedColumn .&&.
     posOffset result === posOffset pos + 1

-- Property: posAfter regular character increments column L.and offset
prop_posAfter_regular :: Char -> SourcePos -> Property
prop_posAfter_regular c pos =
  c /= '\n' && c /= '\t' ==>
  let result = posAfter c pos
  in posColumn result === posColumn pos + 1 .&&.
     posOffset result === posOffset pos + 1

-- Property: spanBetween creates valid span
prop_spanBetween_valid :: SourcePos -> SourcePos -> Property
prop_spanBetween_valid p1 p2 =
  let span = spanBetween p1 p2
  in spanStart span === min p1 p2 .&&.
     spanEnd span === max p1 p2

-- Property: mergeSpans is commutative
prop_mergeSpans_commutative :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_commutative span1 span2 =
  let merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
  in merged1 === merged2

-- Property: mergeSpans is associative
prop_mergeSpans_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_mergeSpans_associative span1 span2 span3 =
  let merged1 = mergeSpans (mergeSpans span1 span2) span3
      merged2 = mergeSpans span1 (mergeSpans span2 span3)
  in merged1 === merged2

-- Property: empty span is valid
prop_emptySpan_valid :: SourcePos -> Property
prop_emptySpan_valid pos =
  let span = emptySpan pos
  in isValidSpan span === True

-- Property: spanFrom creates empty span
prop_spanFrom_empty :: SourcePos -> Property
prop_spanFrom_empty pos =
  let span = spanFrom pos
  in spanStart span === spanEnd span

-- Property: locatedAt creates located value with correct position
prop_locatedAt_correct :: SourcePos -> Int -> Property
prop_locatedAt_correct pos value =
  let located = locatedAt pos value
  in locatedValue located === value .&&.
     locatedPos located === pos

-- Property: mapLocated preserves location
prop_mapLocated_preserves_location :: SourceSpan -> String -> Property
prop_mapLocated_preserves_location span str =
  let original = locatedWithSpan span str
      transformed = mapLocated L.length original
  in locatedSpan transformed === locatedSpan original

-- Property: advancePosBy is consistent with repeated advancePos
prop_advancePosBy_consistent :: String -> SourcePos -> Property
prop_advancePosBy_consistent chars pos =
  let advancedBy = advancePosBy chars pos
      advancedRepeated = L.foldl (flip advancePos) pos chars
  in advancedBy === advancedRepeated

-- Property: advancePosByText consistent with advancePosBy
prop_advancePosByText_consistent :: String -> SourcePos -> Property
prop_advancePosByText_consistent str pos =
  let byText = advancePosByText (T.pack str) pos
      byString = advancePosBy str pos
  in byText === byString

-- Property: advancePosByLine only changes line L.and resets column
prop_advancePosByLine_only_changes_line :: SourcePos -> Int -> Property
prop_advancePosByLine_only_changes_line pos numLines =
  let result = advancePosByLine numLines pos
  in posLine result === posLine pos + numLines .&&.
     posColumn result === 1

-- Property: toErrorLocation preserves line L.and column
prop_toErrorLocation_preserves :: SourcePos -> Property
prop_toErrorLocation_preserves pos =
  let errorLoc = toErrorLocation pos
  in line errorLoc === posLine pos .&&.
     column errorLoc === posColumn pos .&&.
     endLine errorLoc === Nothing .&&.
     endColumn errorLoc === Nothing

-- Property: toErrorLocationWithSpan preserves span information
prop_toErrorLocationWithSpan_preserves :: SourceSpan -> Property
prop_toErrorLocationWithSpan_preserves span =
  let errorLoc = toErrorLocationWithSpan span
      start = spanStart span
      end = spanEnd span
  in line errorLoc === posLine start .&&.
     column errorLoc === posColumn start .&&.
     endLine errorLoc === Just (posLine end) .&&.
     endColumn errorLoc === Just (posColumn end)

-- Property: isValidSpan checks start <= end
prop_isValidSpan_correct :: SourceSpan -> Property
prop_isValidSpan_correct span =
  let start = spanStart span
      end = spanEnd span
  in isValidSpan span === (start <= end)

tests :: TestTree
tests =
  testGroup "Additional SourceLocation QuickCheck tests"
    [ fastProperty "posAfter newline increments line" prop_posAfter_newline
    , fastProperty "posAfter tab aligns correctly" prop_posAfter_tab
    , fastProperty "posAfter regular character" prop_posAfter_regular
    , fastProperty "spanBetween creates valid span" prop_spanBetween_valid
    , fastProperty "mergeSpans commutative" prop_mergeSpans_commutative
    , fastProperty "mergeSpans associative" prop_mergeSpans_associative
    , fastProperty "emptySpan is valid" prop_emptySpan_valid
    , fastProperty "spanFrom creates empty span" prop_spanFrom_empty
    , fastProperty "locatedAt creates correct located value" prop_locatedAt_correct
    , fastProperty "mapLocated preserves location" prop_mapLocated_preserves_location
    , fastProperty "advancePosBy consistent with advancePos" prop_advancePosBy_consistent
    , fastProperty "advancePosByText consistent with advancePosBy" prop_advancePosByText_consistent
    , fastProperty "advancePosByLine only changes line" prop_advancePosByLine_only_changes_line
    , fastProperty "toErrorLocation preserves position" prop_toErrorLocation_preserves
    , fastProperty "toErrorLocationWithSpan preserves span" prop_toErrorLocationWithSpan_preserves
    , fastProperty "isValidSpan checks correctly" prop_isValidSpan_correct
    ]