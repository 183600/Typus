{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.Unit.NewSourceLocationTrackingQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

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
import Compiler.Errors.Core (ErrorLocation(..))
import Data.Char (isSpace)

-- | 新的源码位置跟踪QuickCheck测试套件
tests :: TestTree
tests =
  testGroup "New Source Location Tracking QuickCheck Tests"
    [ fastProperty "posAfter correctly advances position for newline" prop_posAfter_newline
    , fastProperty "posAfter correctly advances position for tab" prop_posAfter_tab
    , fastProperty "posAfter correctly advances position for regular character" prop_posAfter_regular
    , fastProperty "spanBetween creates valid span" prop_spanBetween_valid
    , fastProperty "mergeSpans contains both original spans" prop_mergeSpans_contains
    , fastProperty "locatedAt creates located value with correct position" prop_locatedAt_position
    , fastProperty "advancePosBy advances correctly for multiple characters" prop_advancePosBy_multiple
    , fastProperty "advancePosByText advances correctly for text" prop_advancePosByText_correct
    , fastProperty "toErrorLocationWithSpan preserves span information" prop_toErrorLocationWithSpan_preserves
    , fastProperty "mapLocated preserves location" prop_mapLocated_preserves_location
    ]

-- Property: posAfter correctly advances position for newline
prop_posAfter_newline :: Int -> Int -> Property
prop_posAfter_newline line col =
  line > 0 && col > 0 && line <= 1000 && col <= 1000 ==>
  let pos = posAt line col
      newPos = posAfter '\n' pos
  in property $ posLine newPos === line + 1 .&&.
     posColumn newPos === 1 .&&.
     posOffset newPos === posOffset pos + 1

-- Property: posAfter correctly advances position for tab
prop_posAfter_tab :: Int -> Int -> Property
prop_posAfter_tab line col =
  line > 0 && col > 0 && line <= 1000 && col <= 1000 ==>
  let pos = posAt line col
      newPos = posAfter '\t' pos
      expectedCol = ((col - 1) `div` 8 + 1) * 8 + 1
  in property $ posLine newPos === line .&&.
     posColumn newPos === expectedCol .&&.
     posOffset newPos === posOffset pos + 1

-- Property: posAfter correctly advances position for regular character
prop_posAfter_regular :: Int -> Int -> Char -> Property
prop_posAfter_regular line col char =
  line > 0 && col > 0 && line <= 1000 && col <= 1000 &&
  char /= '\n' && char /= '\t' ==>
  let pos = posAt line col
      newPos = posAfter char pos
  in property $ posLine newPos === line .&&.
     posColumn newPos === col + 1 .&&.
     posOffset newPos === posOffset pos + 1

-- Property: spanBetween creates valid span
prop_spanBetween_valid :: Int -> Int -> Int -> Int -> Int -> Property
prop_spanBetween_valid startLine startCol endLine endCol offset =
  startLine > 0 && startCol > 0 && endLine > 0 && endCol > 0 &&
  startLine <= 1000 && startCol <= 1000 && endLine <= 1000 && endCol <= 1000 &&
  offset >= 0 && offset <= 1000 ==>
  let startPos = posAtLineCol startLine startCol offset
      endPos = posAtLineCol endLine endCol (offset + abs (endLine - startLine) + abs (endCol - startCol))
      span = spanBetween startPos endPos
  in property $ spanStart span === startPos .&&.
     spanEnd span === endPos .&&.
     isValidSpan span

-- Property: mergeSpans contains both original spans
prop_mergeSpans_contains :: Int -> Int -> Int -> Int -> Property
prop_mergeSpans_contains line1 col1 line2 col2 =
  line1 > 0 && col1 > 0 && line2 > 0 && col2 > 0 &&
  line1 <= 1000 && col1 <= 1000 && line2 <= 1000 && col2 <= 1000 ==>
  let pos1 = posAt line1 col1
      pos2 = posAt line2 col2
      span1 = emptySpan pos1
      span2 = emptySpan pos2
      merged = mergeSpans span1 span2
  in property $ spanStart merged <= spanStart span1 .&&.
     spanStart merged <= spanStart span2 .&&.
     spanEnd merged >= spanEnd span1 .&&.
     spanEnd merged >= spanEnd span2

-- Property: locatedAt creates located value with correct position
prop_locatedAt_position :: Int -> Int -> String -> Property
prop_locatedAt_position line col value =
  line > 0 && col > 0 && line <= 1000 && col <= 1000 ==>
  let pos = posAt line col
      located = locatedAt pos value
  in property $ locatedValue located === value .&&.
     locatedPos located === pos .&&.
     spanStart (locatedSpan located) === pos .&&.
     spanEnd (locatedSpan located) === pos

-- Property: advancePosBy advances correctly for multiple characters
prop_advancePosBy_multiple :: Int -> Int -> String -> Property
prop_advancePosBy_multiple line col chars =
  line > 0 && col > 0 && line <= 1000 && col <= 1000 &&
  length chars <= 100 ==>
  let pos = posAt line col
      advanced = advancePosBy chars pos
      expectedOffset = posOffset pos + length chars
  in property $ posOffset advanced === expectedOffset .&&.
     (if '\n' `elem` chars 
      then posLine advanced > line 
      else posLine advanced === line)

-- Property: advancePosByText advances correctly for text
prop_advancePosByText_correct :: Int -> Int -> String -> Property
prop_advancePosByText_correct line col text =
  line > 0 && col > 0 && line <= 1000 && col <= 1000 &&
  length text <= 100 ==>
  let pos = posAt line col
      textObj = T.pack text
      advanced = advancePosByText textObj pos
      expectedOffset = posOffset pos + length text
  in property $ posOffset advanced === expectedOffset

-- Property: toErrorLocationWithSpan preserves span information
prop_toErrorLocationWithSpan_preserves :: Int -> Int -> Int -> Int -> Property
prop_toErrorLocationWithSpan_preserves startLine startCol endLine endCol =
  startLine > 0 && startCol > 0 && endLine > 0 && endCol > 0 &&
  startLine <= 1000 && startCol <= 1000 && endLine <= 1000 && endCol <= 1000 ==>
  let startPos = posAt startLine startCol
      endPos = posAt endLine endCol
      span = spanBetween startPos endPos
      errorLoc = toErrorLocationWithSpan span
  in property $ line errorLoc === startLine .&&.
     column errorLoc === startCol .&&.
     endLine errorLoc === Just endLine .&&.
     endColumn errorLoc === Just endCol

-- Property: mapLocated preserves location
prop_mapLocated_preserves_location :: Int -> Int -> String -> Property
prop_mapLocated_preserves_location line col value =
  line > 0 && col > 0 && line <= 1000 && col <= 1000 ==>
  let pos = posAt line col
      located = locatedAt pos value
      transformed = mapLocated (++ "_suffix") located
  in property $ locatedPos transformed === locatedPos located .&&.
     locatedSpan transformed === locatedSpan located .&&.
     locatedValue transformed === value ++ "_suffix"

-- Additional properties for source location tracking

-- Property: advancePosByLine advances by correct number of lines
prop_advancePosByLine_correct :: Int -> Int -> Int -> Property
prop_advancePosByLine_correct line col numLines =
  line > 0 && col > 0 && numLines >= 0 && numLines <= 100 &&
  line <= 1000 && col <= 1000 ==>
  let pos = posAt line col
      advanced = advancePosByLine numLines pos
  in property $ posLine advanced === line + numLines .&&.
     posColumn advanced === 1

-- Property: spanFrom creates span with same start and end
prop_spanFrom_same_start_end :: Int -> Int -> Property
prop_spanFrom_same_start_end line col =
  line > 0 && col > 0 && line <= 1000 && col <= 1000 ==>
  let pos = posAt line col
      span = spanFrom pos
  in property $ spanStart span === pos .&&.
     spanEnd span === pos

-- Property: locatedWithSpan creates located value with correct span
prop_locatedWithSpan_correct_span :: Int -> Int -> Int -> Int -> String -> Property
prop_locatedWithSpan_correct_span startLine startCol endLine endCol value =
  startLine > 0 && startCol > 0 && endLine > 0 && endCol > 0 &&
  startLine <= 1000 && startCol <= 1000 && endLine <= 1000 && endCol <= 1000 ==>
  let startPos = posAt startLine startCol
      endPos = posAt endLine endCol
      span = spanBetween startPos endPos
      located = locatedWithSpan span value
  in property $ locatedValue located === value .&&.
     locatedSpan located === span .&&.
     locatedPos located === startPos

-- Property: isValidSpan correctly identifies valid spans
prop_isValidSpan_correct :: Int -> Int -> Int -> Int -> Property
prop_isValidSpan_correct startLine startCol endLine endCol =
  startLine > 0 && startCol > 0 && endLine > 0 && endCol > 0 &&
  startLine <= 1000 && startCol <= 1000 && endLine <= 1000 && endCol <= 1000 ==>
  let startPos = posAt startLine startCol
      endPos = posAt endLine endCol
      span = spanBetween startPos endPos
      reversedSpan = spanBetween endPos startPos
  in property $ isValidSpan span .&&. 
     (if startPos <= endPos then isValidSpan span else not (isValidSpan span)) .&&.
     (if startPos <= endPos then not (isValidSpan reversedSpan) else isValidSpan reversedSpan)

-- Property: toErrorLocation correctly converts position
prop_toErrorLocation_correct :: Int -> Int -> Property
prop_toErrorLocation_correct line col =
  line > 0 && col > 0 && line <= 1000 && col <= 1000 ==>
  let pos = posAt line col
      errorLoc = toErrorLocation pos
  in property $ line errorLoc === line .&&.
     column errorLoc === col .&&.
     endLine errorLoc === Nothing .&&.
     endColumn errorLoc === Nothing

-- Property: HasLocation instance works correctly
prop_hasLocation_correct :: Int -> Int -> Int -> Int -> String -> Property
prop_hasLocation_correct startLine startCol endLine endCol value =
  startLine > 0 && startCol > 0 && endLine > 0 && endCol > 0 &&
  startLine <= 1000 && startCol <= 1000 && endLine <= 1000 && endCol <= 1000 ==>
  let startPos = posAt startLine startCol
      endPos = posAt endLine endCol
      span = spanBetween startPos endPos
      located = locatedWithSpan span value
  in property $ getLocation located === span

-- Property: Position ordering is consistent with offset
prop_position_ordering :: Int -> Int -> Int -> Int -> Property
prop_position_ordering line1 col1 line2 col2 =
  line1 > 0 && col1 > 0 && line2 > 0 && col2 > 0 &&
  line1 <= 1000 && col1 <= 1000 && line2 <= 1000 && col2 <= 1000 ==>
  let pos1 = posAtLineCol line1 col1 (line1 * 1000 + col1)
      pos2 = posAtLineCol line2 col2 (line2 * 1000 + col2)
  in if posOffset pos1 < posOffset pos2
     then property $ pos1 < pos2
     else if posOffset pos1 > posOffset pos2
          then property $ pos1 > pos2
          else property $ pos1 === pos2

-- Property: advancePos with mixed characters
prop_advancePos_mixed :: Int -> Int -> String -> Property
prop_advancePos_mixed line col chars =
  line > 0 && col > 0 && line <= 1000 && col <= 1000 &&
  length chars <= 50 ==>
  let pos = posAt line col
      finalPos = foldl (flip advancePos) pos chars
      manualAdvance = advancePosBy chars pos
  in property $ finalPos === manualAdvance