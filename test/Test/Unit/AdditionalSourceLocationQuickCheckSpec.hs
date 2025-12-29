{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.AdditionalSourceLocationQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose)
import TestSupport.Arbitrary

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  , posAfter
  , posAt
  , posAtLineCol
  , advancePos
  , advancePosBy
  , advancePosByText
  , advancePosByLine
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
  , toErrorLocation
  , toErrorLocationWithSpan
  )

import qualified Data.Text as T
import Data.Char (isSpace)

-- ============================================================================
-- Additional QuickCheck Tests for SourceLocation Module
-- ============================================================================

-- Property: posAfter newline behavior consistency
prop_posAfter_newline_consistency :: Int -> Int -> Property
prop_posAfter_newline_consistency line col =
  line > 0 && col > 0 ==> 
  let pos = SourcePos line col (line * 100 + col) -- Simple offset calculation
      newPos = posAfter '\n' pos
  in property $ posLine newPos === line + 1 .&&.
     posColumn newPos === 1 .&&.
     posOffset newPos === posOffset pos + 1

-- Property: posAfter tab behavior consistency
prop_posAfter_tab_consistency :: Int -> Int -> Property
prop_posAfter_tab_consistency line col =
  line > 0 && col > 0 && col <= 100 ==> 
  let pos = SourcePos line col (line * 100 + col)
      newPos = posAfter '\t' pos
      expectedCol = ((col - 1) `div` 8 + 1) * 8 + 1
  in property $ posLine newPos === line .&&.
     posColumn newPos === expectedCol .&&.
     posOffset newPos === posOffset pos + 1

-- Property: posAfter regular character behavior
prop_posAfter_regular_char_consistency :: Int -> Int -> Char -> Property
prop_posAfter_regular_char_consistency line col char =
  line > 0 && col > 0 && char `notElem` "\n\t" ==> 
  let pos = SourcePos line col (line * 100 + col)
      newPos = posAfter char pos
  in property $ posLine newPos === line .&&.
     posColumn newPos === col + 1 .&&.
     posOffset newPos === posOffset pos + 1

-- Property: advancePosBy consistency with repeated posAfter
prop_advancePosBy_consistency :: Int -> Int -> String -> Property
prop_advancePosBy_consistency line col chars =
  line > 0 && col > 0 ==> 
  let pos = SourcePos line col (line * 100 + col)
      advanced = advancePosBy chars pos
      manualAdvanced = foldl (flip posAfter) pos chars
  in property $ advanced === manualAdvanced

-- Property: advancePosByText consistency with advancePosBy
prop_advancePosByText_consistency :: Int -> Int -> String -> Property
prop_advancePosByText_consistency line col text =
  line > 0 && col > 0 ==> 
  let pos = SourcePos line col (line * 100 + col)
      textAdvanced = advancePosByText (T.pack text) pos
      stringAdvanced = advancePosBy text pos
  in property $ textAdvanced === stringAdvanced

-- Property: advancePosByLine consistency
prop_advancePosByLine_consistency :: Int -> Int -> Int -> Property
prop_advancePosByLine_consistency line col numLines =
  line > 0 && col > 0 && numLines >= 0 ==> 
  let pos = SourcePos line col (line * 100 + col)
      advanced = advancePosByLine numLines pos
  in property $ posLine advanced === line + numLines .&&.
     posColumn advanced === 1 .&&.
     posOffset advanced === posOffset pos

-- Property: spanBetween creates valid spans
prop_spanBetween_valid :: Int -> Int -> Int -> Int -> Property
prop_spanBetween_valid line1 col1 line2 col2 =
  line1 > 0 && col1 > 0 && line2 > 0 && col2 > 0 ==> 
  let start = SourcePos line1 col1 (line1 * 100 + col1)
      end = SourcePos line2 col2 (line2 * 100 + col2)
      span = spanBetween start end
  in property $ spanStart span === start .&&.
     spanEnd span === end

-- Property: mergeSpans commutativity
prop_mergeSpans_commutative :: Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_mergeSpans_commutative line1 col1 line2 col2 line3 col3 =
  all (>0) [line1, col1, line2, col2, line3, col3] ==> 
  let pos1 = SourcePos line1 col1 (line1 * 100 + col1)
      pos2 = SourcePos line2 col2 (line2 * 100 + col2)
      pos3 = SourcePos line3 col3 (line3 * 100 + col3)
      span1 = spanBetween pos1 pos2
      span2 = spanBetween pos2 pos3
      merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
  in property $ merged1 === merged2

-- Property: mergeSpans associativity
prop_mergeSpans_associative :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_mergeSpans_associative line1 col1 line2 col2 line3 col3 line4 col4 =
  all (>0) [line1, col1, line2, col2, line3, col3, line4, col4] ==> 
  let pos1 = SourcePos line1 col1 (line1 * 100 + col1)
      pos2 = SourcePos line2 col2 (line2 * 100 + col2)
      pos3 = SourcePos line3 col3 (line3 * 100 + col3)
      pos4 = SourcePos line4 col4 (line4 * 100 + col4)
      span1 = spanBetween pos1 pos2
      span2 = spanBetween pos2 pos3
      span3 = spanBetween pos3 pos4
      merged1 = mergeSpans (mergeSpans span1 span2) span3
      merged2 = mergeSpans span1 (mergeSpans span2 span3)
  in property $ merged1 === merged2

-- Property: isValidSpan correctly identifies valid spans
prop_isValidSpan_correct :: Int -> Int -> Int -> Int -> Property
prop_isValidSpan_correct line1 col1 line2 col2 =
  line1 > 0 && col1 > 0 && line2 > 0 && col2 > 0 ==> 
  let pos1 = SourcePos line1 col1 (line1 * 100 + col1)
      pos2 = SourcePos line2 col2 (line2 * 100 + col2)
      span = spanBetween pos1 pos2
      shouldBeValid = pos1 <= pos2
  in property $ isValidSpan span === shouldBeValid

-- Property: locatedAt creates span with same start and end
prop_locatedAt_span_consistency :: Int -> Int -> String -> Property
prop_locatedAt_span_consistency line col value =
  line > 0 && col > 0 ==> 
  let pos = SourcePos line col (line * 100 + col)
      located = locatedAt pos value
  in property $ locatedSpan located === emptySpan pos .&&.
     locatedPos located === pos .&&.
     locatedValue located === value

-- Property: mapLocated preserves location
prop_mapLocated_preserves_location :: Int -> Int -> Int -> Int -> String -> Property
prop_mapLocated_preserves_location line1 col1 line2 col2 value =
  all (>0) [line1, col1, line2, col2] ==> 
  let start = SourcePos line1 col1 (line1 * 100 + col1)
      end = SourcePos line2 col2 (line2 * 100 + col2)
      span = spanBetween start end
      located = locatedWithSpan span value
      mapped = mapLocated length located
  in property $ locatedSpan mapped === span .&&.
     locatedPos mapped === start .&&.
     locatedValue mapped === length value

-- Property: toErrorLocation preserves position information
prop_toErrorLocation_preserves_position :: Int -> Int -> Property
prop_toErrorLocation_preserves_position line col =
  line > 0 && col > 0 ==> 
  let pos = SourcePos line col (line * 100 + col)
      errLoc = toErrorLocation pos
  in property $ line errLoc === line .&&.
     column errLoc === col .&&.
     endLine errLoc === Nothing .&&.
     endColumn errLoc === Nothing

-- Property: toErrorLocationWithSpan preserves span information
prop_toErrorLocationWithSpan_preserves_span :: Int -> Int -> Int -> Int -> Property
prop_toErrorLocationWithSpan_preserves_span line1 col1 line2 col2 =
  all (>0) [line1, col1, line2, col2] ==> 
  let start = SourcePos line1 col1 (line1 * 100 + col1)
      end = SourcePos line2 col2 (line2 * 100 + col2)
      span = spanBetween start end
      errLoc = toErrorLocationWithSpan span
  in property $ line errLoc === line1 .&&.
     column errLoc === col1 .&&.
     endLine errLoc === Just line2 .&&.
     endColumn errLoc === Just col2

-- Property: posAt creates valid positions
prop_posAt_valid :: Int -> Int -> Property
prop_posAt_valid line col =
  line > 0 && col > 0 ==> 
  let pos = posAt line col
  in property $ posLine pos === line .&&.
     posColumn pos === col .&&.
     posOffset pos === 0

-- Property: posAtLineCol creates valid positions with offset
prop_posAtLineCol_valid :: Int -> Int -> Int -> Property
prop_posAtLineCol_valid line col offset =
  line > 0 && col > 0 && offset >= 0 ==> 
  let pos = posAtLineCol line col offset
  in property $ posLine pos === line .&&.
     posColumn pos === col .&&.
     posOffset pos === offset

-- Property: spanFrom and spanTo consistency
prop_spanFrom_spanTo_consistency :: Int -> Int -> Property
prop_spanFrom_spanTo_consistency line col =
  line > 0 && col > 0 ==> 
  let pos = SourcePos line col (line * 100 + col)
      spanFromPos = spanFrom pos
      spanToPos = spanTo pos
  in property $ spanFromPos === emptySpan pos .&&.
     spanToPos === emptySpan pos

-- Property: advancePosByText with empty text
prop_advancePosByText_empty :: Int -> Int -> Property
prop_advancePosByText_empty line col =
  line > 0 && col > 0 ==> 
  let pos = SourcePos line col (line * 100 + col)
      advanced = advancePosByText T.empty pos
  in property $ advanced === pos

-- Property: advancePosBy with empty string
prop_advancePosBy_empty :: Int -> Int -> Property
prop_advancePosBy_empty line col =
  line > 0 && col > 0 ==> 
  let pos = SourcePos line col (line * 100 + col)
      advanced = advancePosBy "" pos
  in property $ advanced === pos

-- Property: advancePosByLine with zero lines
prop_advancePosByLine_zero :: Int -> Int -> Property
prop_advancePosByLine_zero line col =
  line > 0 && col > 0 ==> 
  let pos = SourcePos line col (line * 100 + col)
      advanced = advancePosByLine 0 pos
  in property $ advanced === pos

-- Property: mergeSpans with identical spans
prop_mergeSpans_identical :: Int -> Int -> Int -> Int -> Property
prop_mergeSpans_identical line1 col1 line2 col2 =
  all (>0) [line1, col1, line2, col2] ==> 
  let start = SourcePos line1 col1 (line1 * 100 + col1)
      end = SourcePos line2 col2 (line2 * 100 + col2)
      span = spanBetween start end
      merged = mergeSpans span span
  in property $ merged === span

-- Property: Located functor laws
prop_located_functor_identity :: Int -> Int -> Int -> Int -> String -> Property
prop_located_functor_identity line1 col1 line2 col2 value =
  all (>0) [line1, col1, line2, col2] ==> 
  let start = SourcePos line1 col1 (line1 * 100 + col1)
      end = SourcePos line2 col2 (line2 * 100 + col2)
      span = spanBetween start end
      located = locatedWithSpan span value
      mapped = mapLocated id located
  in property $ mapped === located

-- Property: Located functor composition
prop_located_functor_composition :: Int -> Int -> Int -> Int -> String -> Property
prop_located_functor_composition line1 col1 line2 col2 value =
  all (>0) [line1, col1, line2, col2] ==> 
  let start = SourcePos line1 col1 (line1 * 100 + col1)
      end = SourcePos line2 col2 (line2 * 100 + col2)
      span = spanBetween start end
      located = locatedWithSpan span value
      f = length
      g = (*2)
      mapped1 = mapLocated (f . g) located
      mapped2 = mapLocated f (mapLocated g located)
  in property $ mapped1 === mapped2

-- Property: Complex position advancement with mixed characters
prop_complex_position_advancement :: Int -> Int -> String -> Property
prop_complex_position_advancement line col text =
  line > 0 && col > 0 ==> 
  let pos = SourcePos line col (line * 100 + col)
      -- Count newlines and tabs in text
      newlineCount = length $ filter (== '\n') text
      tabCount = length $ filter (== '\t') text
      otherCount = length text - newlineCount - tabCount
      advanced = advancePosBy text pos
  in property $ posLine advanced === line + newlineCount .&&.
     posOffset advanced === posOffset pos + length text

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "Additional SourceLocation QuickCheck Tests"
  [ fastProperty "posAfter newline behavior consistency" prop_posAfter_newline_consistency
  , fastProperty "posAfter tab behavior consistency" prop_posAfter_tab_consistency
  , fastProperty "posAfter regular character behavior" prop_posAfter_regular_char_consistency
  , fastProperty "advancePosBy consistency with repeated posAfter" prop_advancePosBy_consistency
  , fastProperty "advancePosByText consistency with advancePosBy" prop_advancePosByText_consistency
  , fastProperty "advancePosByLine consistency" prop_advancePosByLine_consistency
  , fastProperty "spanBetween creates valid spans" prop_spanBetween_valid
  , fastProperty "mergeSpans commutativity" prop_mergeSpans_commutative
  , fastProperty "mergeSpans associativity" prop_mergeSpans_associative
  , fastProperty "isValidSpan correctly identifies valid spans" prop_isValidSpan_correct
  , fastProperty "locatedAt creates span with same start and end" prop_locatedAt_span_consistency
  , fastProperty "mapLocated preserves location" prop_mapLocated_preserves_location
  , fastProperty "toErrorLocation preserves position information" prop_toErrorLocation_preserves_position
  , fastProperty "toErrorLocationWithSpan preserves span information" prop_toErrorLocationWithSpan_preserves_span
  , fastProperty "posAt creates valid positions" prop_posAt_valid
  , fastProperty "posAtLineCol creates valid positions with offset" prop_posAtLineCol_valid
  , fastProperty "spanFrom and spanTo consistency" prop_spanFrom_spanTo_consistency
  , fastProperty "advancePosByText with empty text" prop_advancePosByText_empty
  , fastProperty "advancePosBy with empty string" prop_advancePosBy_empty
  , fastProperty "advancePosByLine with zero lines" prop_advancePosByLine_zero
  , fastProperty "mergeSpans with identical spans" prop_mergeSpans_identical
  , fastProperty "Located functor identity" prop_located_functor_identity
  , fastProperty "Located functor composition" prop_located_functor_composition
  , fastProperty "Complex position advancement with mixed characters" prop_complex_position_advancement
  ]