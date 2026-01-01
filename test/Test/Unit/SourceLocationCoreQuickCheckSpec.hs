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
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf, elements, oneof, suchThat)

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
  , toErrorLocation
  , toErrorLocationWithSpan
  )

import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import Compiler.Errors.Core (ErrorLocation(..))

-- ============================================================================
-- Generators for QuickCheck
-- ============================================================================

-- Generate a valid line number (1-100)
genLine :: Gen Int
genLine = choose (1, 100)

-- Generate a valid column number (1-200)
genColumn :: Gen Int
genColumn = choose (1, 200)

-- Generate a valid offset (0-10000)
genOffset :: Gen Int
genOffset = choose (0, 10000)

-- Generate a source position
genSourcePos :: Gen SourcePos
genSourcePos = do
  line <- genLine
  column <- genColumn
  offset <- genOffset
  return $ SourcePos line column offset

-- Generate a source position with valid offset based on line L.and column
genValidSourcePos :: Gen SourcePos
genValidSourcePos = do
  line <- genLine
  column <- genColumn
  -- Approximate offset based on line L.and column (assuming ~80 chars per line)
  let offset = (line - 1) * 80 + (column - 1)
  return $ SourcePos line column offset

-- Generate a source span
genSourceSpan :: Gen SourceSpan
genSourceSpan = do
  start <- genValidSourcePos
  endOffset <- choose (0, 500)
  let end = start { posOffset = posOffset start + endOffset, posColumn = posColumn start + endOffset }
  return $ SourceSpan start end

-- Generate a valid source span (where start <= end)
genValidSourceSpan :: Gen SourceSpan
genValidSourceSpan = do
  startLine <- genLine
  startColumn <- genColumn
  endLine <- choose (startLine, startLine + 10)
  endColumn <- if endLine == startLine 
               then choose (startColumn, startColumn + 100)
               else genColumn
  let start = SourcePos startLine startColumn ((startLine - 1) * 80 + (startColumn - 1))
      end = SourcePos endLine endColumn ((endLine - 1) * 80 + (endColumn - 1))
  return $ SourceSpan start end

-- Generate a character for position advancement
genChar :: Gen Char
genChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n.,;:!?()[]{}+-*/=<>'\""

-- Generate a string for position advancement
genString :: Gen String
genString = listOf genChar

-- Generate text for position advancement
genText :: Gen Text
genText = T.pack <$> genString

-- ============================================================================
-- SourcePos Properties
-- ============================================================================

-- Property: startPos is at line 1, column 1, offset 0
prop_startPos_properties :: Property
prop_startPos_properties =
  posLine startPos === 1 .&&.
  posColumn startPos === 1 .&&.
  posOffset startPos === 0

-- Property: posAfter advances line number for newline
prop_posAfter_newline :: SourcePos -> Property
prop_posAfter_newline pos =
  let newPos = posAfter '\n' pos
  in posLine newPos === posLine pos + 1 .&&.
     posColumn newPos === 1 .&&.
     posOffset newPos === posOffset pos + 1

-- Property: posAfter advances column for regular characters
prop_posAfter_regular_char :: SourcePos -> Char -> Property
prop_posAfter_regular_char pos char =
  char /= '\n' && char /= '\t' ==>
  let newPos = posAfter char pos
  in posLine newPos === posLine pos .&&.
     posColumn newPos === posColumn pos + 1 .&&.
     posOffset newPos === posOffset pos + 1

-- Property: posAfter handles tab correctly (rounds up to next 8-column boundary)
prop_posAfter_tab :: SourcePos -> Property
prop_posAfter_tab pos =
  let newPos = posAfter '\t' pos
      expectedColumn = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in posLine newPos === posLine pos .&&.
     posColumn newPos === expectedColumn .&&.
     posOffset newPos === posOffset pos + 1

-- Property: posAt creates position with correct line L.and column
prop_posAt_correct :: Int -> Int -> Property
prop_posAt_correct line col =
  line > 0 && col > 0 ==>
  let pos = posAt line col
  in posLine pos === line .&&. posColumn pos === col .&&. posOffset pos === 0

-- Property: posAtLineCol creates position with correct line, column, L.and offset
prop_posAtLineCol_correct :: Int -> Int -> Int -> Property
prop_posAtLineCol_correct line col offset =
  line > 0 && col > 0 && offset >= 0 ==>
  let pos = posAtLineCol line col offset
  in posLine pos === line .&&. posColumn pos === col .&&. posOffset pos === offset

-- Property: advancePos is same as posAfter
prop_advancePos_equals_posAfter :: SourcePos -> Char -> Property
prop_advancePos_equals_posAfter pos char =
  advancePos char pos === posAfter char pos

-- Property: advancePosBy advances position correctly for empty string
prop_advancePosBy_empty :: SourcePos -> Property
prop_advancePosBy_empty pos =
  advancePosBy "" pos === pos

-- Property: advancePosBy is consistent with repeated advancePos
prop_advancePosBy_consistent :: SourcePos -> String -> Property
prop_advancePosBy_consistent pos str =
  let advanced = advancePosBy str pos
      manualAdv = L.foldl (flip advancePos) pos str
  in advanced === manualAdv

-- Property: advancePosByText is consistent with advancePosBy
prop_advancePosByText_consistent :: SourcePos -> Text -> Property
prop_advancePosByText_consistent pos text =
  advancePosByText text pos === advancePosBy (T.unpack text) pos

-- Property: advancePosByLine advances line number L.and resets column
prop_advancePosByLine_correct :: SourcePos -> Int -> Property
prop_advancePosByLine_correct pos numLines =
  numLines >= 0 ==>
  let newPos = advancePosByLine numLines pos
  in posLine newPos === posLine pos + numLines .&&.
     posColumn newPos === 1 .&&.
     posOffset newPos === posOffset pos + numLines

-- ============================================================================
-- SourceSpan Properties
-- ============================================================================

-- Property: emptySpan creates span with same start L.and end
prop_emptySpan_properties :: SourcePos -> Property
prop_emptySpan_properties pos =
  let span = emptySpan pos
  in spanStart span === pos .&&. spanEnd span === pos

-- Property: spanFrom creates empty span at position
prop_spanFrom_equals_emptySpan :: SourcePos -> Property
prop_spanFrom_equals_emptySpan pos =
  spanFrom pos === emptySpan pos

-- Property: spanTo creates span with same start L.and end
prop_spanTo_properties :: SourcePos -> Property
prop_spanTo_properties pos =
  let span = spanTo pos
  in spanStart span === pos .&&. spanEnd span === pos

-- Property: spanBetween creates span with correct start L.and end
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

-- Property: mergeSpans is commutative
prop_mergeSpans_commutative :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_commutative span1 span2 =
  mergeSpans span1 span2 === mergeSpans span2 span1

-- Property: mergeSpans is associative
prop_mergeSpans_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_mergeSpans_associative span1 span2 span3 =
  mergeSpans span1 (mergeSpans span2 span3) === mergeSpans (mergeSpans span1 span2) span3

-- Property: isValidSpan returns True for valid spans
prop_isValidSpan_valid :: SourceSpan -> Property
prop_isValidSpan_valid span =
  spanStart span <= spanEnd span ==> isValidSpan span === True

-- Property: isValidSpan returns False for invalid spans
prop_isValidSpan_invalid :: SourcePos -> SourcePos -> Property
prop_isValidSpan_invalid start end =
  start > end ==> isValidSpan (SourceSpan start end) === False

-- ============================================================================
-- Located Properties
-- ============================================================================

-- Property: locatedAt creates located value with correct position L.and span
prop_locatedAt_correct :: SourcePos -> Int -> Property
prop_locatedAt_correct pos value =
  let located = locatedAt pos value
      expectedSpan = emptySpan pos
  in locValue located === value .&&.
     locPos located === pos .&&.
     locSpan located === expectedSpan

-- Property: locatedWithSpan creates located value with correct span
prop_locatedWithSpan_correct :: SourceSpan -> String -> Property
prop_locatedWithSpan_correct span value =
  let located = locatedWithSpan span value
  in locValue located === value .&&.
     locSpan located === span .&&.
     locPos located === spanStart span

-- Property: locatedValue extracts the value
prop_locatedValue_correct :: SourcePos -> String -> Property
prop_locatedValue_correct pos value =
  locatedValue (locatedAt pos value) === value

-- Property: locatedSpan extracts the span
prop_locatedSpan_correct :: SourcePos -> Int -> Property
prop_locatedSpan_correct pos value =
  let expectedSpan = emptySpan pos
  in locatedSpan (locatedAt pos value) === expectedSpan

-- Property: locatedPos extracts the starting position
prop_locatedPos_correct :: SourcePos -> Int -> Property
prop_locatedPos_correct pos value =
  locatedPos (locatedAt pos value) === pos

-- Property: mapLocated applies function to value
prop_mapLocated_correct :: SourcePos -> Int -> Property
prop_mapLocated_correct pos value =
  let located = locatedAt pos value
      mapped = mapLocated (*2) located
  in locValue mapped === value * 2 .&&.
     locPos mapped === pos .&&.
     locSpan mapped === locSpan located

-- ============================================================================
-- Error Location Conversion Properties
-- ============================================================================

-- Property: toErrorLocation creates correct error location
prop_toErrorLocation_correct :: SourcePos -> Property
prop_toErrorLocation pos =
  let errLoc = toErrorLocation pos
  in filePath errLoc === Nothing .&&.
     line errLoc === posLine pos .&&.
     column errLoc === posColumn pos .&&.
     endLine errLoc === Nothing .&&.
     endColumn errLoc === Nothing

-- Property: toErrorLocationWithSpan creates correct error location with range
prop_toErrorLocationWithSpan_correct :: SourceSpan -> Property
prop_toErrorLocationWithSpan_correct span =
  let errLoc = toErrorLocationWithSpan span
      start = spanStart span
      end = spanEnd span
  in filePath errLoc === Nothing .&&.
     line errLoc === posLine start .&&.
     column errLoc === posColumn start .&&.
     endLine errLoc === Just (posLine end) .&&.
     endColumn errLoc === Just (posColumn end)

-- ============================================================================
-- Advanced Properties
-- ============================================================================

-- Property: Position advancement preserves ordering
prop_advancePos_preserves_ordering :: SourcePos -> String -> Property
prop_advancePos_preserves_ordering pos str =
  let advanced = advancePosBy str pos
  in posOffset advanced >= posOffset pos

-- Property: Span merging contains original spans
prop_mergeSpans_contains_originals :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_contains_originals span1 span2 =
  let merged = mergeSpans span1 span2
  in spanStart merged <= spanStart span1 .&&.
     spanEnd merged >= spanEnd span1 .&&.
     spanStart merged <= spanStart span2 .&&.
     spanEnd merged >= spanEnd span2

-- Property: Located values maintain structure through mapping
prop_mapLocated_preserves_structure :: SourcePos -> [Int] -> Property
prop_mapLocated_preserves_structure pos values =
  let locateds = L.map (`locatedAt` pos) values
      mapped = L.map (mapLocated (*2)) locateds
  in conjoin [locPos (mapped !! i) === pos | i <- [0..L.length values-1]] .&&.
     conjoin [locSpan (mapped !! i) === locSpan (locateds !! i) | i <- [0..L.length values-1]]

-- Property: Complex position advancement scenario
prop_complex_position_advancement :: SourcePos -> String -> String -> String -> Property
prop_complex_position_advancement pos str1 str2 str3 =
  let pos1 = advancePosBy str1 pos
      pos2 = advancePosBy str2 pos1
      pos3 = advancePosBy str3 pos2
      directPos = advancePosBy (str1 ++ str2 ++ str3) pos
  in pos3 === directPos

-- Property: Span creation L.and manipulation consistency
prop_span_manipulation_consistency :: SourcePos -> SourcePos -> Property
prop_span_manipulation_consistency start end =
  let span1 = spanBetween start end
      span2 = spanBetween end start
      merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
  in merged1 === merged2

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "SourceLocation Core QuickCheck Tests"
  [ testGroup "SourcePos Properties"
    [ fastProperty "startPos has correct values" prop_startPos_properties
    , fastProperty "posAfter advances line for newline" prop_posAfter_newline
    , fastProperty "posAfter advances column for regular chars" prop_posAfter_regular_char
    , fastProperty "posAfter handles tab correctly" prop_posAfter_tab
    , fastProperty "posAt creates correct position" prop_posAt_correct
    , fastProperty "posAtLineCol creates correct position" prop_posAtLineCol_correct
    , fastProperty "advancePos equals posAfter" prop_advancePos_equals_posAfter
    , fastProperty "advancePosBy handles empty string" prop_advancePosBy_empty
    , fastProperty "advancePosBy is consistent" prop_advancePosBy_consistent
    , fastProperty "advancePosByText is consistent" prop_advancePosByText_consistent
    , fastProperty "advancePosByLine advances correctly" prop_advancePosByLine_correct
    ]

  , testGroup "SourceSpan Properties"
    [ fastProperty "emptySpan has same start L.and end" prop_emptySpan_properties
    , fastProperty "spanFrom equals emptySpan" prop_spanFrom_equals_emptySpan
    , fastProperty "spanTo has same start L.and end" prop_spanTo_properties
    , fastProperty "spanBetween creates correct span" prop_spanBetween_correct
    , fastProperty "mergeSpans creates covering span" prop_mergeSpans_correct
    , fastProperty "mergeSpans is commutative" prop_mergeSpans_commutative
    , fastProperty "mergeSpans is associative" prop_mergeSpans_associative
    , fastProperty "isValidSpan for valid spans" prop_isValidSpan_valid
    , fastProperty "isValidSpan for invalid spans" prop_isValidSpan_invalid
    ]

  , testGroup "Located Properties"
    [ fastProperty "locatedAt creates correct located value" prop_locatedAt_correct
    , fastProperty "locatedWithSpan creates correct located value" prop_locatedWithSpan_correct
    , fastProperty "locatedValue extracts value" prop_locatedValue_correct
    , fastProperty "locatedSpan extracts span" prop_locatedSpan_correct
    , fastProperty "locatedPos extracts position" prop_locatedPos_correct
    , fastProperty "mapLocated applies function" prop_mapLocated_correct
    ]

  , testGroup "Error Location Conversion Properties"
    [ fastProperty "toErrorLocation creates correct error location" prop_toErrorLocation_correct
    , fastProperty "toErrorLocationWithSpan creates correct error location" prop_toErrorLocationWithSpan_correct
    ]

  , testGroup "Advanced Properties"
    [ fastProperty "advancePos preserves ordering" prop_advancePos_preserves_ordering
    , fastProperty "mergeSpans contains originals" prop_mergeSpans_contains_originals
    , fastProperty "mapLocated preserves structure" prop_mapLocated_preserves_structure
    , fastProperty "complex position advancement" prop_complex_position_advancement
    , fastProperty "span manipulation consistency" prop_span_manipulation_consistency
    ]
  ]