{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewSourceLocationQuickCheckTestsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose)
import Test.QuickCheck.Gen (Gen(..), vectorOf)

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

import Compiler.Errors.Core (ErrorLocation(..))

import Data.Char (isSpace)
import qualified Data.Text as T
import Data.Text (Text)

-- ============================================================================
-- Arbitrary instances
-- ============================================================================

instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 1000)
    column <- choose (1, 1000)
    offset <- choose (0, 10000)
    return $ SourcePos line column offset

instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    endOffset <- choose (0, 100)
    let end = start { posOffset = posOffset start + endOffset }
    return $ SourceSpan start end

instance Arbitrary a => Arbitrary (Located a) where
  arbitrary = do
    pos <- arbitrary
    value <- arbitrary
    return $ Located pos value

-- Generate reasonable character for position advancement
validChar :: Gen Char
validChar = oneof
  [ return 'a'  -- Regular character
  , return '\n' -- Newline
  , return '\t' -- Tab
  , return ' '  -- Space
  , elements ['b'..'z']  -- Other letters
  ]

-- Generate string for position advancement
validString :: Gen String
validString = listOf validChar

-- Generate text for position advancement
validText :: Gen Text
validText = T.pack <$> validString

-- ============================================================================
-- Source Position Property Tests
-- ============================================================================

-- Property: startPos has correct initial values
prop_startPos_values :: Property
prop_startPos_values =
  property $ posLine startPos === 1 .&&.
               posColumn startPos === 1 .&&.
               posOffset startPos === 0

-- Property: posAfter handles newline correctly
prop_posAfter_newline :: SourcePos -> Property
prop_posAfter_newline pos =
  let result = posAfter '\n' pos
  in property $ posLine result === posLine pos + 1 .&&.
               posColumn result === 1 .&&.
               posOffset result === posOffset pos + 1

-- Property: posAfter handles tab correctly (8-space tab width)
prop_posAfter_tab :: SourcePos -> Property
prop_posAfter_tab pos =
  let result = posAfter '\t' pos
      expectedColumn = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in property $ posLine result === posLine pos .&&.
               posColumn result === expectedColumn .&&.
               posOffset result === posOffset pos + 1

-- Property: posAfter handles regular character correctly
prop_posAfter_regular :: SourcePos -> Char -> Property
prop_posAfter_regular pos char =
  char /= '\n' && char /= '\t' ==>
  let result = posAfter char pos
  in property $ posLine result === posLine pos .&&.
               posColumn result === posColumn pos + 1 .&&.
               posOffset result === posOffset pos + 1

-- Property: posAt creates position with correct line and column
prop_posAt_correct :: Int -> Int -> Property
prop_posAt_correct line col =
  line >= 1 && col >= 1 ==>
  let pos = posAt line col
  in property $ posLine pos === line .&&.
               posColumn pos === col .&&.
               posOffset pos === 0

-- Property: posAtLineCol creates position with all fields
prop_posAtLineCol_correct :: Int -> Int -> Int -> Property
prop_posAtLineCol_correct line col offset =
  line >= 1 && col >= 1 && offset >= 0 ==>
  let pos = posAtLineCol line col offset
  in property $ posLine pos === line .&&.
               posColumn pos === col .&&.
               posOffset pos === offset

-- ============================================================================
-- Source Span Property Tests
-- ============================================================================

-- Property: emptySpan has zero length
prop_emptySpan_zero_length :: Property
prop_emptySpan_zero_length =
  property $ posOffset (spanEnd emptySpan) === posOffset (spanStart emptySpan)

-- Property: spanFrom creates span from position
prop_spanFrom_creates :: SourcePos -> Property
prop_spanFrom_creates pos =
  let span = spanFrom pos
  in property $ spanStart span === pos .&&.
               spanEnd span === pos

-- Property: spanTo creates span to position
prop_spanTo_creates :: SourcePos -> Property
prop_spanTo_creates pos =
  let span = spanTo pos
  in property $ spanStart span === pos .&&.
               spanEnd span === pos

-- Property: spanBetween creates span between positions
prop_spanBetween_correct :: SourcePos -> SourcePos -> Property
prop_spanBetween_correct start end =
  posOffset end >= posOffset start ==>
  let span = spanBetween start end
  in property $ spanStart span === start .&&.
               spanEnd span === end

-- Property: mergeSpans creates span covering both spans
prop_mergeSpans_correct :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_correct span1 span2 =
  let merged = mergeSpans span1 span2
      start1 = spanStart span1
      end1 = spanEnd span1
      start2 = spanStart span2
      end2 = spanEnd span2
      expectedStart = if posOffset start1 <= posOffset start2 then start1 else start2
      expectedEnd = if posOffset end1 >= posOffset end2 then end1 else end2
  in property $ spanStart merged === expectedStart .&&.
               spanEnd merged === expectedEnd

-- Property: isValidSpan checks span validity
prop_isValidSpan_valid :: SourceSpan -> Property
prop_isValidSpan_valid span =
  let start = spanStart span
      end = spanEnd span
      valid = isValidSpan span
  in property $ valid === (posOffset end >= posOffset start)

-- ============================================================================
-- Located Value Property Tests
-- ============================================================================

-- Property: locatedAt creates located value
prop_locatedAt_creates :: SourcePos -> Int -> Property
prop_locatedAt_creates pos value =
  let located = locatedAt pos value
  in property $ locatedPos located === pos .&&.
               locatedValue located === value

-- Property: locatedWithSpan creates located value with span
prop_locatedWithSpan_creates :: SourceSpan -> String -> Property
prop_locatedWithSpan_creates span value =
  let located = locatedWithSpan span value
  in property $ locatedSpan located === span .&&.
               locatedValue located === value

-- Property: mapLocated transforms located value
prop_mapLocated_transforms :: SourcePos -> Int -> Property
prop_mapLocated_transforms pos value =
  let located = locatedAt pos value
      transformed = mapLocated (*2) located
  in property $ locatedPos transformed === pos .&&.
               locatedValue transformed === value * 2

-- ============================================================================
-- Position Advancement Property Tests
-- ============================================================================

-- Property: advancePos is same as posAfter
prop_advancePos_equals_posAfter :: SourcePos -> Char -> Property
prop_advancePos_equals_posAfter pos char =
  advancePos char pos === posAfter char pos

-- Property: advancePosBy advances by multiple characters
prop_advancePosBy_multiple :: SourcePos -> String -> Property
prop_advancePosBy_multiple pos chars =
  let result = advancePosBy chars pos
      expected = foldl (flip posAfter) pos chars
  in result === expected

-- Property: advancePosByText advances by text content
prop_advancePosByText_text :: SourcePos -> Text -> Property
prop_advancePosByText_text pos text =
  let result = advancePosByText text pos
      expected = advancePosBy (T.unpack text) pos
  in result === expected

-- Property: advancePosByLine advances line number
prop_advancePosByLine_correct :: SourcePos -> Int -> Property
prop_advancePosByLine_correct pos numLines =
  let result = advancePosByLine numLines pos
  in property $ posLine result === posLine pos + numLines .&&.
               posColumn result === 1 .&&.
               posOffset result === posOffset pos

-- ============================================================================
-- Error Location Conversion Property Tests
-- ============================================================================

-- Property: toErrorLocation converts position correctly
prop_toErrorLocation_correct :: SourcePos -> Property
prop_toErrorLocation_correct pos =
  let errorLoc = toErrorLocation pos
  in property $ filePath errorLoc === Nothing .&&.
               line errorLoc === posLine pos .&&.
               column errorLoc === posColumn pos .&&.
               endLine errorLoc === Nothing .&&.
               endColumn errorLoc === Nothing

-- Property: toErrorLocationWithSpan converts span correctly
prop_toErrorLocationWithSpan_correct :: SourceSpan -> Property
prop_toErrorLocationWithSpan_correct span =
  let errorLoc = toErrorLocationWithSpan span
      start = spanStart span
      end = spanEnd span
  in property $ filePath errorLoc === Nothing .&&.
               line errorLoc === posLine start .&&.
               column errorLoc === posColumn start .&&.
               endLine errorLoc === Just (posLine end) .&&.
               endColumn errorLoc === Just (posColumn end)

-- ============================================================================
-- Advanced Property Tests
-- ============================================================================

-- Property: Position advancement is consistent
prop_advancePos_consistency :: SourcePos -> String -> String -> Property
prop_advancePos_consistency pos str1 str2 =
  let combined = str1 ++ str2
      result1 = advancePosBy str1 pos
      result2 = advancePosBy str2 result1
      resultCombined = advancePosBy combined pos
  in result2 === resultCombined

-- Property: Span merging is associative
prop_mergeSpans_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_mergeSpans_associative span1 span2 span3 =
  let merge12 = mergeSpans span1 span2
      merge23 = mergeSpans span2 span3
      result1 = mergeSpans merge12 span3
      result2 = mergeSpans span1 merge23
  in result1 === result2

-- Property: Span merging is commutative for overlapping spans
prop_mergeSpans_commutative_overlap :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_commutative_overlap span1 span2 =
  let start1 = spanStart span1
      end1 = spanEnd span1
      start2 = spanStart span2
      end2 = spanEnd span2
      overlapping = posOffset start1 <= posOffset end2 && posOffset end1 >= posOffset start2
  in overlapping ===>
     let merge1 = mergeSpans span1 span2
         merge2 = mergeSpans span2 span1
     in merge1 === merge2

-- Property: Located value mapping preserves location
prop_mapLocated_preserves_location :: SourcePos -> String -> Property
prop_mapLocated_preserves_location pos value =
  let located = locatedAt pos value
      transformed = mapLocated reverse located
  in locatedPos transformed === locatedPos located

-- Property: Position advancement with empty string
prop_advancePosBy_empty :: SourcePos -> Property
prop_advancePosBy_empty pos =
  advancePosBy "" pos === pos

-- Property: Text advancement with empty text
prop_advancePosByText_empty :: SourcePos -> Property
prop_advancePosByText_empty pos =
  advancePosByText T.empty pos === pos

-- Property: Line advancement by zero
prop_advancePosByLine_zero :: SourcePos -> Property
prop_advancePosByLine_zero pos =
  advancePosByLine 0 pos === pos

-- Property: Tab advancement respects 8-space tab width
prop_tab_alignment :: SourcePos -> Property
prop_tab_alignment pos =
  let result = posAfter '\t' pos
      column = posColumn result
  in property $ column `mod` 8 === 1 .||. column === 1

-- Property: Newline resets column to 1
prop_newline_resets_column :: SourcePos -> Property
prop_newline_resets_column pos =
  let result = posAfter '\n' pos
  in property $ posColumn result === 1

-- Property: Position ordering by offset
prop_position_ordering :: SourcePos -> SourcePos -> Property
prop_position_ordering pos1 pos2 =
  pos1 <= pos2 === (posOffset pos1 <= posOffset pos2)

-- Property: Span length calculation
prop_span_length :: SourceSpan -> Property
prop_span_length span =
  let start = spanStart span
      end = spanEnd span
      expectedLength = max 0 (posOffset end - posOffset start)
  in posOffset end >= posOffset start ==> 
     property $ posOffset end - posOffset start === expectedLength

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New SourceLocation QuickCheck Tests"
  [ fastProperty "startPos has correct initial values" prop_startPos_values
  , fastProperty "posAfter handles newline correctly" prop_posAfter_newline
  , fastProperty "posAfter handles tab correctly" prop_posAfter_tab
  , fastProperty "posAfter handles regular character correctly" prop_posAfter_regular
  , fastProperty "posAt creates position with correct line and column" prop_posAt_correct
  , fastProperty "posAtLineCol creates position with all fields" prop_posAtLineCol_correct
  , fastProperty "emptySpan has zero length" prop_emptySpan_zero_length
  , fastProperty "spanFrom creates span from position" prop_spanFrom_creates
  , fastProperty "spanTo creates span to position" prop_spanTo_creates
  , fastProperty "spanBetween creates span between positions" prop_spanBetween_correct
  , fastProperty "mergeSpans creates span covering both spans" prop_mergeSpans_correct
  , fastProperty "isValidSpan checks span validity" prop_isValidSpan_valid
  , fastProperty "locatedAt creates located value" prop_locatedAt_creates
  , fastProperty "locatedWithSpan creates located value with span" prop_locatedWithSpan_creates
  , fastProperty "mapLocated transforms located value" prop_mapLocated_transforms
  , fastProperty "advancePos is same as posAfter" prop_advancePos_equals_posAfter
  , fastProperty "advancePosBy advances by multiple characters" prop_advancePosBy_multiple
  , fastProperty "advancePosByText advances by text content" prop_advancePosByText_text
  , fastProperty "advancePosByLine advances line number" prop_advancePosByLine_correct
  , fastProperty "toErrorLocation converts position correctly" prop_toErrorLocation_correct
  , fastProperty "toErrorLocationWithSpan converts span correctly" prop_toErrorLocationWithSpan_correct
  , fastProperty "Position advancement is consistent" prop_advancePos_consistency
  , fastProperty "Span merging is associative" prop_mergeSpans_associative
  , fastProperty "Span merging is commutative for overlapping spans" prop_mergeSpans_commutative_overlap
  , fastProperty "Located value mapping preserves location" prop_mapLocated_preserves_location
  , fastProperty "Position advancement with empty string" prop_advancePosBy_empty
  , fastProperty "Text advancement with empty text" prop_advancePosByText_empty
  , fastProperty "Line advancement by zero" prop_advancePosByLine_zero
  , fastProperty "Tab advancement respects 8-space tab width" prop_tab_alignment
  , fastProperty "Newline resets column to 1" prop_newline_resets_column
  , fastProperty "Position ordering by offset" prop_position_ordering
  , fastProperty "Span length calculation" prop_span_length
  ]