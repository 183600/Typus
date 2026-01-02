{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.EnhancedSourceLocationPropertiesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements)
import TestSupport.Arbitrary

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
import Data.Char (isSpace)

-- Property: startPos is always (1, 1, 0)
prop_startPos_constant :: Property
prop_startPos_constant =
  property $ startPos === SourcePos 1 1 0

-- Property: posAfter newline increments line L.and resets column
prop_posAfter_newline :: SourcePos -> Property
prop_posAfter_newline pos =
  let newPos = posAfter '\n' pos
  in property $ posLine newPos === posLine pos + 1 .&&.
     posColumn newPos === 1 .&&.
     posOffset newPos === posOffset pos + 1

-- Property: posAfter tab advances to next tab stop (8-char alignment)
prop_posAfter_tab :: SourcePos -> Property
prop_posAfter_tab pos =
  let newPos = posAfter '\t' pos
      expectedColumn = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in property $ posLine newPos === posLine pos .&&.
     posColumn newPos === expectedColumn .&&.
     posOffset newPos === posOffset pos + 1

-- Property: posAfter regular character increments column L.and offset
prop_posAfter_regular :: SourcePos -> Char -> Property
prop_posAfter_regular pos c =
  c /= '\n' && c /= '\t' ==>
  let newPos = posAfter c pos
  in property $ posLine newPos === posLine pos .&&.
     posColumn newPos === posColumn pos + 1 .&&.
     posOffset newPos === posOffset pos + 1

-- Property: posAt creates position with given line L.and column
prop_posAt_creates_position :: Int -> Int -> Property
prop_posAt_creates_position line col =
  line > 0 && col > 0 ==>
  let pos = posAt line col
  in property $ posLine pos === line .&&.
     posColumn pos === col .&&.
     posOffset pos === 0

-- Property: posAtLineCol creates position with given line, column, L.and offset
prop_posAtLineCol_creates_position :: Int -> Int -> Int -> Property
prop_posAtLineCol_creates_position line col offset =
  line >= 0 && col >= 0 && offset >= 0 ==>
  let pos = posAtLineCol line col offset
  in property $ posLine pos === line .&&.
     posColumn pos === col .&&.
     posOffset pos === offset

-- Property: emptySpan has start L.and end at startPos
prop_emptySpan_properties :: Property
prop_emptySpan_properties =
  let span = emptySpan
  in property $ spanStart span === startPos .&&.
     spanEnd span === startPos

-- Property: spanFrom creates span with given start L.and end at startPos
prop_spanFrom_properties :: SourcePos -> Property
prop_spanFrom_properties start =
  let span = spanFrom start
  in property $ spanStart span === start .&&.
     spanEnd span === startPos

-- Property: spanTo creates span with start at startPos L.and given end
prop_spanTo_properties :: SourcePos -> Property
prop_spanTo_properties end =
  let span = spanTo end
  in property $ spanStart span === startPos .&&.
     spanEnd span === end

-- Property: spanBetween creates span with given start L.and end
prop_spanBetween_properties :: SourcePos -> SourcePos -> Property
prop_spanBetween_properties start end =
  let span = spanBetween start end
  in property $ spanStart span === start .&&.
     spanEnd span === end

-- Property: mergeSpans creates span covering both input spans
prop_mergeSpans_covers_both :: SourcePos -> SourcePos -> SourcePos -> SourcePos -> Property
prop_mergeSpans_covers_both start1 end1 start2 end2 =
  let span1 = spanBetween start1 end1
      span2 = spanBetween start2 end2
      merged = mergeSpans span1 span2
      minLine = min (posLine start1) (posLine start2)
      maxLine = max (posLine end1) (posLine end2)
  in property $ posLine (spanStart merged) <= minLine .&&.
     posLine (spanEnd merged) >= maxLine

-- Property: isValidSpan returns true for spans with start <= end
prop_isValidSpan_valid :: SourcePos -> SourcePos -> Property
prop_isValidSpan_valid start end =
  let span = spanBetween start end
      valid = posLine start < posLine end || 
              (posLine start == posLine end && posColumn start <= posColumn end)
  in property $ isValidSpan span === valid

-- Property: locatedAt creates located value with given position
prop_locatedAt_properties :: Int -> String -> Property
prop_locatedAt_properties line value =
  line > 0 ==>
  let pos = posAt line 1
      located = locatedAt pos value
  in property $ locatedSpan located === spanFrom pos .&&.
     locatedValue located === value

-- Property: locatedWithSpan creates located value with given span
prop_locatedWithSpan_properties :: SourcePos -> SourcePos -> String -> Property
prop_locatedWithSpan_properties start end value =
  let span = spanBetween start end
      located = locatedWithSpan span value
  in property $ locatedSpan located === span .&&.
     locatedValue located === value

-- Property: mapLocated applies function to located value
prop_mapLocated_properties :: SourcePos -> String -> Property
prop_mapLocated_properties pos value =
  let located = locatedAt pos value
      mapped = mapLocated (L.reverse) located
  in property $ locatedSpan mapped === locatedSpan located .&&.
     locatedValue mapped === L.reverse value

-- Property: advancePos by single character matches posAfter
prop_advancePos_matches_posAfter :: SourcePos -> Char -> Property
prop_advancePos_matches_posAfter pos c =
  let advanced = advancePos c pos
      expected = posAfter c pos
  in property $ advanced === expected

-- Property: advancePosBy multiple characters
prop_advancePosBy_multiple :: SourcePos -> String -> Property
prop_advancePosBy_multiple pos str =
  let advanced = advancePosBy str pos
      expected = L.foldl (flip posAfter) pos str
  in property $ advanced === expected

-- Property: advancePosByText for Text
prop_advancePosByText_text :: SourcePos -> Text -> Property
prop_advancePosByText_text pos text =
  let advanced = advancePosByText text pos
      expected = advancePosBy (T.unpack text) pos
  in property $ advanced === expected

-- Property: advancePosByLine increments line L.and resets column
prop_advancePosByLine_properties :: SourcePos -> Property
prop_advancePosByLine_properties pos =
  let advanced = advancePosByLine pos
  in property $ posLine advanced === posLine pos + 1 .&&.
     posColumn advanced === 1 .&&.
     posOffset advanced === posOffset pos

-- Property: toErrorLocation converts position to ErrorLocation
prop_toErrorLocation_properties :: SourcePos -> Property
prop_toErrorLocation_properties pos =
  let errorLoc = toErrorLocation pos
  in property $ errorLocLine errorLoc === posLine pos .&&.
     errorLocColumn errorLoc === posColumn pos

-- Property: toErrorLocationWithSpan converts span to ErrorLocation
prop_toErrorLocationWithSpan_properties :: SourcePos -> SourcePos -> Property
prop_toErrorLocationWithSpan_properties start end =
  let span = spanBetween start end
      errorLoc = toErrorLocationWithSpan span
  in property $ errorLocLine errorLoc === posLine start .&&.
     errorLocColumn errorLoc === posColumn start

tests :: TestTree
tests = testGroup "Enhanced SourceLocation Properties QuickCheck"
  [ fastProperty "startPos is constant" prop_startPos_constant
  , fastProperty "posAfter newline increments line" prop_posAfter_newline
  , fastProperty "posAfter tab advances to tab stop" prop_posAfter_tab
  , fastProperty "posAfter regular character increments column" prop_posAfter_regular
  , fastProperty "posAt creates position" prop_posAt_creates_position
  , fastProperty "posAtLineCol creates position" prop_posAtLineCol_creates_position
  , fastProperty "emptySpan properties" prop_emptySpan_properties
  , fastProperty "spanFrom properties" prop_spanFrom_properties
  , fastProperty "spanTo properties" prop_spanTo_properties
  , fastProperty "spanBetween properties" prop_spanBetween_properties
  , fastProperty "mergeSpans covers both spans" prop_mergeSpans_covers_both
  , fastProperty "isValidSpan validation" prop_isValidSpan_valid
  , fastProperty "locatedAt properties" prop_locatedAt_properties
  , fastProperty "locatedWithSpan properties" prop_locatedWithSpan_properties
  , fastProperty "mapLocated properties" prop_mapLocated_properties
  , fastProperty "advancePos matches posAfter" prop_advancePos_matches_posAfter
  , fastProperty "advancePosBy multiple characters" prop_advancePosBy_multiple
  , fastProperty "advancePosByText for Text" prop_advancePosByText_text
  , fastProperty "advancePosByLine properties" prop_advancePosByLine_properties
  , fastProperty "toErrorLocation properties" prop_toErrorLocation_properties
  , fastProperty "toErrorLocationWithSpan properties" prop_toErrorLocationWithSpan_properties
  ]