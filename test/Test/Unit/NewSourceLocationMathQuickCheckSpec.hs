{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewSourceLocationMathQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

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
  , spanStart
  , spanEnd
  )

import Data.Char (isSpace)
import qualified Data.Text as T

-- Property: startPos consistency
prop_startPos_consistency :: Property
prop_startPos_consistency =
  let pos = startPos
  in property $ posLine pos === 1 .&&. posColumn pos === 1 .&&. posOffset pos === 0

-- Property: posAt creates correct position
prop_posAt_correctness :: Int -> Int -> Property
prop_posAt_correctness line col =
  line > 0 && col > 0 && line <= 1000 && col <= 1000 ==>
  let pos = posAt line col
  in property $ posLine pos === line .&&. posColumn pos === col .&&. posOffset pos === 0

-- Property: posAtLineCol creates correct position with offset
prop_posAtLineCol_correctness :: Int -> Int -> Int -> Property
prop_posAtLineCol_correctness line col offset =
  line > 0 && col > 0 && offset >= 0 && line <= 1000 && col <= 1000 && offset <= 10000 ==>
  let pos = posAtLineCol line col offset
  in property $ posLine pos === line .&&. posColumn pos === col .&&. posOffset pos === offset

-- Property: posAfter handles newline correctly
prop_posAfter_newline :: Int -> Int -> Int -> Property
prop_posAfter_newline line col offset =
  line > 0 && col > 0 && offset >= 0 && line <= 100 && col <= 100 && offset <= 1000 ==>
  let pos = posAtLineCol line col offset
      newPos = posAfter '\n' pos
  in property $ posLine newPos === line + 1 .&&. posColumn newPos === 1 .&&. posOffset newPos === offset + 1

-- Property: posAfter handles tab correctly (8-space tabs)
prop_posAfter_tab :: Int -> Int -> Int -> Property
prop_posAfter_tab line col offset =
  line > 0 && col > 0 && offset >= 0 && line <= 100 && col <= 100 && offset <= 1000 ==>
  let pos = posAtLineCol line col offset
      newPos = posAfter '\t' pos
      expectedCol = ((col - 1) `div` 8 + 1) * 8 + 1
  in property $ posLine newPos === line .&&. posColumn newPos === expectedCol .&&. posOffset newPos === offset + 1

-- Property: posAfter handles regular characters correctly
prop_posAfter_regular :: Int -> Int -> Int -> Char -> Property
prop_posAfter_regular line col offset ch =
  line > 0 && col > 0 && offset >= 0 && line <= 100 && col <= 100 && offset <= 1000 && ch /= '\n' && ch /= '\t' ==>
  let pos = posAtLineCol line col offset
      newPos = posAfter ch pos
  in property $ posLine newPos === line .&&. posColumn newPos === col + 1 .&&. posOffset newPos === offset + 1

-- Property: emptySpan consistency
prop_emptySpan_consistency :: Property
prop_emptySpan_consistency =
  let span = emptySpan
      start = spanStart span
      end = spanEnd span
  in property $ start === startPos .&&. end === startPos

-- Property: spanFrom creates span from position
prop_spanFrom_correctness :: Int -> Int -> Property
prop_spanFrom_correctness line col =
  line > 0 && col > 0 && line <= 100 && col <= 100 ==>
  let pos = posAt line col
      span = spanFrom pos
      start = spanStart span
      end = spanEnd span
  in property $ start === pos .&&. end === pos

-- Property: spanTo creates span to position
prop_spanTo_correctness :: Int -> Int -> Property
prop_spanTo_correctness line col =
  line > 0 && col > 0 && line <= 100 && col <= 100 ==>
  let pos = posAt line col
      span = spanTo pos
      start = spanStart span
      end = spanEnd span
  in property $ start === startPos .&&. end === pos

-- Property: spanBetween creates correct span
prop_spanBetween_correctness :: Int -> Int -> Int -> Int -> Property
prop_spanBetween_correctness line1 col1 line2 col2 =
  line1 > 0 && col1 > 0 && line2 > 0 && col2 > 0 &&
  line1 <= 100 && col1 <= 100 && line2 <= 100 && col2 <= 100 ==>
  let pos1 = posAt line1 col1
      pos2 = posAt line2 col2
      span = spanBetween pos1 pos2
      start = spanStart span
      end = spanEnd span
  in property $ start === pos1 .&&. end === pos2

-- Property: mergeSpans correctness
prop_mergeSpans_correctness :: Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_mergeSpans_correctness line1 col1 line2 col2 line3 col3 =
  line1 > 0 && col1 > 0 && line2 > 0 && col2 > 0 && line3 > 0 && col3 > 0 &&
  line1 <= 100 && col1 <= 100 && line2 <= 100 && col2 <= 100 && line3 <= 100 && col3 <= 100 ==>
  let pos1 = posAt line1 col1
      pos2 = posAt line2 col2
      pos3 = posAt line3 col3
      span1 = spanBetween pos1 pos2
      span2 = spanBetween pos2 pos3
      merged = mergeSpans span1 span2
      start = spanStart merged
      end = spanEnd merged
  in property $ start === pos1 .&&. end === pos3

-- Property: locatedAt creates correct located value
prop_locatedAt_correctness :: Int -> Int -> String -> Property
prop_locatedAt_correctness line col value =
  line > 0 && col > 0 && line <= 100 && col <= 100 ==>
  let pos = posAt line col
      located = locatedAt pos value
      span = locatedSpan located
      extractedValue = locatedValue located
      extractedPos = locatedPos located
  in property $ spanStart span === pos .&&. spanEnd span === pos .&&.
     extractedValue === value .&&. extractedPos === pos

-- Property: mapLocated preserves location
prop_mapLocated_preserves_location :: Int -> Int -> String -> Property
prop_mapLocated_preserves_location line col value =
  line > 0 && col > 0 && line <= 100 && col <= 100 ==>
  let pos = posAt line col
      located = locatedAt pos value
      mapped = mapLocated length located
      originalSpan = locatedSpan located
      mappedSpan = locatedSpan mapped
  in property $ originalSpan === mappedSpan .&&. locatedValue mapped === length value

-- Property: advancePosByText correctness
prop_advancePosByText_correctness :: Int -> Int -> String -> Property
prop_advancePosByText_correctness line col text =
  line > 0 && col > 0 && line <= 50 && col <= 50 && length text <= 100 ==>
  let pos = posAt line col
      advanced = advancePosByText pos text
      expectedOffset = posOffset pos + length text
  in property $ posOffset advanced >= expectedOffset

-- Property: advancePosByLine correctness
prop_advancePosByLine_correctness :: Int -> Int -> Int -> Property
prop_advancePosByLine_correctness line col numLines =
  line > 0 && col > 0 && numLines >= 0 && line <= 50 && col <= 50 && numLines <= 50 ==>
  let pos = posAt line col
      advanced = advancePosByLine pos numLines
      expectedLine = line + numLines
  in property $ posLine advanced === expectedLine .&&. posColumn advanced === col

-- Property: isValidSpan correctness
prop_isValidSpan_correctness :: Int -> Int -> Int -> Int -> Property
prop_isValidSpan_correctness line1 col1 line2 col2 =
  line1 > 0 && col1 > 0 && line2 > 0 && col2 > 0 &&
  line1 <= 100 && col1 <= 100 && line2 <= 100 && col2 <= 100 ==>
  let pos1 = posAt line1 col1
      pos2 = posAt line2 col2
      span = spanBetween pos1 pos2
      isValid = isValidSpan span
      pos1Valid = pos1 <= pos2
  in property $ isValid === pos1Valid

-- Property: Position ordering consistency
prop_position_ordering_consistency :: Int -> Int -> Int -> Int -> Property
prop_position_ordering_consistency line1 col1 line2 col2 =
  line1 > 0 && col1 > 0 && line2 > 0 && col2 > 0 &&
  line1 <= 100 && col1 <= 100 && line2 <= 100 && col2 <= 100 ==>
  let pos1 = posAtLineCol line1 col1 (line1 * 100 + col1)
      pos2 = posAtLineCol line2 col2 (line2 * 100 + col2)
      ordering1 = pos1 <= pos2
      ordering2 = posLine pos1 < posLine pos2 || 
                  (posLine pos1 == posLine pos2 && posColumn pos1 <= posColumn pos2)
  in property $ ordering1 === ordering2

-- Property: Span merging is associative
prop_mergeSpans_associative :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_mergeSpans_associative l1 c1 l2 c2 l3 c3 l4 c4 =
  all (>0) [l1, c1, l2, c2, l3, c3, l4, c4] &&
  all (<=100) [l1, c1, l2, c2, l3, c3, l4, c4] ==>
  let pos1 = posAt l1 c1
      pos2 = posAt l2 c2
      pos3 = posAt l3 c3
      pos4 = posAt l4 c4
      span1 = spanBetween pos1 pos2
      span2 = spanBetween pos2 pos3
      span3 = spanBetween pos3 pos4
      merge12 = mergeSpans span1 span2
      merge23 = mergeSpans span2 span3
      result1 = mergeSpans merge12 span3
      result2 = mergeSpans span1 merge23
  in property $ result1 === result2

tests :: TestTree
tests = testGroup "New Source Location Math QuickCheck Tests"
  [ fastProperty "startPos consistency" prop_startPos_consistency
  , fastProperty "posAt creates correct position" prop_posAt_correctness
  , fastProperty "posAtLineCol creates correct position with offset" prop_posAtLineCol_correctness
  , fastProperty "posAfter handles newline correctly" prop_posAfter_newline
  , fastProperty "posAfter handles tab correctly" prop_posAfter_tab
  , fastProperty "posAfter handles regular characters correctly" prop_posAfter_regular
  , fastProperty "emptySpan consistency" prop_emptySpan_consistency
  , fastProperty "spanFrom creates span from position" prop_spanFrom_correctness
  , fastProperty "spanTo creates span to position" prop_spanTo_correctness
  , fastProperty "spanBetween creates correct span" prop_spanBetween_correctness
  ]