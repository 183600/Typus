{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.SourceLocationMathPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (choose, listOf1, elements)

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
  , advancePos
  , advancePosBy
  )

import Data.List (sort)
import Data.Char (isSpace)

-- Property: startPos is the minimal position
prop_startPos_minimal :: Property
prop_startPos_minimal =
  posLine startPos === 1 .&&.
  posColumn startPos === 1 .&&.
  posOffset startPos === 0

-- Property: posAt creates positions with correct line and column
prop_posAt_correct :: Int -> Int -> Property
prop_posAt_correct line col =
  line >= 1 && col >= 1 ==>
  posLine (posAt line col) === line .&&.
  posColumn (posAt line col) === col

-- Property: posAtLineCol creates positions with correct line, column, and offset
prop_posAtLineCol_correct :: Int -> Int -> Int -> Property
prop_posAtLineCol_correct line col offset =
  line >= 1 && col >= 1 && offset >= 0 ==>
  posLine (posAtLineCol line col offset) === line .&&.
  posColumn (posAtLineCol line col offset) === col .&&.
  posOffset (posAtLineCol line col offset) === offset

-- Property: posAfter newline increments line and resets column
prop_posAfter_newline :: SourcePos -> Property
prop_posAfter_newline pos =
  let newPos = posAfter '\n' pos
  in posLine newPos === posLine pos + 1 .&&.
     posColumn newPos === 1 .&&.
     posOffset newPos === posOffset pos + 1

-- Property: posAfter tab advances to next tab stop (8-column alignment)
prop_posAfter_tab :: SourcePos -> Property
prop_posAfter_tab pos =
  let newPos = posAfter '\t' pos
      expectedCol = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in posColumn newPos === expectedCol .&&.
     posOffset newPos === posOffset pos + 1

-- Property: posAfter regular character increments column and offset
prop_posAfter_regular :: SourcePos -> Char -> Property
prop_posAfter_regular pos c =
  c /= '\n' && c /= '\t' ==>
  let newPos = posAfter c pos
  in posColumn newPos === posColumn pos + 1 .&&.
     posLine newPos === posLine pos .&&.
     posOffset newPos === posOffset pos + 1

-- Property: emptySpan creates a span with same start and end
prop_emptySpan_identity :: SourcePos -> Property
prop_emptySpan_identity pos =
  let span = emptySpan pos
  in spanStart span === pos .&&. spanEnd span === pos

-- Property: spanFrom is equivalent to emptySpan
prop_spanFrom_equals_emptySpan :: SourcePos -> Property
prop_spanFrom_equals_emptySpan pos =
  spanFrom pos === emptySpan pos

-- Property: spanTo creates a span with same start and end
prop_spanTo_identity :: SourcePos -> Property
prop_spanTo_identity pos =
  let span = spanTo pos
  in spanStart span === pos .&&. spanEnd span === pos

-- Property: spanBetween creates span with given start and end
prop_spanBetween_correct :: SourcePos -> SourcePos -> Property
prop_spanBetween_correct start end =
  let span = spanBetween start end
  in spanStart span === start .&&. spanEnd span === end

-- Property: mergeSpans creates span that encompasses both spans
prop_mergeSpans_encompassing :: SourcePos -> SourcePos -> SourcePos -> SourcePos -> Property
prop_mergeSpans_encompassing start1 end1 start2 end2 =
  let span1 = spanBetween start1 end1
      span2 = spanBetween start2 end2
      merged = mergeSpans span1 span2
  in spanStart merged === min start1 start2 .&&.
     spanEnd merged === max end1 end2

-- Property: mergeSpans is commutative
prop_mergeSpans_commutative :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_commutative span1 span2 =
  mergeSpans span1 span2 === mergeSpans span2 span1

-- Property: mergeSpans is associative
prop_mergeSpans_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_mergeSpans_associative span1 span2 span3 =
  mergeSpans (mergeSpans span1 span2) span3 === mergeSpans span1 (mergeSpans span2 span3)

-- Property: isValidSpan checks start <= end
prop_isValidSpan_correct :: SourcePos -> SourcePos -> Property
prop_isValidSpan_correct start end =
  let span = spanBetween start end
  in isValidSpan span === (start <= end)

-- Property: locatedAt creates located value with correct position
prop_locatedAt_correct :: SourcePos -> Int -> Property
prop_locatedAt_correct pos value =
  let located = locatedAt pos value
  in locatedValue located === value .&&.
     locatedPos located === pos .&&.
     locatedSpan located === emptySpan pos

-- Property: locatedWithSpan creates located value with correct span
prop_locatedWithSpan_correct :: SourceSpan -> Int -> Property
prop_locatedWithSpan_correct span value =
  let located = locatedWithSpan span value
  in locatedValue located === value .&&.
     locatedSpan located === span .&&.
     locatedPos located === spanStart span

-- Property: advancePos by newline advances line count
prop_advancePos_newline :: SourcePos -> Int -> Property
prop_advancePos_newline pos n =
  n >= 0 ==>
  let newPos = advancePos '\n' n pos
  in posLine newPos === posLine pos + n .&&.
     posColumn newPos === 1 .&&.
     posOffset newPos === posOffset pos + n

-- Property: advancePos preserves position for zero count
prop_advancePos_zero :: SourcePos -> Char -> Property
prop_advancePos_zero pos c =
  advancePos c 0 pos === pos

-- Property: advancePosBy multiple characters advances correctly
prop_advancePosBy_sequence :: SourcePos -> String -> Property
prop_advancePosBy_sequence pos chars =
  not (null chars) ==>
  let finalPos = advancePosBy chars pos
      expectedPos = foldl (flip posAfter) pos chars
  in finalPos === expectedPos

tests :: TestTree
tests =
  testGroup "SourceLocation Math Properties"
    [ fastProperty "startPos is minimal" prop_startPos_minimal
    , fastProperty "posAt creates correct positions" prop_posAt_correct
    , fastProperty "posAtLineCol creates correct positions" prop_posAtLineCol_correct
    , fastProperty "posAfter handles newline correctly" prop_posAfter_newline
    , fastProperty "posAfter handles tab correctly" prop_posAfter_tab
    , fastProperty "posAfter handles regular characters correctly" prop_posAfter_regular
    , fastProperty "emptySpan has same start and end" prop_emptySpan_identity
    , fastProperty "spanFrom equals emptySpan" prop_spanFrom_equals_emptySpan
    , fastProperty "spanTo creates identity span" prop_spanTo_identity
    , fastProperty "spanBetween creates correct span" prop_spanBetween_correct
    , fastProperty "mergeSpans encompasses both spans" prop_mergeSpans_encompassing
    , fastProperty "mergeSpans is commutative" prop_mergeSpans_commutative
    , fastProperty "mergeSpans is associative" prop_mergeSpans_associative
    , fastProperty "isValidSpan checks correctly" prop_isValidSpan_correct
    , fastProperty "locatedAt creates correct located value" prop_locatedAt_correct
    , fastProperty "locatedWithSpan creates correct located value" prop_locatedWithSpan_correct
    , fastProperty "advancePos handles newline correctly" prop_advancePos_newline
    , fastProperty "advancePos preserves position for zero count" prop_advancePos_zero
    , fastProperty "advancePosBy advances sequence correctly" prop_advancePosBy_sequence
    ]