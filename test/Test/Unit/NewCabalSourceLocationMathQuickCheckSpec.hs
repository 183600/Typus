{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.Unit.NewCabalSourceLocationMathQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf1, vectorOf)

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

-- Generate a valid source position
genSourcePos :: Gen SourcePos
genSourcePos = do
  line <- choose (1, 1000)
  col <- choose (1, 1000)
  return $ SourcePos line col

-- Generate a valid source span
genSourceSpan :: Gen SourceSpan
genSourceSpan = do
  startLine <- choose (1, 100)
  startCol <- choose (1, 100)
  endLine <- choose (startLine, startLine + 50)  -- Ensure end line >= start line
  endCol <- if endLine == startLine 
            then choose (startCol, startCol + 50)  -- If same line, ensure end col >= start col
            else choose (1, 100)
  return $ SourceSpan (SourcePos startLine startCol) (SourcePos endLine endCol)

-- Property: startPos is always at line 1, column 1
prop_startPos_constant :: Property
prop_startPos_constant = 
  startPos === SourcePos 1 1

-- Property: posAfter advances column by 1 on the same line
prop_posAfter_advances_column :: SourcePos -> Property
prop_posAfter_advances_column pos =
  let newPos = posAfter pos
  in sourceLine newPos === sourceLine pos .&&. sourceColumn newPos === sourceColumn pos + 1

-- Property: posAt creates position at specific line and column
prop_posAt_creates_position :: Int -> Int -> Property
prop_posAt_creates_position line col =
  line > 0 && col > 0 ==> 
  let pos = posAt line col
  in sourceLine pos === line .&&. sourceColumn pos === col

-- Property: posAtLineCol is consistent with posAt
prop_posAtLineCol_consistent :: Int -> Int -> Property
prop_posAtLineCol_consistent line col =
  line > 0 && col > 0 ==>
  posAt line col === posAtLineCol line col

-- Property: emptySpan has start and end at the same position
prop_emptySpan_same_position :: SourcePos -> Property
prop_emptySpan_same_position pos =
  let span = emptySpan pos
  in spanStart span === pos .&&. spanEnd span === pos

-- Property: spanFrom creates span from position to next position
prop_spanFrom_creates_span :: SourcePos -> Property
prop_spanFrom_creates_span pos =
  let span = spanFrom pos
      expectedEnd = posAfter pos
  in spanStart span === pos .&&. spanEnd span === expectedEnd

-- Property: spanTo creates span from startPos to given position
prop_spanTo_creates_span :: SourcePos -> Property
prop_spanTo_creates_span endPos =
  let span = spanTo endPos
  in spanStart span === startPos .&&. spanEnd span === endPos

-- Property: spanBetween creates span with correct start and end
prop_spanBetween_correct :: SourcePos -> SourcePos -> Property
prop_spanBetween_correct start end =
  let span = spanBetween start end
  in spanStart span === start .&&. spanEnd span === end

-- Property: mergeSpans contains both original spans
prop_mergeSpans_contains_both :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_contains_both span1 span2 =
  let merged = mergeSpans span1 span2
      start1 = spanStart span1
      end1 = spanEnd span1
      start2 = spanStart span2
      end2 = spanEnd span2
      mergedStart = spanStart merged
      mergedEnd = spanEnd merged
  in (sourceLine mergedStart <= sourceLine start1 .&&. sourceColumn mergedStart <= sourceColumn start1) .&&.
     (sourceLine mergedEnd >= sourceLine end1 .&&. sourceColumn mergedEnd >= sourceColumn end1) .&&.
     (sourceLine mergedStart <= sourceLine start2 .&&. sourceColumn mergedStart <= sourceColumn start2) .&&.
     (sourceLine mergedEnd >= sourceLine end2 .&&. sourceColumn mergedEnd >= sourceColumn end2)

-- Property: isValidSpan returns true for valid spans
prop_isValidSpan_valid :: SourceSpan -> Property
prop_isValidSpan_valid span =
  let start = spanStart span
      end = spanEnd span
  in (sourceLine start < sourceLine end) .||. 
     (sourceLine start == sourceLine end .&&. sourceColumn start <= sourceColumn end) ==>
     isValidSpan span === True

-- Property: locatedAt creates located value at position
prop_locatedAt_correct :: SourcePos -> Int -> Property
prop_locatedAt_correct pos value =
  let located = locatedAt pos value
      expectedSpan = emptySpan pos
  in locatedValue located === value .&&. locatedSpan located === expectedSpan

-- Property: locatedWithSpan creates located value with span
prop_locatedWithSpan_correct :: SourceSpan -> String -> Property
prop_locatedWithSpan_correct span value =
  let located = locatedWithSpan span value
  in locatedValue located === value .&&. locatedSpan located === span

-- Property: advancePos advances position by one character
prop_advancePos_advances :: SourcePos -> Property
prop_advancePos_advances pos =
  let newPos = advancePos pos '\n'
  in if '\n' == '\n'  -- newline character
     then sourceLine newPos === sourceLine pos + 1 .&&. sourceColumn newPos === 1
     else sourceColumn newPos === sourceColumn pos + 1

-- Property: advancePosBy advances position by multiple characters
prop_advancePosBy_advances_multiple :: SourcePos -> String -> Property
prop_advancePosBy_advances_multiple pos str =
  let finalPos = advancePosBy pos str
      newlineCount = length $ filter (== '\n') str
  in if newlineCount > 0
     then sourceLine finalPos === sourceLine pos + newlineCount
     else sourceLine finalPos === sourceLine pos

tests :: TestTree
tests =
  testGroup "SourceLocation Math QuickCheck Tests"
    [ fastProperty "startPos is constant" prop_startPos_constant
    , fastProperty "posAfter advances column" prop_posAfter_advances_column
    , fastProperty "posAt creates position" prop_posAt_creates_position
    , fastProperty "posAtLineCol is consistent" prop_posAtLineCol_consistent
    , fastProperty "emptySpan has same position" prop_emptySpan_same_position
    , fastProperty "spanFrom creates span" prop_spanFrom_creates_span
    , fastProperty "spanTo creates span" prop_spanTo_creates_span
    , fastProperty "spanBetween correct" prop_spanBetween_correct
    , fastProperty "mergeSpans contains both" prop_mergeSpans_contains_both
    , fastProperty "isValidSpan valid" prop_isValidSpan_valid
    , fastProperty "locatedAt correct" prop_locatedAt_correct
    , fastProperty "locatedWithSpan correct" prop_locatedWithSpan_correct
    , fastProperty "advancePos advances" prop_advancePos_advances
    , fastProperty "advancePosBy advances multiple" prop_advancePosBy_advances_multiple
    ]