{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.SourceLocationMathQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose, Positive(..))
import Data.Char (isSpace)

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
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
  , advancePos
  , advancePosBy
  , spanStart
  , spanEnd
  )

-- Property: startPos is always (1, 1)
prop_startPos_constant :: Property
prop_startPos_constant =
  counterexample "startPos should always be (1, 1)" $
     startPos === SourcePos 1 1

-- Property: posAfter advances column by 1 for non-newline characters
prop_posAfter_advances_column :: SourcePos -> Char -> Property
prop_posAfter_advances_column pos c =
  c /= '\n' ==>
  let newPos = posAfter pos c
      expectedCol = sourceColumn pos + 1
  in counterexample "posAfter should advance column by 1 for non-newline" $
     sourceLine newPos === sourceLine pos .&&.
     sourceColumn newPos === expectedCol

-- Property: posAfter advances line and resets column for newline
prop_posAfter_newline :: SourcePos -> Property
prop_posAfter_newline pos =
  let newPos = posAfter pos '\n'
  in counterexample "posAfter should advance line and reset column for newline" $
     sourceLine newPos === sourceLine pos + 1 .&&.
     sourceColumn newPos === 1

-- Property: posAt creates position at specific line and column
prop_posAt_creation :: Int -> Int -> Property
prop_posAt_creation line col =
  line > 0 && col > 0 ==>
  let pos = posAt line col
  in counterexample "posAt should create position at given line and column" $
     sourceLine pos === line .&&.
     sourceColumn pos === col

-- Property: posAtLineCol is consistent with posAt
prop_posAtLineCol_consistency :: Int -> Int -> Property
prop_posAtLineCol_consistency line col =
  line > 0 && col > 0 ==>
  let pos1 = posAt line col
      pos2 = posAtLineCol line col
  in counterexample "posAtLineCol should be consistent with posAt" $
     pos1 === pos2

-- Property: emptySpan has start > end (invalid by definition)
prop_emptySpan_invalid :: Property
prop_emptySpan_invalid =
  let span = emptySpan
  in counterexample "emptySpan should be invalid" $
     not (isValidSpan span)

-- Property: spanFrom creates valid span with same start and end
prop_spanFrom_valid :: SourcePos -> Property
prop_spanFrom_valid pos =
  let span = spanFrom pos
  in counterexample "spanFrom should create valid span" $
     isValidSpan span .&&.
     spanStart span === spanEnd span

-- Property: spanTo creates valid span from startPos to given position
prop_spanTo_valid :: SourcePos -> Property
prop_spanTo_valid endPos =
  let span = spanTo endPos
  in counterexample "spanTo should create valid span from startPos" $
     isValidSpan span .&&.
     spanStart span === startPos .&&.
     spanEnd span === endPos

-- Property: spanBetween creates span between two positions
prop_spanBetween_order :: SourcePos -> SourcePos -> Property
prop_spanBetween_order pos1 pos2 =
  let span = spanBetween pos1 pos2
      start = spanStart span
      end = spanEnd span
  in counterexample "spanBetween should create span with proper order" $
     (start <= pos1 && end >= pos2) .||. (start <= pos2 && end >= pos1)

-- Property: mergeSpans contains both original spans
prop_mergeSpans_contains_both :: SourcePos -> SourcePos -> SourcePos -> SourcePos -> Property
prop_mergeSpans_contains_both start1 end1 start2 end2 =
  let span1 = spanBetween start1 end1
      span2 = spanBetween start2 end2
      merged = mergeSpans span1 span2
      mergedStart = spanStart merged
      mergedEnd = spanEnd merged
  in counterexample "mergeSpans should contain both original spans" $
     mergedStart <= spanStart span1 .&&.
     mergedEnd >= spanEnd span1 .&&.
     mergedStart <= spanStart span2 .&&.
     mergedEnd >= spanEnd span2

-- Property: advancePos preserves line advancement for newlines
prop_advancePos_newline :: SourcePos -> String -> Property
prop_advancePos_newline pos s =
  let finalPos = advancePos pos s
      newlineCount = length $ filter (== '\n') s
  in counterexample "advancePos should advance line by newline count" $
     sourceLine finalPos >= sourceLine pos + newlineCount

-- Property: advancePosBy with empty string returns original position
prop_advancePosBy_empty :: SourcePos -> Property
prop_advancePosBy_empty pos =
  let result = advancePosBy pos ""
  in counterexample "advancePosBy with empty string should return original position" $
     result === pos

-- Property: advancePosBy is consistent with repeated posAfter
prop_advancePosBy_consistency :: SourcePos -> String -> Property
prop_advancePosBy_consistency pos s =
  let directResult = advancePosBy pos s
      stepByStepResult = foldl posAfter pos s
  in counterexample "advancePosBy should be consistent with repeated posAfter" $
     directResult === stepByStepResult

-- Property: SourcePos ordering is transitive
prop_sourcepos_transitive :: SourcePos -> SourcePos -> SourcePos -> Property
prop_sourcepos_transitive pos1 pos2 pos3 =
  pos1 <= pos2 && pos2 <= pos3 ==>
  counterexample "SourcePos ordering should be transitive" $
     pos1 <= pos3

-- Property: spanBetween with same positions creates zero-length span
prop_spanBetween_zero_length :: SourcePos -> Property
prop_spanBetween_zero_length pos =
  let span = spanBetween pos pos
  in counterexample "spanBetween with same positions should create zero-length span" $
     spanStart span === spanEnd span

-- Generate valid source positions
genSourcePos :: Gen SourcePos
genSourcePos = do
  line <- choose (1, 1000)
  col <- choose (1, 1000)
  return $ SourcePos line col

-- Generate strings with various characters for position advancement
genAdvancementString :: Gen String
genAdvancementString = listOf $ oneof
  [ elements ['a'..'z']
  , elements ['A'..'Z']
  , elements ['0'..'9']
  , elements " \t!@#$%^&*()_+-=[]{}|;':\",./<>?"
  , return '\n'
  , return '\r'
  ]

tests :: TestTree
tests = testGroup "Source Location Math QuickCheck Tests"
  [ fastProperty "startPos is constant" prop_startPos_constant
  , fastProperty "posAfter advances column" prop_posAfter_advances_column
  , fastProperty "posAfter handles newline" prop_posAfter_newline
  , fastProperty "posAt creation" prop_posAt_creation
  , fastProperty "posAtLineCol consistency" prop_posAtLineCol_consistency
  , fastProperty "emptySpan is invalid" prop_emptySpan_invalid
  , fastProperty "spanFrom creates valid span" prop_spanFrom_valid
  , fastProperty "spanTo creates valid span" prop_spanTo_valid
  , fastProperty "spanBetween order" prop_spanBetween_order
  , fastProperty "mergeSpans contains both" prop_mergeSpans_contains_both
  , fastProperty "advancePos newline handling" prop_advancePos_newline
  , fastProperty "advancePosBy empty string" prop_advancePosBy_empty
  , fastProperty "advancePosBy consistency" prop_advancePosBy_consistency
  , fastProperty "SourcePos transitivity" prop_sourcepos_transitive
  , fastProperty "spanBetween zero length" prop_spanBetween_zero_length
  ]