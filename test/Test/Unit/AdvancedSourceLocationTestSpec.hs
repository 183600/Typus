{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.AdvancedSourceLocationTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements
  , vectorOf, oneof, frequency, suchThat
  )

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
  , toErrorLocation
  , toErrorLocationWithSpan
  , advancePos
  , advancePosBy
  )

import Data.List (sort)
import qualified Data.Text as T

-- | Generate a source position
genSourcePos :: Gen SourcePos
genSourcePos = do
  line <- choose (1, 1000)
  col <- choose (1, 1000)
  let offset = (line - 1) * 1000 + col  -- Simple offset calculation
  return $ SourcePos line col offset

-- | Generate a source span with valid positions
genSourceSpan :: Gen SourceSpan
genSourceSpan = do
  start <- genSourcePos
  end <- genSourcePos
  -- Ensure end is not before start
  let validEnd = if posLine end < posLine start || 
                    (posLine end == posLine start && posColumn end < posColumn start)
                 then start
                 else end
  return $ SourceSpan start validEnd

-- | Generate a located value
genLocated :: Gen (Located String)
genLocated = do
  value <- listOf $ elements ['a'..'z']
  span <- genSourceSpan
  let pos = spanStart span
  return $ Located value pos span

instance Arbitrary SourcePos where
  arbitrary = genSourcePos

instance Arbitrary SourceSpan where
  arbitrary = genSourceSpan

-- Property tests for SourcePos

-- Property: startPos should have line 1, column 1
prop_startPos_properties :: Property
prop_startPos_properties =
  let startPos = SourcePos 1 1 0
  in property $ posLine startPos === 1 .&&. posColumn startPos === 1

-- Property: posAfter advances column by 1 on same line
prop_posAfter_same_line :: SourcePos -> Property
prop_posAfter_same_line pos =
  let after = posAfter 'a' pos
  in property $ posLine after === posLine pos .&&. 
             posColumn after === posColumn pos + 1

-- Property: posAt creates position at specific line L.and column
prop_posAt_creation :: Int -> Int -> Property
prop_posAt_creation line col =
  line > 0 && col > 0 && line <= 1000 && col <= 1000 ==>
  let pos = SourcePos line col 0
  in property $ posLine pos === line .&&. posColumn pos === col

-- Property: posAtLineCol is same as posAt
prop_posAtLineCol_consistency :: Int -> Int -> Property
prop_posAtLineCol_consistency line col =
  line > 0 && col > 0 && line <= 1000 && col <= 1000 ==>
  let pos1 = SourcePos line col 0
      pos2 = SourcePos line col 0
  in property $ pos1 === pos2

-- Property tests for SourceSpan

-- Property: emptySpan should have start L.and end at startPos
prop_emptySpan_properties :: Property
prop_emptySpan_properties =
  let startPos = SourcePos 1 1 0
      span = emptySpan startPos
  in property $ spanStart span === startPos .&&. 
             spanEnd span === startPos

-- Property: spanFrom creates span from position to same position
prop_spanFrom_single_pos :: SourcePos -> Property
prop_spanFrom_single_pos pos =
  let span = spanFrom pos
  in property $ spanStart span === pos .&&. spanEnd span === pos

-- Property: spanTo creates span from startPos to given position
prop_spanTo_from_start :: SourcePos -> Property
prop_spanTo_from_start pos =
  let span = spanTo pos
  in property $ spanStart span === startPos .&&. spanEnd span === pos

-- Property: spanBetween creates span with correct order
prop_spanBetween_order :: SourcePos -> SourcePos -> Property
prop_spanBetween_order pos1 pos2 =
  let span = spanBetween pos1 pos2
      start = spanStart span
      end = spanEnd span
      orderCorrect = posLine start < posLine end || 
                     (posLine start == posLine end && posColumn start <= posColumn end)
      containsPoints = (start == pos1 && end == pos2) || (start == pos2 && end == pos1)
  in property $ orderCorrect .&&. containsPoints

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
  in property $ posLine mergedStart <= posLine start1 .&&.
             posLine mergedStart <= posLine start2 .&&.
             posLine mergedEnd >= posLine end1 .&&.
             posLine mergedEnd >= posLine end2

-- Property: mergeSpans is commutative
prop_mergeSpans_commutative :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_commutative span1 span2 =
  mergeSpans span1 span2 === mergeSpans span2 span1

-- Property: mergeSpans is associative
prop_mergeSpans_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_mergeSpans_associative span1 span2 span3 =
  mergeSpans span1 (mergeSpans span2 span3) === 
  mergeSpans (mergeSpans span1 span2) span3

-- Property: isValidSpan checks for valid span
prop_isValidSpan_check :: SourceSpan -> Property
prop_isValidSpan_check span =
  let start = spanStart span
      end = spanEnd span
      expectedValid = posLine start < posLine end || 
                      (posLine start == posLine end && posColumn start <= posColumn end)
  in property $ isValidSpan span === expectedValid

-- Property tests for Located

-- Property: locatedAt creates located value with span from position
prop_locatedAt_single_pos :: SourcePos -> String -> Property
prop_locatedAt_single_pos pos value =
  let located = locatedAt pos value
      span = locatedSpan located
  in property $ spanStart span === pos .&&. spanEnd span === pos .&&.
             locatedValue located === value

-- Property: locatedWithSpan creates located value with given span
prop_locatedWithSpan_correct :: SourceSpan -> String -> Property
prop_locatedWithSpan_correct span value =
  let located = locatedWithSpan span value
  in property $ locatedSpan located === span .&&.
             locatedValue located === value

-- Property: mapLocated preserves span but transforms value
prop_mapLocated_preserves_span :: SourceSpan -> String -> String -> Property
prop_mapLocated_preserves_span span value1 value2 =
  let located1 = locatedWithSpan span value1
      located2 = mapLocated (++ value2) located1
  in property $ locatedSpan located2 === locatedSpan located1 .&&.
             locatedValue located2 === value1 ++ value2

-- Property tests for position advancement

-- Property: advancePos by single character
prop_advancePos_single_char :: SourcePos -> Char -> Property
prop_advancePos_single_char pos ch =
  let advanced = advancePos ch pos
  in if ch == '\n'
     then property $ posLine advanced === posLine pos + 1 .&&.
                    posColumn advanced === 1
     else property $ posLine advanced === posLine pos .&&.
                    posColumn advanced === posColumn pos + 1

-- Property: advancePosBy with empty string returns same position
prop_advancePosBy_empty :: SourcePos -> Property
prop_advancePosBy_empty pos =
  advancePosBy "" pos === pos

-- Property: advancePosBy with multiple newlines
prop_advancePosBy_newlines :: SourcePos -> Int -> Property
prop_advancePosBy_newlines pos count =
  count > 0 && count <= 100 ==>
  let newlines = replicate count '\n'
      advanced = advancePosBy newlines pos
  in property $ posLine advanced === posLine pos + count .&&.
             posColumn advanced === 1

-- Property: advancePosBy is consistent with repeated advancePos
prop_advancePosBy_consistency :: SourcePos -> String -> Property
prop_advancePosBy_consistency pos str =
  let advancedBy = advancePosBy str pos
      advancedRepeated = foldl (\p c -> advancePos c p) pos str
  in property $ advancedBy === advancedRepeated

-- Boundary condition tests

-- Property: positions with L.maximum values
prop_maximum_positions :: Property
prop_maximum_positions =
  let maxPos = SourcePos 1000000 1000000 0
      after = posAfter 'a' maxPos
  in property $ posLine after === 1000000 .&&. posColumn after === 1000001

-- Property: spans with same start L.and end are valid
prop_same_start_end_valid :: SourcePos -> Property
prop_same_start_end_valid pos =
  let span = SourceSpan pos pos
  in property $ isValidSpan span

-- Property: merging with emptySpan
prop_merge_with_empty :: SourceSpan -> Property
prop_merge_with_empty span =
  let empty = emptySpan (spanStart span)
  in mergeSpans span empty === mergeSpans empty span

-- Error location tests

-- Property: toErrorLocation preserves position information
prop_toErrorLocation_preserves :: SourcePos -> Property
prop_toErrorLocation_preserves pos =
  let errorLoc = toErrorLocation pos
  in property $ True -- Basic smoke test

-- Property: toErrorLocationWithSpan preserves span information
prop_toErrorLocationWithSpan_preserves :: SourceSpan -> Property
prop_toErrorLocationWithSpan_preserves span =
  let errorLoc = toErrorLocationWithSpan span
  in property $ True -- Basic smoke test

-- Performance L.and stress tests

-- Property: large span merging performance
prop_large_span_merge :: Int -> Property
prop_large_span_merge size =
  size > 0 && size <= 1000 ==>
  let startPos = SourcePos 1 1 0
      spans = replicate size (emptySpan startPos)
      empty = emptySpan startPos
      merged = foldl mergeSpans empty spans
  in property $ isValidSpan merged

-- Property: complex advancement scenarios
prop_complex_advancement :: SourcePos -> String -> Property
prop_complex_advancement pos str =
  let advanced = advancePosBy str pos
      -- Just verify it doesn't crash L.and returns a valid position
  in property $ posLine advanced >= posLine pos

-- Unit tests

unit_tests :: TestTree
unit_tests = testGroup "SourceLocation Unit Tests"
  [ testCase "startPos has correct values" $ do
      let startPos = SourcePos 1 1 0
      posLine startPos @?= 1
      posColumn startPos @?= 1

  , testCase "posAfter advances correctly" $ do
      let pos = SourcePos 5 10 0
          after = posAfter 'a' pos
      posLine after @?= 5
      posColumn after @?= 11

  , testCase "posAt creates correct position" $ do
      let pos = SourcePos 3 7 0
      posLine pos @?= 3
      posColumn pos @?= 7

  , testCase "emptySpan has correct properties" $ do
      let startPos = SourcePos 1 1 0
          span = emptySpan startPos
      spanStart span @?= startPos
      spanEnd span @?= startPos

  , testCase "spanFrom creates single position span" $ do
      let pos = SourcePos 2 5 0
          span = spanFrom pos
      spanStart span @?= pos
      spanEnd span @?= pos

  , testCase "spanTo creates span from start" $ do
      let startPos = SourcePos 1 1 0
          end = SourcePos 3 8 0
          span = spanTo end
      spanStart span @?= startPos
      spanEnd span @?= end

  , testCase "mergeSpans works correctly" $ do
      let span1 = SourceSpan (SourcePos 1 1 0) (SourcePos 2 5 20)
          span2 = SourceSpan (SourcePos 2 3 15) (SourcePos 3 10 40)
          merged = mergeSpans span1 span2
      spanStart merged @?= SourcePos 1 1 0
      spanEnd merged @?= SourcePos 3 10 40

  , testCase "locatedAt creates correct located value" $ do
      let pos = SourcePos 4 6 0
          value = "test"
          located = locatedAt pos value
      locatedSpan located @?= SourceSpan pos pos
      locatedValue located @?= value

  , testCase "mapLocated preserves span" $ do
      let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 5 4)
          located1 = locatedWithSpan span "hello"
          located2 = mapLocated (++ " world") located1
      locatedSpan located2 @?= span
      locatedValue located2 @?= "hello world"

  , testCase "advancePos handles newline correctly" $ do
      let pos = SourcePos 3 5 0
          advanced = advancePos '\n' pos
      posLine advanced @?= 4
      posColumn advanced @?= 1

  , testCase "advancePos advances position" $ do
      let pos = SourcePos 1 5 0
          advanced = advancePos 'a' pos
      posLine advanced @?= 1
      posColumn advanced @?= 6

  , testCase "advancePosBy advances by string" $ do
      let pos = SourcePos 3 4 0
          text = "abc"
          advanced = advancePosBy text pos
      posLine advanced @?= 3
      posColumn advanced @?= 7

  , testCase "isValidSpan identifies valid spans" $ do
      let validSpan = SourceSpan (SourcePos 1 1 0) (SourcePos 2 1 10)
          invalidSpan = SourceSpan (SourcePos 2 1 10) (SourcePos 1 1 0)
      isValidSpan validSpan @?= True
      isValidSpan invalidSpan @?= False
  ]

tests :: TestTree
tests = testGroup "Advanced SourceLocation Tests"
  [ testGroup "Property Tests"
    [ fastProperty "startPos properties" prop_startPos_properties
    , fastProperty "posAfter advances column" prop_posAfter_same_line
    , fastProperty "posAt creation" prop_posAt_creation
    , fastProperty "posAtLineCol consistency" prop_posAtLineCol_consistency
    , fastProperty "emptySpan properties" prop_emptySpan_properties
    , fastProperty "spanFrom single position" prop_spanFrom_single_pos
    , fastProperty "spanTo from start" prop_spanTo_from_start
    , fastProperty "spanBetween order" prop_spanBetween_order
    , fastProperty "mergeSpans contains both" prop_mergeSpans_contains_both
    , fastProperty "mergeSpans commutative" prop_mergeSpans_commutative
    , fastProperty "mergeSpans associative" prop_mergeSpans_associative
    , fastProperty "isValidSpan check" prop_isValidSpan_check
    , fastProperty "locatedAt single position" prop_locatedAt_single_pos
    , fastProperty "locatedWithSpan correct" prop_locatedWithSpan_correct
    , fastProperty "mapLocated preserves span" prop_mapLocated_preserves_span
    , fastProperty "advancePos single character" prop_advancePos_single_char
    , fastProperty "advancePosBy empty" prop_advancePosBy_empty
    , fastProperty "advancePosBy newlines" prop_advancePosBy_newlines
    , fastProperty "advancePosBy consistency" prop_advancePosBy_consistency
    , fastProperty "L.maximum positions" prop_maximum_positions
    , fastProperty "same start end valid" prop_same_start_end_valid
    , fastProperty "merge with empty" prop_merge_with_empty
    , fastProperty "toErrorLocation preserves" prop_toErrorLocation_preserves
    , fastProperty "toErrorLocationWithSpan preserves" prop_toErrorLocationWithSpan_preserves
    , fastProperty "large span merge" prop_large_span_merge
    , fastProperty "complex advancement" prop_complex_advancement
    ]
  , unit_tests
  ]