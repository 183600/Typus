{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewSourceLocationPrecisionSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property
  , (===)
  , (==>)
  , forAll
  , counterexample
  , classify
  , property
  , (.&&.)
  , (.||.)
  , Arbitrary(..)
  , Gen
  , choose
  , listOf
  , elements
  , oneof
  , sized
  , resize
  , Positive(..)
  , NonEmptyList(..)
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
  , advancePos
  , advancePosBy
  , toErrorLocation
  , toErrorLocationWithSpan
  )

import Data.Char (isSpace, toLower)
import qualified Data.List as Data.List
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (tails, sort, nub)
import qualified Data.Text as T

-- Test source position creation L.and arithmetic
test_source_position_arithmetic :: TestTree
test_source_position_arithmetic = testCase "Source position arithmetic works correctly" $ do
  let pos1 = SourcePos 1 1 0
      pos2 = advancePos pos1 'a'
      pos3 = advancePos pos2 '\n'
      pos4 = advancePos pos3 'b'
  pos2 @?= SourcePos 1 2 1
  pos3 @?= SourcePos 2 1 2
  pos4 @?= SourcePos 2 2 3

-- Test source span creation L.and validation
test_source_span_creation :: TestTree
test_source_span_creation = testCase "Source span creation L.and validation" $ do
  let start = SourcePos 1 1 0
      end = SourcePos 1 5 4
      span = SourceSpan start end
  isValidSpan span @?= True
  spanFrom start end @?= span
  
  let invalidSpan = SourceSpan end start  -- end before start
  isValidSpan invalidSpan @?= False

-- Test span merging functionality
test_span_merging :: TestTree
test_span_merging = testCase "Span merging works correctly" $ do
  let span1 = SourceSpan (SourcePos 1 1 0) (SourcePos 1 5 4)
      span2 = SourceSpan (SourcePos 1 3 2) (SourcePos 1 8 7)
      merged = mergeSpans span1 span2
  merged @?= SourceSpan (SourcePos 1 1 0) (SourcePos 1 8 7)

-- Test located values L.and their positions
test_located_values :: TestTree
test_located_values = testCase "Located values track positions correctly" $ do
  let pos = SourcePos 2 3 10
      span = SourceSpan pos (SourcePos 2 8 15)
      located = locatedWithSpan span "test value"
  locatedValue located @?= "test value"
  locatedSpan located @?= span
  locatedPos located @?= pos

-- Test position advancement with different characters
test_position_advancement :: TestTree
test_position_advancement = testCase "Position advancement handles different characters" $ do
  let start = SourcePos 1 1 0
      pos1 = advancePos start 'a'      -- Regular character
      pos2 = advancePos pos1 '\t'      -- Tab
      pos3 = advancePos pos2 '\n'      -- Newline
      pos4 = advancePos pos3 ' '       -- Space
      pos5 = advancePos pos4 '中'      -- Unicode character
  pos1 @?= SourcePos 1 2 1
  pos2 @?= SourcePos 1 3 2
  pos3 @?= SourcePos 2 1 3
  pos4 @?= SourcePos 2 2 4
  pos5 @?= SourcePos 2 3 5

-- Test position advancement with strings
test_position_advancement_by_string :: TestTree
test_position_advancement_by_string = testCase "Position advancement by strings" $ do
  let start = SourcePos 1 1 0
      pos1 = advancePosBy start "hello"
      pos2 = advancePosBy pos1 "\nworld"
      pos3 = advancePosBy pos2 "\ttest"
  pos1 @?= SourcePos 1 6 5
  pos2 @?= SourcePos 2 6 11
  pos3 @?= SourcePos 2 11 16

-- Test error location conversion
test_error_location_conversion :: TestTree
test_error_location_conversion = testCase "Error location conversion" $ do
  let pos = SourcePos 3 4 20
      span = SourceSpan pos (SourcePos 3 8 24)
      errorLoc1 = toErrorLocation pos
      errorLoc2 = toErrorLocationWithSpan span
  -- Test that error locations contain position information
  assertBool "Error location should contain line info" $ 
    L.isInfixOf "3" (show errorLoc1)
  assertBool "Error location with span should contain line info" $ 
    L.isInfixOf "3" (show errorLoc2)

-- Test location tracking with multi-line content
test_multiline_location_tracking :: TestTree
test_multiline_location_tracking = testCase "Multi-line location tracking" $ do
  let content = unlines
        [ "line 1"
        , "line 2"
        , "line 3"
        ]
      start = startPos
      posAfterLine1 = advancePosBy start "line 1\n"
      posAfterLine2 = advancePosBy posAfterLine1 "line 2\n"
      posAfterLine3 = advancePosBy posAfterLine2 "line 3"
  posAfterLine1 @?= SourcePos 2 1 7
  posAfterLine2 @?= SourcePos 3 1 14
  posAfterLine3 @?= SourcePos 3 7 21

-- Test location precision with Unicode characters
test_unicode_location_precision :: TestTree
test_unicode_location_precision = testCase "Unicode location precision" $ do
  let start = SourcePos 1 1 0
      unicodeText = "测试Unicode🚀"
      posAfter = advancePosBy start unicodeText
  posAfter @?= SourcePos 1 (1 + L.length unicodeText) (L.length unicodeText)

-- Test span boundaries L.and containment
test_span_boundaries :: TestTree
test_span_boundaries = testCase "Span boundaries L.and containment" $ do
  let outerSpan = SourceSpan (SourcePos 1 1 0) (SourcePos 3 1 20)
      innerSpan1 = SourceSpan (SourcePos 1 5 4) (SourcePos 1 10 9)
      innerSpan2 = SourceSpan (SourcePos 2 1 10) (SourcePos 2 8 17)
      overlappingSpan = SourceSpan (SourcePos 3 1 20) (SourcePos 4 1 25)
  
  -- Test span creation L.and validation
  isValidSpan outerSpan @?= True
  isValidSpan innerSpan1 @?= True
  isValidSpan innerSpan2 @?= True
  isValidSpan overlappingSpan @?= True

-- Property: Position advancement is consistent
prop_position_advancement_consistent :: String -> Char -> Property
prop_position_advancement_consistent str ch = 
  let start = startPos
      pos1 = advancePosBy start str
      pos2 = advancePos pos1 ch
      pos3 = advancePosBy start (str ++ [ch])
  in pos2 === pos3

-- Property: Span merging is commutative for valid spans
prop_span_merging_commutative :: SourceSpan -> SourceSpan -> Property
prop_span_merging_commutative span1 span2 = 
  isValidSpan span1 && isValidSpan span2 ==>
    let merged1 = mergeSpans span1 span2
        merged2 = mergeSpans span2 span1
    in merged1 === merged2

-- Property: Position arithmetic is additive
prop_position_arithmetic_additive :: String -> String -> Property
prop_position_arithmetic_additive str1 str2 = 
  let start = startPos
      pos1 = advancePosBy start str1
      pos2 = advancePosBy pos1 str2
      pos3 = advancePosBy start (str1 ++ str2)
  in pos2 === pos3

-- Property: Located values preserve their content
prop_located_values_preserve_content :: String -> SourceSpan -> Property
prop_located_values_preserve_content str span = 
  isValidSpan span ==>
    let located = locatedWithSpan span str
    in locatedValue located === str

-- Property: Position tracking handles newlines correctly
prop_position_newline_handling :: Int -> Int -> Property
prop_position_newline_handling (Positive lines) (Positive cols) = 
  let newlineCount = min lines 100
      colCount = min cols 100
      content = unlines (replicate newlineCount (replicate colCount 'a'))
      start = startPos
      finalPos = advancePosBy start content
  in posLine finalPos === newlineCount + 1 .&&. posColumn finalPos === 1

-- Property: Span boundaries are consistent
prop_span_boundaries_consistent :: Int -> Int -> Property
prop_span_boundaries_consistent (Positive startLine) (Positive L.length) = 
  let startL = min startLine 100
      len = min L.length 100
      start = SourcePos startL 1 (startL * 10)
      end = advancePosBy start (replicate len 'a')
      span = SourceSpan start end
  in isValidSpan span ==> posLine (spanStart span) === startL .&&. posLine (spanEnd span) >= startL

-- Property: Error locations contain position information
prop_error_locations_contain_position :: SourcePos -> Property
prop_error_locations_contain_position pos = 
  let errorLoc = toErrorLocation pos
      posStr = show pos
      errorStr = show errorLoc
  in property $ L.isInfixOf (show (posLine pos)) errorStr .&&. 
             L.isInfixOf (show (posColumn pos)) errorStr

tests :: TestTree
tests = testGroup "New Source Location Precision Tests"
  [ test_source_position_arithmetic
  , test_source_span_creation
  , test_span_merging
  , test_located_values
  , test_position_advancement
  , test_position_advancement_by_string
  , test_error_location_conversion
  , test_multiline_location_tracking
  , test_unicode_location_precision
  , test_span_boundaries
  , fastProperty "Position advancement is consistent" prop_position_advancement_consistent
  , fastProperty "Span merging is commutative" prop_span_merging_commutative
  , fastProperty "Position arithmetic is additive" prop_position_arithmetic_additive
  , fastProperty "Located values preserve content" prop_located_values_preserve_content
  , fastProperty "Position tracking handles newlines correctly" prop_position_newline_handling
  , fastProperty "Span boundaries are consistent" prop_span_boundaries_consistent
  , fastProperty "Error locations contain position information" prop_error_locations_contain_position
  ]