{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.SourceLocationBoundaryQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.)
  , Arbitrary(..), Gen, oneof, choose, listOf, vectorOf, elements, sized, frequency
  , suchThat, resize
  )

import SourceLocation 
  ( SourcePos(..), SourceSpan(..), Located(..), startPos, posAfter, posAt, advancePos
  , advancePosByText, spanFrom, spanTo, mergeSpans, isValidSpan, locatedAt
  , locatedWithSpan, locatedValue, locatedSpan, locatedPos, mapLocated
  )
import Data.Char (isSpace, isPrint)
import qualified Data.Text as T
import Data.List (foldl')

-- Test data for source location
data SourceLocationTestData = SourceLocationTestData
  { testText :: String
  , startPos :: SourcePos
  , expectedEndPos :: SourcePos
  } deriving (Show, Eq)

instance Arbitrary SourceLocationTestData where
  arbitrary = do
    text <- listOf $ elements "abc\n\t "
    startLine <- choose (1, 50)
    startCol <- choose (1, 50)
    let start = SourcePos startLine startCol 0
        endPos = advancePosByText start (T.pack text)
    return $ SourceLocationTestData text start endPos

-- Property: Position advancement for single character
prop_pos_after_single_char :: Char -> SourcePos -> Property
prop_pos_after_single_char char pos =
  let newPos = posAfter char pos
      lineChanged = char == '\n'
  in property $ if lineChanged
    then posLine newPos == posLine pos + 1 && posColumn newPos == 1
    else posLine newPos == posLine pos && posColumn newPos == posColumn pos + 1

-- Property: Position advancement for tab character
prop_pos_after_tab :: SourcePos -> Property
prop_pos_after_tab pos =
  let newPos = posAfter '\t' pos
      expectedCol = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in property $ posLine newPos == posLine pos && posColumn newPos == expectedCol

-- Property: Position advancement preserves monotonicity
prop_pos_advancement_monotonic :: String -> SourcePos -> Property
prop_pos_advancement_monotonic text pos =
  not (null text) ==>
  let finalPos = advancePosByText pos (T.pack text)
      startOffset = posOffset pos
      finalOffset = posOffset finalPos
  in property $ finalOffset >= startOffset

-- Property: Line advancement is correct for newlines
prop_line_advancement_correct :: String -> SourcePos -> Property
prop_line_advancement_correct text pos =
  let newlineCount = length $ filter (== '\n') text
      finalPos = advancePosByText pos (T.pack text)
      expectedLine = posLine pos + newlineCount
  in property $ posLine finalPos == expectedLine

-- Property: Column resets after newlines
prop_column_resets_after_newlines :: String -> SourcePos -> Property
prop_column_resets_after_newlines text pos =
  let finalPos = advancePosByText pos (T.pack text)
      lastNewlineIndex = length text - length (dropWhile (/= '\n') (reverse text))
      hasNewline = '\n' `elem` text
  in hasNewline ==>
  let afterLastNewline = drop lastNewlineIndex text
      expectedCol = if null afterLastNewline 
                   then 1 
                   else 1 + length (takeWhile (/= '\n') afterLastNewline)
  in property $ posColumn finalPos == expectedCol

-- Property: Span validity is consistent
prop_span_validity_consistent :: SourcePos -> SourcePos -> Property
prop_span_validity_consistent start end =
  let span = SourceSpan start end
      valid = isValidSpan span
  in property $ valid == (posLine start <= posLine end && 
                         (posLine start < posLine end || posColumn start <= posColumn end))

-- Property: Span merging works correctly
prop_span_merging_correct :: SourcePos -> SourcePos -> SourcePos -> Property
prop_span_merging_correct pos1 pos2 pos3 =
  let span1 = spanFrom pos1 pos2
      span2 = spanFrom pos2 pos3
      merged = mergeSpans span1 span2
      expectedStart = spanStart span1
      expectedEnd = spanEnd span2
  in property $ spanStart merged == expectedStart && spanEnd merged == expectedEnd

-- Property: Located values preserve position information
prop_located_preserves_position :: SourcePos -> String -> Property
prop_located_preserves_position pos value =
  let located = locatedAt pos value
  in property $ locatedPos located == pos

-- Property: Located values preserve content
prop_located_preserves_content :: SourcePos -> String -> Property
prop_located_preserves_content pos value =
  let located = locatedAt pos value
  in property $ locatedValue located == value

-- Property: Mapping located values preserves position
prop_map_located_preserves_position :: SourcePos -> Int -> Property
prop_map_located_preserves_position pos value =
  let located = locatedAt pos value
      mapped = mapLocated (*2) located
  in property $ locatedPos mapped == pos

-- Property: Mapping located values transforms content
prop_map_located_transforms_content :: SourcePos -> Int -> Property
prop_map_located_transforms_content pos value =
  let located = locatedAt pos value
      mapped = mapLocated (*2) located
  in property $ locatedValue mapped == value * 2

-- Property: Position tracking through multiple lines
prop_multi_line_position_tracking :: [String] -> SourcePos -> Property
prop_multi_line_position_tracking lines' pos =
  not (null lines') ==>
  let text = T.unlines lines'
      finalPos = advancePosByText pos text
      expectedLine = posLine pos + length lines' - 1
      lastLine = last lines'
      expectedCol = if null lastLine then 1 else length lastLine + 1
  in property $ posLine finalPos == expectedLine && posColumn finalPos == expectedCol

-- Property: Offset calculation is consistent
prop_offset_calculation_consistent :: String -> SourcePos -> Property
prop_offset_calculation_consistent text pos =
  let finalPos = advancePosByText pos (T.pack text)
      startOffset = posOffset pos
      expectedOffset = startOffset + length text
  in property $ posOffset finalPos == expectedOffset

tests :: TestTree
tests = testGroup "Source Location Boundary QuickCheck Tests"
  [ fastProperty "Position advancement for single character" prop_pos_after_single_char
  , fastProperty "Position advancement for tab character" prop_pos_after_tab
  , fastProperty "Position advancement preserves monotonicity" prop_pos_advancement_monotonic
  , fastProperty "Line advancement is correct for newlines" prop_line_advancement_correct
  , fastProperty "Column resets after newlines" prop_column_resets_after_newlines
  , fastProperty "Span validity is consistent" prop_span_validity_consistent
  , fastProperty "Span merging works correctly" prop_span_merging_correct
  , fastProperty "Located values preserve position information" prop_located_preserves_position
  , fastProperty "Located values preserve content" prop_located_preserves_content
  , fastProperty "Mapping located values preserves position" prop_map_located_preserves_position
  , fastProperty "Mapping located values transforms content" prop_map_located_transforms_content
  , fastProperty "Position tracking through multiple lines" prop_multi_line_position_tracking
  , fastProperty "Offset calculation is consistent" prop_offset_calculation_consistent
  , testCase "Manual source location test" $ do
      let start = startPos
          afterHello = advancePosByText start "Hello"
          afterHelloWorld = advancePosByText afterHello " World"
          afterNewline = advancePosByText afterHelloWorld "\n"
          afterNewLineHello = advancePosByText afterNewline "Hello"
      
      posLine afterHello @?= 1
      posColumn afterHello @?= 6
      posLine afterHelloWorld @?= 1
      posColumn afterHelloWorld @?= 12
      posLine afterNewline @?= 2
      posColumn afterNewline @?= 1
      posLine afterNewLineHello @?= 2
      posColumn afterNewLineHello @?= 6
      
      let span = spanFrom start afterHelloWorld
      isValidSpan span @?= True
      spanStart span @?= start
      spanEnd span @?= afterHelloWorld
  ]