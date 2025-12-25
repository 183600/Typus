{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.SourceLocationTrackingQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , locatedWithSpan
  , locatedValue
  , startPos
  , posAt
  , posAtLineCol
  , spanStart
  , spanEnd
  , spanBetween
  , mergeSpans
  , isValidSpan
  , posLine
  , posColumn
  , advancePos
  )
import Parser (parseTypus, CodeBlock(..), TypusFile(..))
import Compiler.Errors.Core (ErrorLocation(..), getErrorLine, getErrorColumn)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, sort, lines, unlines)
import Data.Char (isSpace, isDigit)

-- Property: Source position creation preserves line and column
prop_source_pos_creation :: Int -> Int -> Property
prop_source_pos_creation line col =
  line > 0 && col > 0 ==>
  let pos = posAt line col
  in posLine pos === line && posColumn pos === col

-- Property: Source span creation preserves start and end positions
prop_source_span_creation :: Int -> Int -> Int -> Int -> Property
prop_source_span_creation startLine startCol endLine endCol =
  startLine > 0 && startCol > 0 && endLine > 0 && endCol > 0 &&
  (startLine < endLine || (startLine == endLine && startCol <= endCol)) ==>
  let startPos = posAt startLine startCol
      endPos = posAt endLine endCol
      span = spanBetween startPos endPos
  in spanStart span === startPos && spanEnd span === endPos



-- Property: Position advancement works correctly
prop_position_advancement :: Int -> Int -> String -> Property
prop_position_advancement line col text =
  line > 0 && col > 0 && not (null text) ==>
  let pos = mkSourcePos line col
      advancedPos = advancePos pos text
  in posLine advancedPos >= line && posColumn advancedPos >= col

-- Property: Located values preserve their spans
prop_located_values_preserve_spans :: String -> Property
prop_located_values_preserve_spans value =
  not (null value) ==>
  let start = posAt 1 1
      end = posAt 1 (length value + 1)
      span = spanBetween start end
      locatedVal = locatedWithSpan span value
  in locatedValue locatedVal === value

-- Property: Source locations handle Unicode correctly
prop_source_location_unicode :: String -> Property
prop_source_location_unicode unicodeText =
  not (null unicodeText) ==>
  let pos = posAt 1 1
      advancedPos = advancePos pos unicodeText
  in posLine advancedPos >= 1 && posColumn advancedPos >= 1

-- Property: Source position handles tabs correctly
prop_source_position_tabs :: Int -> Int -> Property
prop_source_position_tabs tabCount =
  tabCount >= 0 && tabCount <= 10 ==>
  let tabText = replicate tabCount '\t'
      pos = mkSourcePos 1 1
      advancedPos = advancePos pos tabText
  in posLine advancedPos === 1 && posColumn advancedPos >= 1

-- Property: Source position handles newlines correctly
prop_source_position_newlines :: Int -> Property
prop_source_position_newlines newlineCount =
  newlineCount >= 0 && newlineCount <= 5 ==>
  let newlineText = replicate newlineCount '\n'
      pos = mkSourcePos 1 1
      advancedPos = advancePos pos newlineText
  in posLine advancedPos === 1 + newlineCount

-- Property: Source span contains entire text
prop_span_contains_text :: String -> Property
prop_span_contains_text text =
  not (null text) && all (`notElem` text) "\r\n" ==>
  let span = mkSourceSpan (mkSourcePos 1 1) (mkSourcePos 1 (length text + 1))
  in spanLength span >= length text

-- Property: Source locations are consistent with file structure
prop_source_location_file_structure :: [String] -> Property
prop_source_location_file_structure fileLines =
  not (null fileLines) && length fileLines <= 10 && all (not . null) fileLines ==>
  let fileContent = unlines fileLines
      result = parseTypus fileContent
  in case result of
    Left _ -> property True
    Right typusFile ->
      let blocks = tfBlocks typusFile
          spans = map cbSpan blocks
      in all (\span -> let start = spanStart span in posLine start >= 1 && posLine start <= length fileLines) spans

-- Property: Source location tracking preserves ordering
prop_source_location_ordering :: [String] -> Property
prop_source_location_ordering codeSegments =
  not (null codeSegments) && length codeSegments <= 5 && all (not . null) codeSegments ==>
  let fullCode = unlines codeSegments
      result = parseTypus fullCode
  in case result of
    Left _ -> property True
    Right typusFile ->
      let blocks = tfBlocks typusFile
          spans = map cbSpan blocks
          positions = map spanStart spans
      in positions == sort positions

tests :: TestTree
tests = testGroup "Source Location Tracking QuickCheck tests"
  [ fastProperty "Source position creation preserves line and column" prop_source_pos_creation
  , fastProperty "Source span creation preserves start and end positions" prop_source_span_creation
  , fastProperty "Position advancement works correctly" prop_position_advancement
  , fastProperty "Located values preserve their spans" prop_located_values_preserve_spans
  , fastProperty "Source position handles tabs correctly" prop_source_position_tabs
  , fastProperty "Source position handles newlines correctly" prop_source_position_newlines
  ]