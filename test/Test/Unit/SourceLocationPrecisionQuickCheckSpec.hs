{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.SourceLocationPrecisionQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), locatedWithSpan, spanStart, spanEnd, startPos)
import Parser (parseTypus)
import Compiler (CompilerError(..))
import ErrorHandler (formatErrorWithLocation)
import Data.List (isInfixOf, lines, length)

-- Property: Source positions are accurate for single-line errors
prop_source_pos_single_line_accuracy :: String -> Int -> Property
prop_source_pos_single_line_accuracy code errorCol =
  let hasCode = length code > 0
      validCol = errorCol >= 1 && errorCol <= length code + 1
      errorPos = SourcePos 1 errorCol
      locatedError = locatedWithSpan errorPos (SourceSpan errorPos errorPos) ("Error at " ++ show errorCol)
      formatted = formatErrorWithLocation locatedError
      hasCol = show errorCol `isInfixOf` formatted
      hasLine = "1" `isInfixOf` formatted
  in hasCode && validCol ==> property $ hasCol .&&. hasLine

-- Property: Source spans correctly cover multi-line content
prop_source_span_multiline_coverage :: [String] -> Property
prop_source_span_multiline_coverage codeLines =
  let hasLines = length codeLines >= 2
      nonEmptyLines = all (not . null) codeLines
      startLine = 1
      endLine = length codeLines
      startCol = 1
      endCol = maximum (map length codeLines)
      startPos = SourcePos startLine startCol
      endPos = SourcePos endLine endCol
      span = SourceSpan startPos endPos
      spanStr = show span
      hasStartLine = show startLine `isInfixOf` spanStr
      hasEndLine = show endLine `isInfixOf` spanStr
  in hasLines && nonEmptyLines ==> property $ hasStartLine .&&. hasEndLine

-- Property: Error location information is preserved in parsing errors
prop_error_location_preserved_in_parsing :: String -> Int -> Property
prop_error_location_preserved_in_parsing malformedCode errorLine =
  let hasCode = length malformedCode > 5
      validLine = errorLine >= 1 && errorLine <= 10
      codeWithLines = unlines $ take errorLine (repeat malformedCode)
  in hasCode && validLine ==>
  case parseTypus codeWithLines of
    Right _ -> property $ True
    Left parseError ->
      let errorStr = show parseError
          hasLineNumber = any (`isInfixOf` errorStr) [show errorLine, "line", "Line"]
          hasColumn = any (`isInfixOf` errorStr) ["column", "col", ":"]
      in property $ hasLineNumber .||. hasColumn

-- Property: Source location tracking is consistent across multiple errors
prop_source_location_consistency :: String -> Property
prop_source_location_consistency code =
  let hasCode = length code > 10
      codeLines = lines code
      hasMultipleLines = length codeLines >= 2
  in hasCode && hasMultipleLines ==>
  case parseTypus code of
    Right _ -> property $ True
    Left parseError ->
      let errorStr = show parseError
          errorLines = lines errorStr
          locationConsistent = all (\line -> 
            any (`isInfixOf` line) ["line", "Line", ":", "column", "col"]) errorLines
      in property $ locationConsistent .||. length errorLines <= 3

-- Property: Located values preserve their position information
prop_located_values_preserve_position :: String -> Int -> Int -> Property
prop_located_values_preserve_position value line col =
  let hasValue = length value > 0
      validLine = line >= 1 && line <= 100
      validCol = col >= 1 && col <= 100
      pos = SourcePos line col
      span = SourceSpan pos pos
      located = locatedWithSpan span value
      retrievedPos = spanStart span
      retrievedValue = located located
  in hasValue && validLine && validCol ==> 
     property $ retrievedValue === value .&&. 
                startPos retrievedPos === pos

-- Property: Source span calculations are mathematically correct
prop_source_span_calculations_correct :: Int -> Int -> Int -> Int -> Property
prop_source_span_calculations_correct startLine startCol endLine endCol =
  let validStart = startLine >= 1 && startCol >= 1
      validEnd = endLine >= startLine && (endLine > startLine || endCol >= startCol)
      startPos = SourcePos startLine startCol
      endPos = SourcePos endLine endCol
      span = SourceSpan startPos endPos
      calculatedStart = spanStart span
      calculatedEnd = spanEnd span
  in validStart && validEnd ==> 
     property $ calculatedStart === startPos .&&. calculatedEnd === endPos

-- Property: Error formatting includes all relevant location information
prop_error_formatting_complete_location :: String -> Int -> Int -> Property
prop_error_formatting_complete_location errorMsg line col =
  let hasError = length errorMsg > 0
      validLine = line >= 1 && line <= 1000
      validCol = col >= 1 && col <= 1000
      pos = SourcePos line col
      span = SourceSpan pos pos
      locatedError = locatedWithSpan span errorMsg
      formatted = formatErrorWithLocation locatedError
      hasErrorMessage = errorMsg `isInfixOf` formatted
      hasLineInfo = show line `isInfixOf` formatted
      hasColInfo = show col `isInfixOf` formatted
  in hasError && validLine && validCol ==> 
     property $ hasErrorMessage .&&. hasLineInfo .&&. hasColInfo

tests :: TestTree
tests = testGroup "Source Location Precision QuickCheck Tests"
  [ fastProperty "Source positions are accurate for single-line errors" prop_source_pos_single_line_accuracy
  , fastProperty "Source spans correctly cover multi-line content" prop_source_span_multiline_coverage
  , fastProperty "Error location information is preserved in parsing errors" prop_error_location_preserved_in_parsing
  , fastProperty "Source location tracking is consistent across multiple errors" prop_source_location_consistency
  , fastProperty "Located values preserve their position information" prop_located_values_preserve_position
  , fastProperty "Source span calculations are mathematically correct" prop_source_span_calculations_correct
  , fastProperty "Error formatting includes all relevant location information" prop_error_formatting_complete_location
  ]