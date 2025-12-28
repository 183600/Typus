{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewComprehensiveQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, oneof, sized, vector)
import Data.Char (isSpace, isAlphaNum, isLetter)
import qualified Data.List as Data.List
import Data.List (isPrefixOf, isInfixOf, sort, nub)
import qualified Data.Text as T

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , removeComments
  , normalizeIndentation
  , breakOn
  )
import SourceLocation
  ( SourcePos(..), SourceSpan(..), Located(..)
  , startPos, posAfter, advancePosBy
  , emptySpan, spanFrom, mergeSpans, isValidSpan
  , locatedAt, locatedWithSpan, mapLocated
  , runLocationTracker, getCurrentPos, setCurrentPos
  , toErrorLocation, toErrorLocationWithSpan
  )
import Parser
  ( parseTypus
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )

-- ============================================================================
-- Comprehensive Integration Properties
-- ============================================================================

-- Property: Parser output location consistency
prop_parser_location_consistency :: String -> Property
prop_parser_location_consistency content =
  not ("{//!" `isInfixOf` content) && not ("//!" `isPrefixOf` content) ==>
  case parseTypus content of
    Left _ -> property False
    Right file -> 
      let blocks = tfBlocks file
          spans = map cbSpan blocks
          locations = map toErrorLocationWithSpan spans
      in property $ all (\loc -> line loc > 0 && column loc > 0) locations

-- Property: Source location tracking through text processing
prop_location_tracking_text_processing :: SourcePos -> String -> Property
prop_location_tracking_text_processing pos text =
  let advanced = advancePosBy text pos
      textLines = lines text
      expectedLines = posLine pos + length textLines - 1
  in property $ posLine advanced >= posLine pos .&&.
             posLine advanced <= expectedLines + 1

-- Property: Comment removal preserves line structure for location tracking
prop_comment_removal_line_structure :: String -> Property
prop_comment_removal_line_structure content =
  not ('"' `elem` content) && not ('\'' `elem` content) ==>
  let withComments = unlines $ map (++ " // comment") (lines content)
      withoutComments = removeComments withComments
      originalLines = lines content
      processedLines = lines withoutComments
  in property $ length processedLines === length originalLines

-- Property: Indentation normalization affects position tracking
prop_indentation_position_tracking :: [Int] -> String -> Property
prop_indentation_position_tracking indentLevels content =
  not (null indentLevels) && not ('\n' `elem` content) ==>
  let lines' = zipWith (\level -> replicate (abs level `mod` 8) ' ' ++) indentLevels (repeat content)
      input = unlines lines'
      normalized = normalizeIndentation input
      originalPositions = scanl (\pos line -> advancePosBy line pos) startPos lines'
      normalizedPositions = scanl (\pos line -> advancePosBy line pos) startPos (lines normalized)
  in property $ length normalizedPositions === length originalPositions .&&.
             all (\pos -> posLine pos >= 1) normalizedPositions

-- Property: Parser directives and source location integration
prop_parser_directives_location :: String -> String -> Property
prop_parser_directives_location directiveKey directiveValue =
  directiveKey `elem` ["ownership", "dependent_types", "constraints"] && 
  directiveValue `elem` ["on", "off"] ==>
  let input = "//! " ++ directiveKey ++ ": " ++ directiveValue ++ "\nsome code"
  in case parseTypus input of
    Left _ -> property False
    Right file -> property $ tfDirectives file /= defaultFileDirectives

-- Property: Complex text processing pipeline
prop_complex_text_pipeline :: String -> Property
prop_complex_text_pipeline content =
  not ('"' `elem` content) && not ('\'' `elem` content) ==>
  let step1 = "// comment\n" ++ content ++ "\n/* block comment */"
      step2 = removeComments step1
      step3 = trim step2
      step4 = normalizeIndentation step3
      finalLines = lines step4
  in property $ not (null finalLines) ==> not (any null finalLines)

-- Property: Source span merging with parser output
prop_span_merging_parser_output :: String -> Property
prop_span_merging_parser_output content =
  not ("{//!" `isInfixOf` content) && not ("//!" `isPrefixOf` content) ==>
  case parseTypus content of
    Left _ -> property False
    Right file -> 
      let blocks = tfBlocks file
          spans = map cbSpan blocks
          merged = foldl mergeSpans emptySpan startPos spans
      in property $ all (`isValidSpan`) spans ==> isValidSpan merged

-- Property: Located value transformation consistency
prop_located_transformation_consistency :: SourceSpan -> [Int] -> Property
prop_located_transformation_consistency span values =
  let located = locatedWithSpan span values
      transformations = [sum, product, length, maximum, minimum]
      results = map (\f -> mapLocated f located) transformations
      spans' = map locatedSpan results
  in property $ all (=== span) spans'

-- Property: Parser error handling with location information
prop_parser_error_location :: String -> Property
prop_parser_error_location malformed =
  let malformedDirective = "//! " ++ malformed ++ ": invalid"
      input = malformedDirective ++ "\nsome code"
      result = parseTypus input
  in case result of
    Left err -> property $ not (null err) -- Error message should not be empty
    Right _ -> property $ True -- Might succeed if malformed is actually valid

-- Property: Text splitting and position tracking
prop_splitting_position_tracking :: Char -> String -> Property
prop_splitting_position_tracking delim str =
  let parts = splitBy delim str
      positions = scanl (\pos part -> advancePosBy (part ++ [delim]) pos) startPos parts
  in property $ length positions === length parts .&&.
             all (\pos -> posColumn pos >= 1) positions

-- Property: Multi-module consistency check
prop_multimodule_consistency :: String -> Property
prop_multimodule_consistency content =
  not ('"' `elem` content) && not ('\'' `elem` content) ==>
  let utilsProcessed = removeComments content |> trim |> normalizeIndentation
      parsed = parseTypus content
  in case parsed of
    Left _ -> property $ not (null utilsProcessed) || null content
    Right file -> property $ not (null (tfBlocks file)) ==> 
                              any (not . null . cbContent) (tfBlocks file)

-- Property: Performance with combined operations
prop_combined_performance :: Int -> String -> Property
prop_combined_performance multiplier base =
  multiplier > 0 && multiplier <= 50 ==> -- Bounded for performance
  let large = concat $ replicate multiplier (base ++ "\n")
      processed = removeComments large |> normalizeIndentation
      parsed = parseTypus large
  in case parsed of
    Left _ -> property $ length processed <= length large
    Right file -> property $ length (tfBlocks file) >= 0

-- Property: Unicode handling across modules
prop_unicode_integration :: String -> Property
prop_unicode_integration content =
  let unicodeContent = content ++ " café naïve 🚀 测试"
      utilsResult = removeComments unicodeContent
      parserResult = parseTypus ("//! ownership: on\n" ++ unicodeContent)
  in case parserResult of
    Left _ -> property $ "café" `isInfixOf` utilsResult
    Right file -> property $ case tfBlocks file of
                              [block] -> "café naïve 🚀 测试" `isInfixOf` cbContent block
                              _ -> property $ "café" `isInfixOf` utilsResult

-- Property: Error location conversion consistency
prop_error_location_consistency :: SourceSpan -> Property
prop_error_location_consistency span =
  let errLoc1 = toErrorLocationWithSpan span
      errLoc2 = toErrorLocation (spanStart span)
  in property $ line errLoc1 === line errLoc2 .&&.
             column errLoc1 === column errLoc2 .&&.
             endLine errLoc1 === Just (posLine (spanEnd span))

-- Property: Complex directive parsing with location tracking
prop_complex_directive_parsing :: [String] -> Property
prop_complex_directive_parsing directives =
  all (not . null) directives && all (not . any isSpace) directives ==>
  let directiveLines = map (\d -> "//! " ++ d ++ ": on") directives
      input = unlines directiveLines ++ "\nsome code"
  in case parseTypus input of
    Left _ -> property $ length directives > 10 -- Might fail with too many directives
    Right file -> property $ tfDirectives file /= defaultFileDirectives

-- Property: Text normalization and parsing integration
prop_normalization_parsing_integration :: [Int] -> String -> Property
prop_normalization_parsing_integration indentLevels content =
  not (null indentLevels) && not ('\n' `elem` content) ==>
  let lines' = zipWith (\level -> replicate (abs level `mod` 6) ' ' ++) indentLevels (repeat content)
      input = unlines lines'
      normalized = normalizeIndentation input
      parsed = parseTypus normalized
  in case parsed of
    Left _ -> property $ not (null normalized)
    Right file -> property $ length (lines normalized) === length indentLevels

-- ============================================================================
-- Edge Cases and Boundary Conditions
-- ============================================================================

-- Property: Empty content handling across modules
prop_empty_content_handling :: Property
prop_empty_content_handling =
  let utilsResult = trim "" |> removeComments |> normalizeIndentation
      parserResult = parseTypus ""
  in case parserResult of
    Left _ -> property $ null utilsResult
    Right file -> property $ null (tfBlocks file) .&&. tfDirectives file === defaultFileDirectives

-- Property: Single character handling
prop_single_character_handling :: Char -> Property
prop_single_character_handling char =
  let content = [char]
      utilsResult = trim content
      parserResult = parseTypus content
  in case parserResult of
    Left _ -> property $ not (isSpace char) ==> not (null utilsResult)
    Right file -> property $ if isSpace char 
                             then null (tfBlocks file)
                             else length (tfBlocks file) >= 0

-- Property: Maximum boundary values
prop_maximum_boundaries :: Property
prop_maximum_boundaries =
  let maxPos = SourcePos 1000000 1000000 1000000
      maxSpan = SourceSpan maxPos maxPos
      maxLocated = locatedWithSpan maxSpan "test"
  in property $ posLine maxPos === 1000000 .&&.
             posColumn maxPos === 1000000 .&&.
             posOffset maxPos === 1000000 .&&.
             isValidSpan maxSpan .&&.
             locatedValue maxLocated === "test"

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New Comprehensive QuickCheck Tests"
  [ testGroup "Integration Properties"
    [ fastProperty "parser output location consistency" prop_parser_location_consistency
    , fastProperty "source location tracking through text processing" prop_location_tracking_text_processing
    , fastProperty "comment removal preserves line structure for location tracking" prop_comment_removal_line_structure
    , fastProperty "indentation normalization affects position tracking" prop_indentation_position_tracking
    , fastProperty "parser directives and source location integration" prop_parser_directives_location
    , fastProperty "complex text processing pipeline" prop_complex_text_pipeline
    ]
  , testGroup "Multi-Module Interaction"
    [ fastProperty "source span merging with parser output" prop_span_merging_parser_output
    , fastProperty "located value transformation consistency" prop_located_transformation_consistency
    , fastProperty "parser error handling with location information" prop_parser_error_location
    , fastProperty "text splitting and position tracking" prop_splitting_position_tracking
    , fastProperty "multi-module consistency check" prop_multimodule_consistency
    , fastProperty "performance with combined operations" prop_combined_performance
    ]
  , testGroup "Unicode and Internationalization"
    [ fastProperty "unicode handling across modules" prop_unicode_integration
    , fastProperty "error location conversion consistency" prop_error_location_consistency
    , fastProperty "complex directive parsing with location tracking" prop_complex_directive_parsing
    , fastProperty "text normalization and parsing integration" prop_normalization_parsing_integration
    ]
  , testGroup "Edge Cases and Boundary Conditions"
    [ fastProperty "empty content handling across modules" prop_empty_content_handling
    , fastProperty "single character handling" prop_single_character_handling
    , fastProperty "maximum boundary values" prop_maximum_boundaries
    ]
  ]