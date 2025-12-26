{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewComprehensiveCabalQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import Parser (parseTypus, TypusFile(..), defaultFileDirectives)
import Utils (trim, removeComments, normalizeIndentation, splitBy)
import SourceLocation (SourcePos(..), startPos, posAt, advancePosByText)
import ErrorHandler (createErrorHandler, addError, addWarning, hasErrors, errorCount, warningCount)
import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf, isInfixOf, sort)

-- Property: Complete parsing pipeline consistency
prop_complete_parsing_pipeline :: String -> String -> Property
prop_complete_parsing_pipeline directives code =
  length directives <= 100 && length code <= 200 &&
  ("//!" `isPrefixOf` directives || null directives) &&
  not (any (`isInfixOf` code) ["/*", "*/"]) ==>
  let fullContent = if null directives then code else directives ++ "\n\n" ++ code
      parsed = parseTypus fullContent "test.typus"
  in case parsed of
    Left _ -> property $ False
    Right file -> property $ tfDirectives file /= defaultFileDirectives || not (null (tfBlocks file))

-- Property: String processing pipeline end-to-end
prop_string_processing_pipeline :: String -> Property
prop_string_processing_pipeline input =
  length input <= 300 ==>
  let processed = input 
                  |> removeComments
                  |> trim
                  |> normalizeIndentation
      processed2 = input 
                   |> trim
                   |> removeComments
                   |> normalizeIndentation
  in property $ processed === processed2

-- Property: Source location tracking with text processing
prop_source_location_text_processing :: String -> Property
prop_source_location_text_processing text =
  length text <= 200 ==>
  let start = startPos
      advanced = advancePosByText start text
      lineCount = length (filter (== '\n') text) + 1
      finalLine = posLine advanced
  in property $ finalLine >= lineCount

-- Property: Error handling with parsing results
prop_error_handling_parsing :: String -> Property
prop_error_handling_parsing content =
  length content <= 150 ==>
  let handler = createErrorHandler
      parsed = parseTypus content "error-test.typus"
      handler2 = case parsed of
                   Left err -> addError ("Parse error: " ++ take 50 (show err)) "parsing" handler
                   Right _ -> handler
  in property $ case parsed of
                  Left _ -> hasErrors handler2 .&&. errorCount handler2 >= 1
                  Right _ -> not (hasErrors handler2)

-- Property: Multi-stage processing consistency
prop_multi_stage_processing :: String -> String -> Property
prop_multi_stage_processing stage1 stage2 =
  length stage1 <= 100 && length stage2 <= 100 ==>
  let combined = stage1 ++ "\n" ++ stage2
      processed1 = trim stage1
      processed2 = trim stage2
      processedCombined = trim combined
      recombined = processed1 ++ "\n" ++ processed2
      recombinedTrimmed = trim recombined
  in property $ processedCombined === recombinedTrimmed

-- Property: Directive parsing with various formats
prop_directive_parsing_formats :: String -> Property
prop_directive_parsing_formats directive =
  length directive <= 80 && ("//!" `isPrefixOf` directive || "//@" `isPrefixOf` directive) ==>
  let content = directive ++ "\nsome code content"
      parsed = parseTypus content "directive-test.typus"
  in case parsed of
    Left _ -> property $ False
    Right file -> property $ tfDirectives file /= defaultFileDirectives || not (null (tfBlocks file))

-- Property: Comment removal in complex scenarios
prop_complex_comment_removal :: String -> String -> String -> Property
prop_complex_comment_removal code1 code2 comment =
  length code1 <= 50 && length code2 <= 50 && length comment <= 50 &&
  not (any (`isInfixOf` code1) ["\"", "'"]) && not (any (`isInfixOf` code2) ["\"", "'"]) &&
  not (any (`isInfixOf` comment) ["\"", "'"]) ==>
  let complex = code1 ++ " // line comment\n/* " ++ comment ++ " */\n" ++ code2 ++ " // another comment"
      processed = removeComments complex
  in property $ not ("// line comment" `isInfixOf` processed) .&&.
     not ("/*" `isInfixOf` processed) .&&.
     not ("*/" `isInfixOf` processed) .&&.
     not ("// another comment" `isInfixOf` processed) .&&.
     (code1 `isInfixOf` processed .||. null code1) .&&.
     (code2 `isInfixOf` processed .||. null code2)

-- Property: Indentation normalization with mixed content
prop_mixed_indentation_normalization :: [String] -> Property
prop_mixed_indentation_normalization lines =
  not (null lines) && length lines <= 10 && all (\l -> length l <= 50) lines ==>
  let content = unlines lines
      normalized = normalizeIndentation content
      normalizedLines = lines normalized
  in property $ length normalizedLines === length lines

-- Property: Error accumulation in processing pipeline
prop_error_accumulation_pipeline :: [String] -> Property
prop_error_accumulation_pipeline errors =
  length errors <= 5 && all (\e -> length e <= 30) errors ==>
  let handler = createErrorHandler
      handler2 = foldr (\err h -> addError err "pipeline" h) handler errors
      finalCount = errorCount handler2
  in property $ finalCount === length errors .&&. hasErrors handler2

-- Property: Splitting and joining consistency
prop_splitting_joining_consistency :: String -> Char -> Property
prop_splitting_joining_consistency input delim =
  length input <= 100 && delim /= '\0' ==>
  let parts = splitBy delim input
      rejoined = concatMap (\p -> p ++ [delim]) (init parts) ++ last parts
  in property $ rejoined === input

-- Property: Position tracking with multi-line content
prop_position_tracking_multiline :: [String] -> Property
prop_position_tracking_multiline lineList =
  not (null lineList) && length lineList <= 8 && all (\l -> length l <= 40) lineList ==>
  let content = unlines lineList
      start = startPos
      end = advancePosByText start content
      expectedLines = posLine start + length lineList - 1
  in property $ posLine end >= expectedLines

-- Property: Content preservation through processing
prop_content_preservation_processing :: String -> Property
prop_content_preservation_processing content =
  length content <= 150 && not ("/*" `isInfixOf` content) && not ("*/" `isInfixOf` content) ==>
  let processed = removeComments content
      trimmed = trim processed
  in property $ if null content 
                 then trimmed === ""
                 else not (null trimmed) || all isSpace content

-- Property: Parser error recovery
prop_parser_error_recovery :: String -> String -> Property
prop_parser_error_recovery good bad =
  length good <= 50 && length bad <= 50 &&
  "/*" `isInfixOf` bad && not ("*/" `isInfixOf` bad) ==>
  let mixed = good ++ "\n" ++ bad ++ "\n" ++ good
      parsed = parseTypus mixed "recovery.typus"
  in case parsed of
    Left _ -> property $ False  -- Should still parse with errors tracked
    Right file -> property $ True  -- Successfully parsed with potential errors

-- Property: Warning and error separation in complex scenarios
prop_complex_error_warning_separation :: [String] -> [String] -> Property
prop_complex_error_warning_separation errorMessages warningMessages =
  length errorMessages <= 3 && length warningMessages <= 3 &&
  all (\m -> length m <= 25) (errorMessages ++ warningMessages) ==>
  let handler = createErrorHandler
      handler2 = foldr (\msg h -> addError msg ("error-" ++ msg)) handler errorMessages
      handler3 = foldr (\msg h -> addWarning msg ("warning-" ++ msg)) handler2 warningMessages
  in property $ errorCount handler3 === length errorMessages .&&.
     warningCount handler3 === length warningMessages .&&.
     (if not (null errorMessages) then hasErrors handler3 else not (hasErrors handler3)) .&&.
     (if not (null warningMessages) then hasWarnings handler3 else not (hasWarnings handler3))

-- Property: File directive parsing with multiple directives
prop_multiple_file_directives :: [String] -> Property
prop_multiple_file_directives directives =
  length directives <= 3 && all (\d -> "//!" `isPrefixOf` d && length d <= 50) directives ==>
  let directiveContent = unlines directives
      content = directiveContent ++ "\nsome code here"
      parsed = parseTypus content "multi-directive.typus"
  in case parsed of
    Left _ -> property $ False
    Right file -> property $ tfDirectives file /= defaultFileDirectives

-- Property: Block directive inheritance
prop_block_directive_inheritance :: String -> String -> Property
prop_block_directive_inheritance fileDirective blockDirective =
  "//!" `isPrefixOf` fileDirective && "//@" `isPrefixOf` blockDirective &&
  length fileDirective <= 40 && length blockDirective <= 40 ==>
  let content = fileDirective ++ "\n\n" ++ blockDirective ++ "\nsome code"
      parsed = parseTypus content "inheritance.typus"
  in case parsed of
    Left _ -> property $ False
    Right file -> property $ tfDirectives file /= defaultFileDirectives .&&. not (null (tfBlocks file))

-- Property: Large content processing performance
prop_large_content_processing :: Int -> String -> Property
prop_large_content_processing multiplier baseContent =
  multiplier > 0 && multiplier <= 20 && length baseContent <= 30 &&
  not (any (`isInfixOf` baseContent) ["/*", "*/"]) ==>
  let largeContent = unlines (replicate multiplier baseContent)
      processed = removeComments largeContent
      normalized = normalizeIndentation processed
  in property $ length normalized >= length baseContent

-- Helper function for pipeline operator
(|>) :: a -> (a -> b) -> b
x |> f = f x

tests :: TestTree
tests = testGroup "New Comprehensive Cabal QuickCheck Tests"
  [ fastProperty "complete parsing pipeline consistency" prop_complete_parsing_pipeline
  , fastProperty "string processing pipeline end-to-end" prop_string_processing_pipeline
  , fastProperty "source location tracking with text processing" prop_source_location_text_processing
  , fastProperty "error handling with parsing results" prop_error_handling_parsing
  , fastProperty "multi-stage processing consistency" prop_multi_stage_processing
  , fastProperty "directive parsing with various formats" prop_directive_parsing_formats
  , fastProperty "comment removal in complex scenarios" prop_complex_comment_removal
  , fastProperty "indentation normalization with mixed content" prop_mixed_indentation_normalization
  , fastProperty "error accumulation in processing pipeline" prop_error_accumulation_pipeline
  , fastProperty "splitting and joining consistency" prop_splitting_joining_consistency
  ]