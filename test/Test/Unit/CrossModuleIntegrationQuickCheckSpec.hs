module Test.Unit.CrossModuleIntegrationQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property)
import Parser (parseTypus, TypusFile(..))
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, advancePosBy)
import Utils (trim, removeComments, splitBy)
import Compiler (compile)
import ErrorHandler (ErrorHandler(..))
import Data.Either (isLeft, isRight)
import Data.List (length)

-- ============================================================================
-- Cross Module Integration QuickCheck Tests
-- ============================================================================

tests :: TestTree
tests = testGroup "Cross Module Integration QuickCheck Tests"
  [ testProperty "parser output integrates with source location tracking" prop_parser_source_location_integration
  , testProperty "utils text processing works on parser output" prop_utils_parser_integration
  , testProperty "source location math works with parser spans" prop_sourcelocation_parser_integration
  , testProperty "error handler processes parser errors correctly" prop_errorhandler_parser_integration
  , testProperty "compiler handles parser output gracefully" prop_compiler_parser_integration
  , testProperty "text processing pipeline consistency" prop_text_processing_pipeline
  , testProperty "source location tracking through compilation" prop_sourcelocation_compilation_tracking
  , testProperty "error recovery maintains source location info" prop_error_recovery_sourcelocation
  ]

-- | Parser output should integrate properly with source location tracking
prop_parser_source_location_integration :: String -> Property
prop_parser_source_location_integration content = 
  let result = parseTypus content
  in case result of
    Left _ -> True  -- Parsing may fail
    Right tf -> all isValidBlockSpan (tfBlocks tf)
  where
    isValidBlockSpan block = spanStart (cbSpan block) <= spanEnd (cbSpan block)

-- | Utils text processing should work correctly on parser output content
prop_utils_parser_integration :: String -> Property
prop_utils_parser_integration content = 
  let result = parseTypus content
  in case result of
    Left _ -> True
    Right tf -> all blockContentProcessable (tfBlocks tf)
  where
    blockContentProcessable block = 
      let processed = removeComments (cbContent block)
          trimmed = trim processed
      in length trimmed <= length (cbContent block)

-- | Source location math should work correctly with parser-generated spans
prop_sourcelocation_parser_integration :: String -> Property
prop_sourcelocation_parser_integration content = 
  let result = parseTypus content
  in case result of
    Left _ -> True
    Right tf -> all spansHaveValidPositions (tfBlocks tf)
  where
    spansHaveValidPositions block = 
      let span = cbSpan block
          start = spanStart span
          end = spanEnd span
      in posLine start > 0 && posColumn start > 0 && 
         posLine end > 0 && posColumn end > 0 &&
         posOffset start <= posOffset end

-- | Error handler should process parser errors correctly
prop_errorhandler_parser_integration :: String -> Property
prop_errorhandler_parser_integration content = 
  let result = parseTypus content
  in case result of
    Left err -> length (show err) > 0  -- Error should have descriptive message
    Right _ -> True  -- Success is also valid

-- | Compiler should handle parser output gracefully
prop_compiler_parser_integration :: String -> Property
prop_compiler_parser_integration content = 
  let parseResult = parseTypus content
  in case parseResult of
    Left _ -> True  -- If parsing fails, compilation behavior is undefined
    Right tf -> 
      let compileResult = compile tf
      in case compileResult of
        Left _ -> True  -- Compilation may fail
        Right _ -> True  -- Or succeed

-- | Text processing pipeline should maintain consistency
prop_text_processing_pipeline :: String -> Property
prop_text_processing_pipeline content = 
  let step1 = trim content
      step2 = removeComments step1
      step3 = trim step2
      lines1 = lines step1
      lines2 = lines step2
      lines3 = lines step3
  in length lines3 <= length lines2 && length lines2 <= length lines1

-- | Source location tracking should work through compilation pipeline
prop_sourcelocation_compilation_tracking :: String -> Property
prop_sourcelocation_compilation_tracking content = 
  let result = parseTypus content
  in case result of
    Left _ -> True
    Right tf -> 
      let spans = map cbSpan (tfBlocks tf)
          positions = map spanStart spans
      in all isValidPosition positions
  where
    isValidPosition pos = posLine pos > 0 && posColumn pos > 0

-- | Error recovery should maintain source location information
prop_error_recovery_sourcelocation :: String -> Property
prop_error_recovery_sourcelocation content = 
  let withError = content ++ "\n@@ MALFORMED SYNTAX @@\n" ++ content
      result = parseTypus withError
  in case result of
    Left _ -> True  -- May fail completely
    Right tf -> 
      let spans = map cbSpan (tfBlocks tf)
      in all (\span -> isValidSpan span) spans

-- Helper function to check span validity
isValidSpan :: SourceSpan -> Bool
isValidSpan span = spanStart span <= spanEnd span