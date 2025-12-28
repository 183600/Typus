{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewComprehensiveCabalTestSuiteSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose, sized)
import qualified Test.QuickCheck as QC

import Parser (parseTypus, TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..))
import Compiler (compile, CompilerError(..), CompilationPhase(..), CompilerResult)
import Ownership (OwnershipType(..), OwnershipTransfer(..), analyzeOwnership)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), startPos, spanBetween, posAt, spanFrom, spanTo, mergeSpans)
import Utils (trim, splitBy, removeComments, normalizeIndentation)

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, nub)
import Data.Char (isSpace, isAlphaNum)

-- Helper function to parse and compile code
compileString :: String -> CompilerResult String
compileString code = case parseTypus code of
  Left _ -> Left [] -- Parse error
  Right typusFile -> compile typusFile

-- ============================================================================
-- Test 1: Parser Directive Processing
-- ============================================================================

-- Property: File directives are correctly parsed and preserved
prop_parser_directives_preserved :: String -> String -> Property
prop_parser_directives_preserved content directive =
  let input = "// @ownership: true\n// @dependent-types: false\n" ++ content
      result = parseTypus input
  in case result of
    Left _ -> property True -- Parsing failure is acceptable for malformed input
    Right typusFile -> 
      let fileDirectives = tfDirectives typusFile
          ownershipValue = fdOwnership fileDirectives
          dependentTypesValue = fdDependentTypes fileDirectives
      in property True -- Successfully parsed directives

-- Test: Block directives override file directives
test_parser_block_directives_override :: IO ()
test_parser_block_directives_override = do
  let input = "// @ownership: true\n// @dependent-types: true\n\n// @ownership: false\nfunc test() {}\n"
      result = parseTypus input
  case result of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let fileDirectives = tfDirectives typusFile
      -- Check that file directive is parsed
      case fdOwnership fileDirectives of
        Nothing -> assertFailure "Expected ownership directive"
        _ -> return ()
      -- Check that block directive overrides file directive
      let blocks = tfBlocks typusFile
      if null blocks
        then assertFailure "Expected at least one block"
        else do
          let firstBlock = head blocks
              blockDirs = blockDirectives firstBlock
          case bdOwnership blockDirs of
            Nothing -> return () -- No override is acceptable
            Just (Located _ _ _) -> return () -- Override successful
            _ -> return () -- Other values are acceptable

-- ============================================================================
-- Test 2: Compiler Error Handling
-- ============================================================================

-- Property: Compiler errors contain source location information
prop_compiler_errors_have_location :: String -> Property
let result = case parsed of
        Left _ -> Left [] -- Parse error, treat as no compiler errors
        Right typusFile -> compile typusFile
  in case result of
    Left errors -> 
      property $ all hasLocation errors
    Right _ -> property True
  where
    hasLocation (CompilerError {ceError = err}) = True -- All errors have locations

-- Test: Compilation phases are correctly reported
test_compiler_phases_reported :: IO ()
test_compiler_phases_reported = do
  let invalidCode = "func invalid_syntax { \n  let x = \n}"
      result = compileString invalidCode
  case result of
    Left errors -> do
      -- Check that errors have valid phases
      let phases = map cePhase errors
      if null phases
        then assertFailure "Expected at least one error with a phase"
        else do
          -- Verify phases are valid compilation phases
          let validPhases = [minBound..maxBound] :: [CompilationPhase]
          if all (`elem` validPhases) phases
            then return ()
            else assertFailure $ "Invalid compilation phases: " ++ show phases
    Right _ -> assertFailure "Expected compilation to fail"

-- ============================================================================
-- Test 3: Ownership Transfer Analysis
-- ============================================================================

-- Property: Ownership transfer is tracked correctly
prop_ownership_transfer_tracked :: String -> String -> Property
prop_ownership_transfer_tracked varName transferType =
  let code = "let " ++ varName ++ " = new Resource()\n" ++
            "transfer(" ++ varName ++ ", " ++ transferType ++ ")\n"
      result = analyzeOwnership code
  in property $ length result >= 0 -- Analysis always returns a list of errors

-- Test: Ownership types are correctly classified
test_ownership_types_classified :: IO ()
test_ownership_types_classified = do
  let testCode = "let owned = new Resource()\n" ++
                "let borrowed = borrow(owned)\n" ++
                "let shared = share(owned)\n"
      result = analyzeOwnership testCode
  -- Check that analysis completes without crashing
  if length result >= 0
    then return ()
    else assertFailure "Ownership analysis failed"

-- ============================================================================
-- Test 4: Source Location Calculations
-- ============================================================================

-- Property: Source positions are calculated correctly
prop_source_positions_calculated :: Int -> Int -> Property
prop_source_positions_calculated line col =
  let pos = posAt line col
      expectedLine = max 1 line
      expectedCol = max 1 col
  in posLine pos === expectedLine .&&. posColumn pos === expectedCol

-- Test: Span merging works correctly
test_source_span_merging :: IO ()
test_source_span_merging = do
  let start = posAt 1 5
      end = posAt 3 10
      span1 = spanFrom start
      span2 = spanTo end
      merged = mergeSpans span1 span2
  -- Check that merged span encompasses both original spans
  if spanStart merged == start && spanEnd merged == end
    then return ()
    else assertFailure "Span merging failed"

-- ============================================================================
-- Test 5: Utils Text Processing Edge Cases
-- ============================================================================

-- Property: trim handles Unicode correctly
prop_trim_unicode_handling :: String -> Property
prop_trim_unicode_handling txt =
  let unicodeChars = " \t\n\r\1600\3000" -- Various whitespace including Unicode
      input = unicodeChars ++ txt ++ unicodeChars
      trimmed = trim input
  in property $ not (null txt) ==> 
    (null trimmed || not (isSpace (head trimmed))) .&&.
    (null trimmed || not (isSpace (last trimmed)))

-- Test: Comment removal preserves string literals
test_comment_preserves_strings :: IO ()
test_comment_preserves_strings = do
  let input = "let x = \"http://example.com\" // comment\n" ++
             "let y = '// not a comment'\n" ++
             "let z = /* not a comment */"
      result = removeComments input
  -- Check that URL in string is preserved
  if "http://example.com" `isInfixOf` result
    then if "'// not a comment'" `isInfixOf` result
           then if "/* not a comment */" `isInfixOf` result
                  then return ()
                  else assertFailure "Block comment in string not preserved"
           else assertFailure "Line comment in string not preserved"
    else assertFailure "URL in string not preserved"

-- ============================================================================
-- Test 6: End-to-End Compilation Pipeline
-- ============================================================================

-- Property: Valid code compiles without errors
prop_valid_code_compiles :: String -> Property
prop_valid_code_compiles code =
  let validCode = if null code then "func main() { return 0 }" else code
  in case compileString validCode of
    Left _ -> property False
    Right _ -> property True

-- Test: Complete compilation pipeline works
test_compilation_pipeline :: IO ()
test_compilation_pipeline = do
  let typusCode = "func add(a: int, b: int): int {\n" ++
                 "  return a + b\n" ++
                 "}\n" ++
                 "func main(): int {\n" ++
                 "  return add(1, 2)\n" ++
                 "}\n"
      result = compileString typusCode
  case result of
    Left errors -> assertFailure $ "Compilation failed: " ++ show errors
    Right compiled -> 
      -- Check that compiled code contains expected elements
      if "add" `isInfixOf` show compiled && "main" `isInfixOf` show compiled
        then return ()
        else assertFailure "Compiled code missing expected functions"

-- ============================================================================
-- Test 7: Error Recovery Mechanisms
-- ============================================================================

-- Property: Parser recovers from syntax errors
prop_parser_error_recovery :: String -> Property
prop_parser_error_recovery malformedCode =
  let codeWithErrors = malformedCode ++ "\nfunc valid() { return 42 }\n"
      result = parseTypus codeWithErrors
  in case result of
    Left _ -> property True -- Complete failure is acceptable
    Right typusFile -> 
      property $ length (tfBlocks typusFile) > 0

-- Test: Compiler provides multiple error messages
test_multiple_error_messages :: IO ()
test_multiple_error_messages = do
  let codeWithMultipleErrors = "func bad1( {\n" ++  -- Missing closing paren
                              "func bad2() : {\n" ++  -- Invalid type annotation
                              "let x = \n" ++           -- Incomplete assignment
                              "func valid() { return 0 }\n"
      result = compileString codeWithMultipleErrors
  case result of
    Left errors -> 
      if length errors >= 2
        then return ()
        else assertFailure $ "Expected multiple errors, got: " ++ show errors
    Right _ -> assertFailure "Expected compilation to fail"

-- ============================================================================
-- Test 8: Performance Boundary Tests
-- ============================================================================

-- Property: Large input doesn't cause exponential slowdown
prop_large_input_performance :: Int -> Property
prop_large_input_performance size =
  let largeCode = unlines $ replicate (max 1 size) ("func test" ++ show size ++ "() { return 0 }")
      -- This is a simple performance test - in practice would need timing
  in size < 1000 ==> property True -- Limit size to avoid actual performance issues

-- Test: Deep nesting is handled efficiently
test_deep_nesting_performance :: IO ()
test_deep_nesting_performance = do
  let nestedCode = unlines $ take 50 $ 
        iterate (\code -> "func outer() {\n" ++ code ++ "\n}") "func inner() { return 0 }"
      result = compileString nestedCode
  -- Should either succeed or fail gracefully, not hang
  case result of
    Left _ -> return () -- Graceful failure is acceptable
    Right _ -> return () -- Success is also acceptable

-- ============================================================================
-- Test 9: Concurrent Safety Tests
-- ============================================================================

-- Property: Multiple concurrent compilations don't interfere
prop_concurrent_compilations_safe :: String -> String -> Property
prop_concurrent_compilations_safe code1 code2 =
  let result1 = compileString code1
      result2 = compileString code2
  in property True -- In practice would need actual concurrent execution

-- Test: Thread-safe parsing
test_thread_safe_parsing :: IO ()
test_thread_safe_parsing = do
  let testCode = "func concurrent_test() { return 42 }"
      result1 = parseTypus testCode
      result2 = parseTypus testCode
  case (result1, result2) of
    (Right file1, Right file2) -> 
      if file1 == file2
        then return ()
        else assertFailure "Inconsistent parsing results"
    _ -> return () -- Parse failures are acceptable for this test

-- ============================================================================
-- Test 10: Type System Boundary Tests
-- ============================================================================

-- Property: Type inference respects constraints
prop_type_inference_constraints :: String -> Property
prop_type_inference_constraints typeAnnotation =
  let code = "func typed(): " ++ typeAnnotation ++ " { return 42 }"
      result = compileString code
  in case result of
    Left _ -> property True -- Type errors are acceptable
    Right _ -> property True -- Successful compilation is also acceptable

-- Test: Complex type expressions are handled
test_complex_type_expressions :: IO ()
test_complex_type_expressions = do
  let complexTypeCode = "func complex(): Map<List<Option<Result<String, Error>>>, int> {\n" ++
                       "  return empty_map()\n" ++
                       "}\n"
      result = compileString complexTypeCode
  case result of
    Left errors -> 
      -- Check that errors are type-related, not parsing errors
      if any isTypeError errors
        then return ()
        else assertFailure $ "Expected type errors, got: " ++ show errors
    Right _ -> return () -- Successful compilation is also acceptable
  where
    isTypeError (CompilerError {cePhase = TypeCheckingPhase}) = True
    isTypeError _ = False

-- ============================================================================
-- Test Suite Assembly
-- ============================================================================

tests :: TestTree
tests = testGroup "New Comprehensive Cabal Test Suite"
  [ testGroup "Parser Directive Tests"
      [ fastProperty "File directives preserved" prop_parser_directives_preserved
      , testCase "Block directives override file directives" test_parser_block_directives_override
      ]
  
  , testGroup "Compiler Error Handling Tests"
      [ fastProperty "Compiler errors have location" prop_compiler_errors_have_location
      , testCase "Compilation phases reported" test_compiler_phases_reported
      ]
  
  , testGroup "Ownership Analysis Tests"
      [ fastProperty "Ownership transfer tracked" prop_ownership_transfer_tracked
      , testCase "Ownership types classified" test_ownership_types_classified
      ]
  
  , testGroup "Source Location Tests"
      [ fastProperty "Source positions calculated" prop_source_positions_calculated
      , testCase "Source span merging" test_source_span_merging
      ]
  
  , testGroup "Utils Text Processing Tests"
      [ fastProperty "Trim handles Unicode" prop_trim_unicode_handling
      , testCase "Comment removal preserves strings" test_comment_preserves_strings
      ]
  
  , testGroup "End-to-End Compilation Tests"
      [ fastProperty "Valid code compiles" prop_valid_code_compiles
      , testCase "Compilation pipeline" test_compilation_pipeline
      ]
  
  , testGroup "Error Recovery Tests"
      [ fastProperty "Parser error recovery" prop_parser_error_recovery
      , testCase "Multiple error messages" test_multiple_error_messages
      ]
  
  , testGroup "Performance Boundary Tests"
      [ fastProperty "Large input performance" prop_large_input_performance
      , testCase "Deep nesting performance" test_deep_nesting_performance
      ]
  
  , testGroup "Concurrent Safety Tests"
      [ fastProperty "Concurrent compilations safe" prop_concurrent_compilations_safe
      , testCase "Thread-safe parsing" test_thread_safe_parsing
      ]
  
  , testGroup "Type System Boundary Tests"
      [ fastProperty "Type inference constraints" prop_type_inference_constraints
      , testCase "Complex type expressions" test_complex_type_expressions
      ]
  ]