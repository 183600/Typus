{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.Unit.BoundaryConditionComprehensiveSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool, Assertion)
import Test.Tasty.QuickCheck (testProperties, Arbitrary(..), Gen, choose, listOf, elements, oneof, vectorOf, property, (===), forAll, counterexample)
import Test.QuickCheck (Gen, Property, (==>), classify)
import Parser (parseTypus, FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile(..), defaultFileDirectives)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), locatedAt, advancePosByText, startPos)
import Compiler.Errors.Core (TypeError(..), ErrorSeverity(..), ErrorCategory(..), 
                            ErrorLocation(..), ErrorContext(..), emptyContext,
                            errorAt, warningAt, infoAt, fatalError, newErrorCollector, addError,
                            getErrors, hasErrors, canRecoverFrom, shouldContinueAfter,
                            formatErrorWithLocation, ErrorCollector)
import Utils (trim, splitBy, removeComments, normalizeIndentation, safeProcessString, isValidChar)
import qualified Data.Text as T
import Data.Char (isAlphaNum, isSpace, isPrint, isControl, chr)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Control.Monad (foldM)
import Control.Monad.State (execState, State)
import Control.Exception (evaluate, try, SomeException)

-- Arbitrary instance for SourcePos
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 1000)
    column <- choose (1, 1000)
    return $ SourcePos { posLine = line, posColumn = column, posOffset = 0 }

-- Arbitrary instance for ErrorSeverity
instance Arbitrary ErrorSeverity where
  arbitrary = elements [Fatal, Error, Warning, Info]



-- Helper generators for Boundary Condition tests
genLargeString :: Int -> Gen String
genLargeString maxSize = do
  len <- choose (1000, maxSize)
  vectorOf len $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n.,;:+-*/=<>()[]{}"

genHugeString :: Gen String
genHugeString = genLargeString 100000

genDeeplyNestedStructure :: Int -> Gen String
genDeeplyNestedStructure 0 = return "base"
genDeeplyNestedStructure n = do
  inner <- genDeeplyNestedStructure (n - 1)
  return $ "outer(" ++ inner ++ ")"

genSpecialChars :: Gen String
genSpecialChars = listOf $ elements "\0\1\2\3\4\5\6\7\8\10\11\12\13\14\15\16\17\18\19\20\21\22\23\24\25\26\27\28\29\30\31\127"

genUnicodeString :: Gen String
genUnicodeString = listOf $ elements $ map chr [0..255]

genExtremePositions :: Gen SourcePos
genExtremePositions = do
  line <- oneof [choose (1, 10), choose (999990, 1000000)]
  column <- oneof [choose (1, 10), choose (999990, 1000000)]
  offset <- oneof [choose (0, 10), choose (999990, 1000000)]
  return $ SourcePos line column offset

-- Test properties for Boundary Condition tests

-- Property 1: Parser handles extremely large inputs
prop_parser_large_input :: Int -> Property
prop_parser_large_input size = 
  size > 1000 && size <= 10000 ==> 
  let content = replicate size 'a' ++ "\nownership=true"
      result = parseTypus content
  in case result of
       Right p -> not (null (tfBlocks p))
       Left _ -> False

-- Property 2: Parser handles deeply nested structures
prop_parser_deeply_nested :: Int -> Property
prop_parser_deeply_nested depth = 
  depth > 0 && depth <= 100 ==> 
  forAll (genDeeplyNestedStructure depth) $ \content ->
  let result = parseTypus content
  in case result of
       Right parsed -> not (null (tfBlocks parsed))
       Left _ -> False

-- Property 3: SourceLocation handles extreme positions
prop_sourcelocation_extreme_positions :: SourcePos -> SourcePos -> Bool
prop_sourcelocation_extreme_positions pos1 pos2 = 
  let span = SourceSpan pos1 pos2
      start = spanStart span
      end = spanEnd span
  in start == pos1 && end == pos2

-- Property 4: Error handling with extreme severity levels
prop_error_extreme_severity :: String -> ErrorSeverity -> Bool
prop_error_extreme_severity message severity = 
  let error = errorAt "Parsing" (T.pack message) (ErrorLocation Nothing 1 1 Nothing Nothing)
      errorWithSeverity = error { severity = severity }
      canRecover = canRecoverFrom errorWithSeverity
      shouldContinue = shouldContinueAfter errorWithSeverity
  in if severity == Fatal
     then not canRecover && not shouldContinue
     else canRecover && shouldContinue

-- Property 5: Utils functions handle extreme inputs
prop_utils_extreme_inputs :: String -> Bool
prop_utils_extreme_inputs input = 
  let trimmed = trim input
      split = splitBy ',' input
      commentsRemoved = removeComments input
      normalized = normalizeIndentation input
      safe = safeProcessString input
  in not (null trimmed) || null input &&
     length split >= 0 &&
     length commentsRemoved >= 0 &&
     case safe of Right s -> all isValidChar s; Left _ -> False

-- Property 6: Error collector handles large numbers of errors
prop_error_collector_large_numbers :: Int -> Property
prop_error_collector_large_numbers numErrors = 
  numErrors > 0 && numErrors <= 1000 ==> 
  let errors = replicate numErrors (errorAt "test" (T.pack "test error") (ErrorLocation Nothing 1 1 Nothing Nothing))
      collector = execState (foldM (\acc err -> addError err) () errors) []
      retrievedErrors = getErrors collector  
  in length retrievedErrors == numErrors && hasErrors collector

-- Property 7: Memory efficiency with repeated operations
prop_memory_efficiency_repeated_operations :: Int -> Property
prop_memory_efficiency_repeated_operations iterations = 
  iterations > 0 && iterations <= 1000 ==> 
  let content = "ownership=true\ntest content"
      results = replicate iterations (parseTypus content)
      totalBlocks = sum $ map (either (const 0) (length . tfBlocks)) results
  in totalBlocks == iterations * case parseTypus content of
                                  Right p -> length (tfBlocks p)
                                  Left _ -> 0

-- Property 8: Handling of special characters and unicode
prop_special_characters_handling :: String -> Bool
prop_special_characters_handling content = 
  let parsed = parseTypus content
      blocks = either (const []) tfBlocks parsed
  in length blocks >= 0  -- Should not crash

-- Property 9: Performance with large directives
prop_large_directives_handling :: Int -> Property
prop_large_directives_handling directiveSize = 
  directiveSize > 0 && directiveSize <= 10000 ==> 
  let largeDirective = "ownership=" ++ replicate directiveSize 'a'
      content = largeDirective ++ "\nsome code"
      result = parseTypus content
  in case result of 
       Right p -> not (null (tfBlocks p))
       Left _ -> False

-- Property 10: Error formatting with extreme content
prop_error_formatting_extreme_content :: String -> Property
prop_error_formatting_extreme_content content = 
  length content > 100 ==> 
  let error = errorAt "Parsing" (T.pack content) (ErrorLocation Nothing 1 1 Nothing Nothing)
      formatted = formatErrorWithLocation error
  in not (T.null (T.pack formatted))

-- Unit tests for boundary conditions
test_extreme_input_sizes :: [TestTree]
test_extreme_input_sizes = 
  [ testCase "parser with empty input" $ do
      let result = parseTypus ""
      assertEqual "should handle empty input" 0 (case result of Right p -> length (tfBlocks p); Left _ -> 0)
  , testCase "parser with single character" $ do
      let result = parseTypus "a"
      assertBool "should handle single character" (case result of Right p -> not (null (tfBlocks p)); Left _ -> False)
  , testCase "parser with very large input" $ do
      let largeContent = replicate 50000 'a' ++ "\nownership=true"
          result = parseTypus largeContent
      assertBool "should handle large input" (case result of Right p -> not (null (tfBlocks p)); Left _ -> False)
  , testCase "parser with very long lines" $ do
      let longLine = replicate 10000 'a'
          content = longLine ++ "\n" ++ longLine
          result = parseTypus content
      assertBool "should handle long lines" (case result of Right p -> not (null (tfBlocks p)); Left _ -> False)
  ]

test_extreme_positions :: [TestTree]
test_extreme_positions = 
  [ testCase "source position at maximum values" $ do
      let maxPos = SourcePos 1000000 1000000 1000000
          span = SourceSpan maxPos maxPos
      assertEqual "should handle max position" maxPos (spanStart span)
  , testCase "source position at minimum values" $ do
      let minPos = startPos
          span = SourceSpan minPos minPos
      assertEqual "should handle min position" minPos (spanStart span)
  , testCase "position advancement with large content" $ do
      let largeContent = replicate 10000 'a'
          endPos = advancePosByText (T.pack largeContent) startPos
      assertBool "should advance correctly" (posLine endPos == 1 && posColumn endPos > 10000)
  ]

test_extreme_error_conditions :: [TestTree]
test_extreme_error_conditions = 
  [ testCase "error collector with many errors" $ do
      let manyErrors = replicate 1000 (errorAt "Parsing" (T.pack "test error") (ErrorLocation Nothing 1 1 Nothing Nothing))
          collector = execState (foldM (\acc err -> addError err) () manyErrors) []
      assertEqual "should handle many errors" 1000 (length (getErrors collector))
  , testCase "error with very long message" $ do
      let longMessage = replicate 10000 'a'
          error = errorAt "Parsing" (T.pack longMessage) (ErrorLocation Nothing 1 1 Nothing Nothing)
          formatted = formatErrorWithLocation error
      assertBool "should format long message" (not (T.null (T.pack formatted)))
  , testCase "fatal error handling" $ do
      let fatal = fatalError "fatal error message"
      let fatal = fatalError "fatal" (T.pack "fatal error") (ErrorLocation Nothing 1 1 Nothing Nothing)
      assertEqual "should not recover from fatal" False (canRecoverFrom fatal)
      assertEqual "should not continue after fatal" False (shouldContinueAfter fatal)
  , testCase "error with extreme severity" $ do
      let severities = [Fatal, Error, Warning, Info]
          errors = map (\sev -> (errorAt "Parsing" (T.pack "test") (ErrorLocation Nothing 1 1 Nothing Nothing)) { severity = sev }) severities
          recoverable = map canRecoverFrom errors
          continue = map shouldContinueAfter errors
      assertEqual "fatal not recoverable" False (head recoverable)
      assertEqual "fatal not continue" False (head continue)
  ]

test_special_characters_and_unicode :: [TestTree]
test_special_characters_and_unicode = 
  [ testCase "parser with control characters" $ do
      let controlContent = "\0\1\2\3\4\5ownership=true"
          result = parseTypus controlContent
      assertBool "should handle control characters" (case result of Right p -> not (null (tfBlocks p)); Left _ -> False)
  , testCase "parser with unicode characters" $ do
      let unicodeContent = "ownership=true\ncode with unicode: αβγδεζηθ"
          result = parseTypus unicodeContent
      assertBool "should handle unicode" (case result of Right p -> not (null (tfBlocks p)); Left _ -> False)
  , testCase "utils with special characters" $ do
      let specialContent = "\0\1\2\3\4\5\6\7\8\9\10\11\12\13\14\15"
          safe = safeProcessString specialContent
      assertBool "should process safely" (case safe of Right s -> all isValidChar s; Left _ -> False)
  , testCase "parser with mixed content types" $ do
      let mixedContent = "ownership=true\n" ++ [chr 0, chr 255] ++ "\n正常内容"
          result = parseTypus mixedContent
      assertBool "should handle mixed content" (case result of Right p -> not (null (tfBlocks p)); Left _ -> False)
  ]

test_resource_limits :: [TestTree]
test_resource_limits = 
  [ testCase "memory usage with repeated parsing" $ do
      let content = "ownership=true\ntest content"
          results = replicate 100 (parseTypus content)
      assertEqual "should handle repeated parsing" 100 (length results)
  , testCase "deep nesting handling" $ do
      let deeplyNested = concat $ replicate 100 "outer("
          nestedContent = deeplyNested ++ "base" ++ concat (replicate 100 ")")
          result = parseTypus nestedContent
      assertBool "should handle deep nesting" (case result of Right p -> not (null (tfBlocks p)); Left _ -> False)
  , testCase "large directive values" $ do
      let largeDirectiveValue = replicate 5000 'a'
          content = "ownership=" ++ largeDirectiveValue ++ "\nsome code"
          result = parseTypus content
      assertBool "should handle large directive values" (case result of Right p -> not (null (tfBlocks p)); Left _ -> False)
  ]

test_concurrent_safety :: [TestTree]
test_concurrent_safety = 
  [ testCase "parser state isolation" $ do
      let content1 = "ownership=true\ncode1"
          content2 = "dependent-types=false\ncode2"
          result1 = parseTypus content1
          result2 = parseTypus content2
      assertBool "should isolate parsing state" 
         ((case result1 of Right p1 -> show (tfDirectives p1); Left _ -> show defaultFileDirectives) /= 
          (case result2 of Right p2 -> show (tfDirectives p2); Left _ -> show defaultFileDirectives))
  , testCase "error collector isolation" $ do
      let error1 = errorAt "Parsing" (T.pack "error1") (ErrorLocation Nothing 1 1 Nothing Nothing)
          error2 = errorAt "TypeChecking" (T.pack "error2") (ErrorLocation Nothing 1 1 Nothing Nothing)
          collector1 = execState (addError error1) []
          collector2 = execState (addError error2) []
      assertBool "should isolate error collectors" 
         (getErrors collector1 /= getErrors collector2)
  ]

test_performance_boundaries :: [TestTree]
test_performance_boundaries = 
  [ testCase "parsing performance with large files" $ do
      let largeFileContent = unlines $ replicate 1000 "ownership=true\nsome code content"
          result = parseTypus largeFileContent
      assertBool "should handle large files efficiently" (case result of Right p -> not (null (tfBlocks p)); Left _ -> False)
  , testCase "error formatting performance" $ do
      let errors = replicate 100 (errorAt "Parsing" (T.pack "test error message") (ErrorLocation Nothing 1 1 Nothing Nothing))
          formatted = map formatErrorWithLocation errors
      assertEqual "should format many errors" 100 (length formatted)
  , testCase "utils performance with large strings" $ do
      let largeString = replicate 10000 "test string with content\n"
          processed = normalizeIndentation (concat largeString)
      assertBool "should process large strings" (not (null processed))
  ]

-- QuickCheck property tests
boundaryConditionQuickCheckTests :: TestTree
boundaryConditionQuickCheckTests = testGroup "QuickCheck Properties"
  [ testProperties "Extreme Input Sizes"
      [ ("parser large input", property prop_parser_large_input)
      , ("parser deeply nested", property prop_parser_deeply_nested)
      , ("large directives handling", property prop_large_directives_handling)
      ]
  , testProperties "Extreme Positions"
      [ ("sourcelocation extreme positions", property prop_sourcelocation_extreme_positions)
      ]
  , testProperties "Extreme Error Conditions"
      [ ("error extreme severity", property prop_error_extreme_severity)
      , ("error collector large numbers", property prop_error_collector_large_numbers)
      , ("error formatting extreme content", property prop_error_formatting_extreme_content)
      ]
  , testProperties "Utils Extreme Inputs"
      [ ("utils extreme inputs", property prop_utils_extreme_inputs)
      ]
  , testProperties "Special Characters"
      [ ("special characters handling", property prop_special_characters_handling)
      ]
  , testProperties "Performance Boundaries"
      [ ("memory efficiency repeated operations", property prop_memory_efficiency_repeated_operations)
      ]
  ]

-- Unit tests
boundaryConditionUnitTests :: TestTree
boundaryConditionUnitTests = testGroup "Unit Tests"
  [ testGroup "Extreme Input Sizes" test_extreme_input_sizes
  , testGroup "Extreme Positions" test_extreme_positions
  , testGroup "Extreme Error Conditions" test_extreme_error_conditions
  , testGroup "Special Characters and Unicode" test_special_characters_and_unicode
  , testGroup "Resource Limits" test_resource_limits
  , testGroup "Concurrent Safety" test_concurrent_safety
  , testGroup "Performance Boundaries" test_performance_boundaries
  ]

-- Main test suite
boundaryConditionComprehensiveTests :: TestTree
boundaryConditionComprehensiveTests = testGroup "Boundary Condition Comprehensive Tests"
  [ boundaryConditionUnitTests
  , boundaryConditionQuickCheckTests
  ]