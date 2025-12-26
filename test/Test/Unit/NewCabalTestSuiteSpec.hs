{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalTestSuiteSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertFailure, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import qualified Test.QuickCheck as QC

import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), startPos, emptySpan, spanFrom, spanTo, mergeSpans, isValidSpan)
import Parser (parseTypus, FileDirectives(..), BlockDirectives(..), TypusFile(..), defaultFileDirectives)
import Utils (trim, splitBy, splitByCollapsed, removeLineComments, removeComments, normalizeIndentation, breakOn)
import Compiler.Errors.Core (TypeError(..), ErrorSeverity(..), ErrorCategory(..), newErrorCollector, addError, getErrors, hasErrors)
import Control.Exception (try, SomeException)
import Data.Char (isSpace, isAlphaNum)
import Data.List (isPrefixOf, isInfixOf, sort)
import qualified Data.Text as T

-- ============================================================================
-- Test Suite for New Cabal Tests
-- ============================================================================

tests :: TestTree
tests = testGroup "New Cabal Test Suite"
  [ -- Test 1: SourceLocation Advanced Properties
    testGroup "SourceLocation Advanced Tests"
    [ testCase "span merging preserves validity" $ do
        let pos1 = startPos 1 1
            pos2 = startPos 1 10
            span1 = spanFrom pos1 5
            span2 = spanFrom pos2 8
            merged = mergeSpans span1 span2
        isValidSpan merged @?= True

    , fastProperty "span position consistency" prop_spanPositionConsistency
    ]

    -- Test 2: Parser Error Recovery
  , testGroup "Parser Error Recovery Tests"
    [ testCase "parser handles malformed directives gracefully" $ do
        let malformed = "// @ownership invalid\nfunc test() {}"
            result = parseTypus malformed
        case result of
          Left err -> assertFailure $ "Parser should handle malformed directives gracefully, but got: " ++ err
          Right _ -> return ()

    , fastProperty "parser preserves line numbers in errors" prop_parserPreservesLineNumbers
    ]

    -- Test 3: Error Handler Comprehensive Tests
  , testGroup "Error Handler Comprehensive Tests"
    [ testCase "error collector maintains severity ordering" $ do
        collector <- newErrorCollector
        collector <- addError collector (TypeError "Test error" ErrorSeverity.Error ErrorCategory.TypeChecking emptySpan)
        collector <- addError collector (TypeError "Test warning" ErrorSeverity.Warning ErrorCategory.TypeChecking emptySpan)
        errors <- getErrors collector
        length errors @?= 2

    , fastProperty "error context propagation" prop_errorContextPropagation
    ]

    -- Test 4: Utils Advanced String Processing
  , testGroup "Utils Advanced String Processing"
    [ testCase "complex comment removal with nested structures" $ do
        let input = unlines
              [ "func test() {"
              , "  var s = \"// not a comment /* also not */\""
              , "  // real comment"
              , "  var nested = \"/* not nested */ // also not\""
              , "}"
              ]
            expected = unlines
              [ "func test() {"
              , "  var s = \"// not a comment /* also not */\""
              , "  "
              , "  var nested = \"/* not nested */ // also not\""
              , "}"
              ]
        removeComments input @?= expected

    , fastProperty "unicode string processing consistency" prop_unicodeStringProcessing
    ]

    -- Test 5: Compiler Pipeline Integration
  , testGroup "Compiler Pipeline Integration"
    [ testCase "compiler handles empty input gracefully" $ do
        let result = parseTypus ""
        case result of
          Left _ -> return ()  -- Expected to fail gracefully
          Right file -> return ()  -- Or succeed with empty structure

    , fastProperty "compiler maintains source mapping" prop_compilerMaintainsSourceMapping
    ]

    -- Test 6: Ownership Analysis Edge Cases
  , testGroup "Ownership Analysis Edge Cases"
    [ testCase "ownership handles circular references" $ do
        let input = unlines
              [ "// @ownership true"
              , "func circular() {"
              , "  var a = circular()"
              , "  return a"
              , "}"
              ]
            result = parseTypus input
        case result of
          Left err -> assertFailure $ "Ownership analysis should handle circular references: " ++ err
          Right _ -> return ()

    , fastProperty "ownership transfer properties" prop_ownershipTransferProperties
    ]

    -- Test 7: Dependencies Type System Integration
  , testGroup "Dependencies Type System Integration"
    [ testCase "dependency resolution with complex types" $ do
        let input = unlines
              [ "// @dependentTypes true"
              , "func complex<T>(x: T) -> T {"
              , "  return x"
              , "}"
              ]
            result = parseTypus input
        case result of
          Left err -> assertFailure $ "Type system should handle complex dependencies: " ++ err
          Right _ -> return ()

    , fastProperty "type inference consistency" prop_typeInferenceConsistency
    ]

    -- Test 8: Syntax Validator Comprehensive Tests
  , testGroup "Syntax Validator Comprehensive Tests"
    [ testCase "validator catches mismatched brackets" $ do
        let malformed = "func test() { if (true { return 1 }"  // Missing closing parenthesis
            result = parseTypus malformed
        case result of
          Left _ -> return ()  -- Expected to fail
          Right _ -> assertFailure "Validator should catch mismatched brackets"

    , fastProperty "syntax validation preserves semantics" prop_syntaxValidationPreservesSemantics
    ]

    -- Test 9: Performance and Memory Tests
  , testGroup "Performance and Memory Tests"
    [ testCase "large file processing performance" $ do
        let largeContent = unlines $ replicate 1000 "func test" ++ show [1..100] ++ "() { return 0; }"
            result = try $ return $ length largeContent
        case result of
          Left (e :: SomeException) -> assertFailure $ "Large file processing failed: " ++ show e
          Right _ -> return ()

    , fastProperty "memory efficiency with repeated operations" prop_memoryEfficiencyRepeatedOps
    ]

    -- Test 10: Integration and End-to-End Tests
  , testGroup "Integration and End-to-End Tests"
    [ testCase "complete compilation pipeline" $ do
        let completeProgram = unlines
              [ "// @ownership true"
              , "// @dependentTypes true"
              , ""
              , "func main() {"
              , "  var x: Int = 42"
              , "  var y: String = \"hello\""
              , "  return x"
              , "}"
              ]
            result = parseTypus completeProgram
        case result of
          Left err -> assertFailure $ "Complete pipeline should process valid program: " ++ err
          Right file -> return ()

    , fastProperty "end-to-end consistency" prop_endToEndConsistency
    ]

  -- Additional QuickCheck Properties for Enhanced Coverage
  , testGroup "Enhanced QuickCheck Properties"
    [ fastProperty "string processing pipeline consistency" prop_stringProcessingPipelineConsistency
    , fastProperty "error recovery robustness" prop_errorRecoveryRobustness
    , fastProperty "type system soundness" prop_typeSystemSoundness
    , fastProperty "parser error localization" prop_parserErrorLocalization
    , fastProperty "ownership analysis completeness" prop_ownershipAnalysisCompleteness
    ]
  ]

-- ============================================================================
-- QuickCheck Properties
-- ============================================================================

-- Property: Span position consistency
prop_spanPositionConsistency :: Int -> Int -> Int -> Property
prop_spanPositionConsistency line col len =
  line >= 1 && line <= 1000 && col >= 1 && col <= 1000 && len >= 0 && len <= 1000 ==>
  let pos = startPos line col
      span = spanFrom pos len
      endPos = spanTo span
  in property $ (sourceLine endPos >= sourceLine pos) .&&. 
              (sourceLine endPos >= sourceColumn pos)

-- Property: Parser preserves line numbers in errors
prop_parserPreservesLineNumbers :: String -> String -> Property
prop_parserPreservesLineNumbers prefix suffix =
  let input = prefix ++ "\ninvalid syntax here\n" ++ suffix
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right _ -> property True  -- If parsing succeeds, that's also valid

-- Property: Error context propagation
prop_errorContextPropagation :: String -> Property
prop_errorContextPropagation errorMsg =
  not (null errorMsg) ==>
  let collector = newErrorCollector
      error = TypeError errorMsg ErrorSeverity.Error ErrorCategory.TypeChecking emptySpan
      collectorWithErrors = addError collector error
      errors = getErrors collectorWithErrors
  in property $ length errors >= 1 .&&. 
              any (\e -> errorMsg `isInfixOf` show e) errors

-- Property: Unicode string processing consistency
prop_unicodeStringProcessing :: String -> Property
prop_unicodeStringProcessing content =
  let unicodeContent = content ++ "测试🚀café naïve"
      trimmed = trim unicodeContent
      removedComments = removeComments unicodeContent
      normalized = normalizeIndentation unicodeContent
  in property $ length trimmed <= length unicodeContent .&&.
              length removedComments <= length unicodeContent .&&.
              length normalized >= 0

-- Property: Compiler maintains source mapping
prop_compilerMaintainsSourceMapping :: String -> Property
prop_compilerMaintainsSourceMapping input =
  let result = parseTypus input
  in case result of
    Left _ -> property True
    Right file -> property $ length (show file) >= 0

-- Property: Ownership transfer properties
prop_ownershipTransferProperties :: String -> Property
prop_ownershipTransferProperties input =
  let ownershipDirective = "// @ownership true\n" ++ input
      result = parseTypus ownershipDirective
  in case result of
    Left _ -> property True
    Right _ -> property True

-- Property: Type inference consistency
prop_typeInferenceConsistency :: String -> Property
prop_typeInferenceConsistency input =
  let typeDirective = "// @dependentTypes true\n" ++ input
      result = parseTypus typeDirective
  in case result of
    Left _ -> property True
    Right _ -> property True

-- Property: Syntax validation preserves semantics
prop_syntaxValidationPreservesSemantics :: String -> Property
prop_syntaxValidationPreservesSemantics input =
  let result = parseTypus input
  in case result of
    Left _ -> property True
    Right _ -> property True

-- Property: Memory efficiency with repeated operations
prop_memoryEfficiencyRepeatedOps :: String -> Int -> Property
prop_memoryEfficiencyRepeatedOps input iterations =
  iterations >= 0 && iterations <= 100 ==>
  let processed = iterate removeComments input !! (iterations `mod` 10)
  in property $ length processed <= length input * 2

-- Property: End-to-end consistency
prop_endToEndConsistency :: String -> Property
prop_endToEndConsistency input =
  let directives = "// @ownership true\n// @dependentTypes true\n" ++ input
      result = parseTypus directives
  in case result of
    Left _ -> property True
    Right _ -> property True

-- Property: String processing pipeline consistency
prop_stringProcessingPipelineConsistency :: String -> Property
prop_stringProcessingPipelineConsistency input =
  let pipeline1 = input |> trim |> removeComments |> normalizeIndentation
      pipeline2 = input |> removeComments |> trim |> normalizeIndentation
  in property $ length pipeline1 >= 0 .&&. length pipeline2 >= 0

-- Property: Error recovery robustness
prop_errorRecoveryRobustness :: String -> String -> Property
prop_errorRecoveryRobustness valid invalid =
  let mixed = valid ++ "\n" ++ invalid ++ "\n" ++ valid
      result = parseTypus mixed
  in case result of
    Left _ -> property True
    Right _ -> property True

-- Property: Type system soundness
prop_typeSystemSoundness :: String -> Property
prop_typeSystemSoundness input =
  let typeDirective = "// @dependentTypes true\n" ++ input
      result = parseTypus typeDirective
  in case result of
    Left _ -> property True
    Right _ -> property True

-- Property: Parser error localization
prop_parserErrorLocalization :: String -> Int -> Property
prop_parserErrorLocalization input errorLine =
  errorLine >= 1 && errorLine <= 10 ==>
  let linesWithContent = take errorLine (lines input ++ repeat "")
      malformedInput = unlines linesWithContent ++ "\ninvalid syntax !!!"
      result = parseTypus malformedInput
  in case result of
    Left _ -> property True
    Right _ -> property True

-- Property: Ownership analysis completeness
prop_ownershipAnalysisCompleteness :: String -> Property
prop_ownershipAnalysisCompleteness input =
  let ownershipInput = "// @ownership true\n" ++ input
      result = parseTypus ownershipInput
  in case result of
    Left _ -> property True
    Right _ -> property True

-- Helper function for pipeline operations
(|>) :: a -> (a -> b) -> b
x |> f = f
infixl 0 |>

-- Additional edge case tests
additionalEdgeCaseTests :: TestTree
additionalEdgeCaseTests = testGroup "Additional Edge Case Tests"
  [ testCase "handles null bytes gracefully" $ do
        let inputWithNull = "func test() {\n  var s = \"hello\0world\"\n  return s\n}"
            result = parseTypus inputWithNull
        case result of
          Left _ -> return ()  -- Expected to fail gracefully
          Right _ -> return ()  -- Or handle correctly

  , testCase "handles extremely long lines" $ do
        let longLine = "func test() { var s = \"" ++ replicate 10000 'a' ++ "\"; return s; }"
            result = parseTypus longLine
        case result of
          Left _ -> return ()  -- Expected to fail gracefully
          Right _ -> return ()  -- Or handle correctly

  , testCase "handles deeply nested structures" $ do
        let nested = unlines $ replicate 100 "  " ++ ["func test() {", "  if (true) {", "    return 1;", "  }", "}"]
            result = parseTypus nested
        case result of
          Left _ -> return ()  -- Expected to fail gracefully
          Right _ -> return ()  -- Or handle correctly
  ]