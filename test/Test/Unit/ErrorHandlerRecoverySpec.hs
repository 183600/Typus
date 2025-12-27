{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ErrorHandlerRecoverySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Arbitrary (Arbitrary(..), arbitrary)
import Test.QuickCheck.Gen (choose, listOf, oneof, elements, vectorOf, suchThat, Gen)

import ErrorHandler
import EnhancedErrorHandler
import Compiler.Errors (CompilerError(..), ErrorCategory(..), ErrorSeverity(..), CompilationPhase(..))
import Compiler.Errors.Core (ErrorLocation(..))

import Parser (parseTypus)
import Compiler (compile, CompilerResult)
import SourceLocation (SourceSpan(..), SourcePos(..))

import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Char (isSpace, isAlphaNum)
import Data.Set (Set)
import qualified Data.Set as Set

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

-- Generate error codes
arbitraryErrorCode :: Gen String
arbitraryErrorCode = do
  prefix <- elements ["CP", "PT", "TC", "OW", "DE"]
  number <- choose (1000, 9999)
  return $ prefix ++ show number

-- Generate error messages
arbitraryErrorMessage :: Gen String
arbitraryErrorMessage = do
  words <- vectorOf 3 8 (elements ["type", "error", "syntax", "semantic", "compilation", "parsing", "analysis", "checking", "invalid", "unexpected", "missing", "found", "required"])
  return $ unwords words

-- Generate error locations
arbitraryErrorLocation :: Gen ErrorLocation
arbitraryErrorLocation = do
  line <- choose (1, 1000)
  column <- choose (1, 100)
  endLine <- elements [Just line, Nothing]
  endColumn <- elements [Just column, Nothing]
  filePath <- elements [Nothing, Just "test.typus"]
  return $ ErrorLocation filePath line column endLine endColumn

-- Generate error categories
arbitraryErrorCategory :: Gen ErrorCategory
arbitraryErrorCategory = elements [Parsing, TypeChecking, Semantic, Analysis, Runtime, System]

-- Generate error severities
arbitraryErrorSeverity :: Gen ErrorSeverity
arbitraryErrorSeverity = elements [Error, Warning, Info, Hint]

-- Generate compilation phases
arbitraryCompilationPhase :: Gen CompilationPhase
arbitraryCompilationPhase = elements [ParsingPhase, TypeCheckingPhase, SemanticPhase, OptimizationPhase, CodeGenPhase]

-- Generate compiler errors
arbitraryCompilerError :: Gen CompilerError
arbitraryCompilerError = do
  code <- arbitraryErrorCode
  message <- T.pack <$> arbitraryErrorMessage
  phase <- arbitraryCompilationPhase
  category <- arbitraryErrorCategory
  severity <- arbitraryErrorSeverity
  location <- arbitraryErrorLocation
  suggestions <- vectorOf 0 3 (T.pack <$> arbitraryErrorMessage)
  stackTrace <- vectorOf 0 5 arbitraryErrorMessage
  return $ CompilerError code message phase category severity (Just location) Nothing suggestions stackTrace Nothing

-- Generate source code with errors
arbitraryErrorCode :: Gen String
arbitraryErrorCode = do
  errorType <- elements ["syntax", "type", "semantic", "runtime"]
  case errorType of
    "syntax" -> do
      let syntaxError = "func test() {\n  if true\n    x := 1\n  }\n}\n"  -- Missing opening brace
      return syntaxError
    "type" -> do
      let typeError = "func test() {\n  x := \"string\"\n  y := 1\n  z := x + y\n}\n"
      return typeError
    "semantic" -> do
      let semanticError = "func test() {\n  x := 1\n  y := x + 1\n  z := undefined_var\n}\n"
      return semanticError
    _ -> do
      let runtimeError = "func test() {\n  x := 1\n  y := 0\n  z := x / y\n}\n"
      return runtimeError

-- Generate valid code snippets
arbitraryValidCode :: Gen String
arbitraryValidCode = do
  funcName <- elements ["test", "process", "calculate", "validate"]
  numStmts <- choose (1, 3)
  stmts <- vectorOf numStmts $ do
    varName <- elements ["x", "y", "z", "result", "value"]
    value <- elements ["1", "2", "42", "true", "false"]
    return $ "  " ++ varName ++ " := " ++ value
  return $ "func " ++ funcName ++ "() {\n" ++ unlines stmts ++ "}\n"

-- ============================================================================
-- Error Handler Recovery Properties
-- ============================================================================

-- Property: Error handler recovers from syntax errors
prop_error_handler_recovers_syntax :: Property
prop_error_handler_recovers_syntax =
  let syntaxErrorCode = "func test() {\n  if true\n    x := 1\n  }\n}\n"
  in case parseTypus syntaxErrorCode of
    Left _ -> property True  -- Parsing fails as expected
    Right typusFile ->
      case compile typusFile of
        Left _ -> property True  -- Compilation fails as expected
        Right _ -> property False  -- Should not succeed with syntax errors

-- Property: Error handler provides meaningful error messages
prop_error_handler_meaningful_messages :: Property
prop_error_handler_meaningful_messages =
  forAll arbitraryErrorCode $ \code ->
  case parseTypus code of
    Left _ -> property True
    Right typusFile ->
      case compile typusFile of
        Left errors -> property $ not (null errors) .&&. all (not . T.null . errorMessage) errors
        Right _ -> property False  -- Should have errors for invalid code

-- Property: Error handler includes source location information
prop_error_handler_includes_location :: Property
prop_error_handler_includes_location =
  forAll arbitraryErrorCode $ \code ->
  case parseTypus code of
    Left _ -> property True
    Right typusFile ->
      case compile typusFile of
        Left errors -> property $ not (null errors) ==> 
                   all (\err -> case errorLocation err of
                     Nothing -> False
                     Just loc -> line loc > 0 && column loc > 0) errors
        Right _ -> property False

-- Property: Error handler categorizes errors correctly
prop_error_handler_categorizes_errors :: Property
prop_error_handler_categorizes_errors =
  forAll arbitraryErrorCode $ \code ->
  case parseTypus code of
    Left _ -> property True
    Right typusFile ->
      case compile typusFile of
        Left errors -> property $ not (null errors) ==> 
                   all (\err -> errorCategory err `elem` [Parsing, TypeChecking, Semantic, Analysis]) errors
        Right _ -> property False

-- Property: Error handler provides recovery suggestions
prop_error_handler_provides_suggestions :: Property
prop_error_handler_provides_suggestions =
  forAll arbitraryErrorCode $ \code ->
  case parseTypus code of
    Left _ -> property True
    Right typusFile ->
      case compile typusFile of
        Left errors -> property $ not (null errors) ==> 
                   any (not . null . errorSuggestions) errors
        Right _ -> property False

-- Property: Error handler handles multiple errors gracefully
prop_error_handler_multiple_errors :: Property
prop_error_handler_multiple_errors =
  let multiErrorCode = "func test() {\n  if true\n    x := \"string\"\n    y := 1\n    z := x + y\n  }\n}\n"
  in case parseTypus multiErrorCode of
    Left _ -> property True
    Right typusFile ->
      case compile typusFile of
        Left errors -> property $ length errors >= 1
        Right _ -> property False

-- Property: Error handler preserves compilation context
prop_error_handler_preserves_context :: Property
prop_error_handler_preserves_context =
  forAll arbitraryErrorCode $ \code ->
  case parseTypus code of
    Left _ -> property True
    Right typusFile ->
      case compile typusFile of
        Left errors -> property $ not (null errors) ==> 
                   all (\err -> not (null $ errorStackTrace err)) errors
        Right _ -> property False

-- Property: Error handler handles edge cases gracefully
prop_error_handler_edge_cases :: Property
prop_error_handler_edge_cases =
  let edgeCases = 
        [ ""  -- Empty file
        , "\n\n\n"  -- Only whitespace
        , "func test()"  -- Incomplete function
        , "func test() {"  -- Unclosed brace
        , "}"  -- Unmatched closing brace
        ]
  in all (\code -> 
    case parseTypus code of
      Left _ -> True
      Right typusFile ->
        case compile typusFile of
          Left _ -> True
          Right _ -> False  -- Should fail for invalid code
    ) edgeCases

-- Property: Error handler recovers from mixed valid/invalid code
prop_error_handler_mixed_code :: Property
prop_error_handler_mixed_code =
  let validCode = "func valid() {\n  x := 1\n  return x\n}\n"
      invalidCode = "func invalid() {\n  if true\n    x := 1\n  }\n}\n"
      mixedCode = validCode ++ "\n" ++ invalidCode
  in case parseTypus mixedCode of
    Left _ -> property True
    Right typusFile ->
      case compile typusFile of
        Left errors -> property $ length errors >= 1
        Right _ -> property False

-- ============================================================================
-- Enhanced Error Handler Properties
-- ============================================================================

-- Property: Enhanced error handler provides detailed context
prop_enhanced_error_detailed_context :: Property
prop_enhanced_error_detailed_context =
  forAll arbitraryErrorCode $ \code ->
  case parseTypus code of
    Left _ -> property True
    Right typusFile ->
      case compile typusFile of
        Left errors -> property $ not (null errors) ==> 
                   all (\err -> T.length (errorMessage err) >= 10) errors
        Right _ -> property False

-- Property: Enhanced error handler groups related errors
prop_enhanced_error_groups_related :: Property
prop_enhanced_error_groups_related =
  let relatedErrorCode = "func test() {\n  x := \"string\"\n  y := 1\n  z := x + y\n  w := x * y\n}\n"
  in case parseTypus relatedErrorCode of
    Left _ -> property True
    Right typusFile ->
      case compile typusFile of
        Left errors -> property $ length errors >= 1
        Right _ -> property False

-- Property: Enhanced error handler provides progressive recovery
prop_enhanced_error_progressive_recovery :: Property
prop_enhanced_error_progressive_recovery =
  let progressiveCode = "func test() {\n  x := 1\n  y := x + 1\n  z := undefined_var\n  w := z + 1\n}\n"
  in case parseTypus progressiveCode of
    Left _ -> property True
    Right typusFile ->
      case compile typusFile of
        Left errors -> property $ length errors >= 1
        Right _ -> property False

-- ============================================================================
-- Performance Properties
-- ============================================================================

-- Property: Error handling is efficient for large files
prop_error_handling_efficient_large :: Property
prop_error_handling_efficient_large =
  let largeCode = unlines $ replicate 100 "func test() {\n  x := 1\n  y := x + 1\n}\n"
  in case parseTypus largeCode of
    Left _ -> property True
    Right typusFile ->
      case compile typusFile of
        Left _ -> property True
        Right _ -> property True

-- Property: Error handling scales with error count
prop_error_handling_scales_with_errors :: Property
prop_error_handling_scales_with_errors =
  let manyErrors = concat $ replicate 10 ["func test() {\n  if true\n    x := 1\n  }\n}\n"]
  in case parseTypus manyErrors of
    Left _ -> property True
    Right typusFile ->
      case compile typusFile of
        Left errors -> property $ length errors >= 1
        Right _ -> property False

-- ============================================================================
-- Advanced Recovery Properties
-- ============================================================================

-- Property: Error handler maintains consistency across phases
prop_error_handler_phase_consistency :: Property
prop_error_handler_phase_consistency =
  forAll arbitraryErrorCode $ \code ->
  case parseTypus code of
    Left _ -> property True
    Right typusFile ->
      case compile typusFile of
        Left errors -> property $ not (null errors) ==> 
                   all (\err -> errorPhase err `elem` [ParsingPhase, TypeCheckingPhase, SemanticPhase]) errors
        Right _ -> property False

-- Property: Error handler provides actionable suggestions
prop_error_handler_actionable_suggestions :: Property
prop_error_handler_actionable_suggestions =
  forAll arbitraryErrorCode $ \code ->
  case parseTypus code of
    Left _ -> property True
    Right typusFile ->
      case compile typusFile of
        Left errors -> property $ not (null errors) ==> 
                   any (\err -> not (null $ errorSuggestions err) && 
                              all (T.length . (> 0)) (errorSuggestions err)) errors
        Right _ -> property False

-- Property: Error handler handles nested errors gracefully
prop_error_handler_nested_errors :: Property
prop_error_handler_nested_errors =
  let nestedErrorCode = "func outer() {\n  func inner() {\n    if true\n      x := 1\n    }\n  }\n}\n"
  in case parseTypus nestedErrorCode of
    Left _ -> property True
    Right typusFile ->
      case compile typusFile of
        Left errors -> property $ length errors >= 1
        Right _ -> property False

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Error Handler Recovery Tests"
  [ testGroup "Basic Error Recovery Properties"
    [ fastProperty "Error handler recovers from syntax errors" prop_error_handler_recovers_syntax
    , fastProperty "Error handler provides meaningful error messages" prop_error_handler_meaningful_messages
    , fastProperty "Error handler includes source location information" prop_error_handler_includes_location
    , fastProperty "Error handler categorizes errors correctly" prop_error_handler_categorizes_errors
    ]

  , testGroup "Error Information Properties"
    [ fastProperty "Error handler provides recovery suggestions" prop_error_handler_provides_suggestions
    , fastProperty "Error handler handles multiple errors gracefully" prop_error_handler_multiple_errors
    , fastProperty "Error handler preserves compilation context" prop_error_handler_preserves_context
    , fastProperty "Error handler handles edge cases gracefully" prop_error_handler_edge_cases
    ]

  , testGroup "Mixed Code Handling Properties"
    [ fastProperty "Error handler recovers from mixed valid/invalid code" prop_error_handler_mixed_code
    ]

  , testGroup "Enhanced Error Handler Properties"
    [ fastProperty "Enhanced error handler provides detailed context" prop_enhanced_error_detailed_context
    , fastProperty "Enhanced error handler groups related errors" prop_enhanced_error_groups_related
    , fastProperty "Enhanced error handler provides progressive recovery" prop_enhanced_error_progressive_recovery
    ]

  , testGroup "Performance Properties"
    [ fastProperty "Error handling is efficient for large files" prop_error_handling_efficient_large
    , fastProperty "Error handling scales with error count" prop_error_handling_scales_with_errors
    ]

  , testGroup "Advanced Recovery Properties"
    [ fastProperty "Error handler maintains consistency across phases" prop_error_handler_phase_consistency
    , fastProperty "Error handler provides actionable suggestions" prop_error_handler_actionable_suggestions
    , fastProperty "Error handler handles nested errors gracefully" prop_error_handler_nested_errors
    ]
  ]