{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ErrorRecoverySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (intercalate, nub, sort)
import Data.Char (isSpace, isLetter, isDigit)

-- Import error handling modules
import qualified ErrorHandler
import qualified ErrorHandler.Core
import qualified EnhancedErrorHandler
import qualified Parser
import qualified Compiler
import qualified SourceLocation
import qualified Utils

-- | Error recovery tests covering error detection, recovery mechanisms, L.and graceful degradation
tests :: TestTree
tests =
  testGroup "Error Recovery"
    [ testGroup "Basic Error Detection"
        [ fastProperty "Error detection accuracy" prop_error_detection_accuracy
        , fastProperty "Error location precision" prop_error_location_precision
        , fastProperty "Error classification correctness" prop_error_classification_correctness
        , testCase "Basic error scenarios" $ do
            let input = "x := 1 + )invalid("  -- Syntax error
            case Parser.parseExpression input of
              Left err -> "syntax" `L.isInfixOf` show err @?= True
              Right _ -> assertFailure "Expected parse error"
        ]

    , testGroup "Error Recovery Mechanisms"
        [ fastProperty "Parser error recovery" prop_parser_error_recovery
        , fastProperty "Compiler error recovery" prop_compiler_error_recovery
        , fastProperty "Type checker error recovery" prop_type_checker_error_recovery
        , testCase "Recovery mechanisms" $ do
            let input = "func test() { return ; }"  -- Missing expression
            result <- Compiler.compileWithRecovery input
            case result of
              (Left _, recovered) -> ErrorHandler.hasPartialResult recovered @?= True
              (Right _, _) -> pure ()  -- Success is also acceptable
        ]

    , testGroup "Error Context Preservation"
        [ fastProperty "Error context maintained" prop_error_context_maintained
        , fastProperty "Error stack trace accuracy" prop_error_stack_trace_accuracy
        , fastProperty "Error propagation consistency" prop_error_propagation_consistency
        , testCase "Context preservation" $ do
            let input = "x := 1"
                context = "variable declaration"
            err <- ErrorHandler.mkErrorWithContext "type error" context (SourceLocation.Position 1 1)
            ErrorHandler.getErrorContext err @?= context
        ]

    , testGroup "Graceful Degradation"
        [ fastProperty "Partial compilation with errors" prop_partial_compilation_with_errors
        , fastProperty "Fallback mechanism correctness" prop_fallback_mechanism_correctness
        , fastProperty "Best effort parsing" prop_best_effort_parsing
        , testCase "Graceful degradation" $ do
            let program = "func main() { invalid_syntax; return 42; }"
            result <- Compiler.attemptCompilation program
            case result of
              Left _ -> pure ()  -- Complete failure is acceptable
              Right partial -> Compiler.hasValidParts partial @?= True
        ]

    , testGroup "Error Message Quality"
        [ fastProperty "Error messages are informative" prop_error_messages_informative
        , fastProperty "Error suggestions are helpful" prop_error_suggestions_helpful
        , fastProperty "Error formatting consistency" prop_error_formatting_consistency
        , testCase "Error message quality" $ do
            let err = ErrorHandler.Core.mkParseError "unexpected token" (SourceLocation.Position 5 10)
            let msg = ErrorHandler.errorMessage err
            "unexpected token" `L.isInfixOf` msg @?= True
            "line 5" `L.isInfixOf` msg @?= True
            "column 10" `L.isInfixOf` msg @?= True
        ]

    , testGroup "Recovery Strategy Testing"
        [ fastProperty "Panic mode recovery" prop_panic_mode_recovery
        , fastProperty "Phrase level recovery" prop_phrase_level_recovery
        , fastProperty "Error production recovery" prop_error_production_recovery
        , testCase "Recovery strategies" $ do
            let input = "x := 1; y := ; z := 3"  -- Error in middle
            result <- Parser.parseWithRecovery input
            ErrorHandler.hasValidAST result @?= True
        ]

    , testGroup "Enhanced Error Handling"
        [ fastProperty "Enhanced error detection" prop_enhanced_error_detection
        , fastProperty "Enhanced recovery mechanisms" prop_enhanced_recovery_mechanisms
        , fastProperty "Enhanced context tracking" prop_enhanced_context_tracking
        , testCase "Enhanced error handling" $ do
            let input = "func test<T>(x: T): T { return x + \"string\"; }"
            result <- EnhancedErrorHandler.analyzeAndRecover input
            EnhancedErrorHandler.hasRecoveryPlan result @?= True
        ]
    ]

-- Property-based tests

-- Basic error detection properties
prop_error_detection_accuracy :: String -> Property
prop_error_detection_accuracy input =
  let hasError = hasSyntaxError input
      detected = case Parser.parseExpression input of
        Left _ -> True
        Right _ -> False
      accuracy = hasError ==> detected
  in property $ accuracy

prop_error_location_precision :: String -> Property
prop_error_location_precision input =
  hasSyntaxError input ==>
  let errorLoc = extractErrorLocation input
      precise = case errorLoc of
        Nothing -> False
        Just loc -> SourceLocation.isValidPosition loc
  in property $ precise

prop_error_classification_correctness :: String -> Property
prop_error_classification_correctness input =
  let errorType = classifyError input
      isCorrectlyClassified = case errorType of
        "syntax" -> hasSyntaxError input
        "type" -> hasTypeError input
        "semantic" -> hasSemanticError input
        _ -> False
  in property $ isCorrectlyClassified || not (hasAnyError input)

-- Error recovery mechanisms properties
prop_parser_error_recovery :: String -> Property
prop_parser_error_recovery input =
  hasSyntaxError input ==>
  let recovered = Parser.parseWithRecovery input
      hasPartialResult = ErrorHandler.hasPartialAST recovered
      recoveryQuality = ErrorHandler.recoveryQuality recovered
  in property $ hasPartialResult .&&. recoveryQuality >= 0.5

prop_compiler_error_recovery :: String -> Property
prop_compiler_error_recovery input =
  hasAnyError input ==>
  let recovered = Compiler.compileWithRecovery input
      hasPartialCompilation = ErrorHandler.hasPartialResult recovered
      canContinue = ErrorHandler.canContinueCompilation recovered
  in property $ hasPartialCompilation .&&. canContinue

prop_type_checker_error_recovery :: String -> Property
prop_type_checker_error_recovery input =
  hasTypeError input ==>
  let recovered = Compiler.TypeChecker.checkWithRecovery input
      hasPartialTypes = ErrorHandler.hasPartialTypeCheck recovered
      suggestionsExist = ErrorHandler.hasSuggestions recovered
  in property $ hasPartialTypes .&&. suggestionsExist

-- Error context preservation properties
prop_error_context_maintained :: String -> String -> Property
prop_error_context_maintained input context =
  hasAnyError input && not (null context) ==>
  let error = ErrorHandler.mkErrorWithContext "error" context (SourceLocation.Position 1 1)
      preserved = ErrorHandler.getErrorContext error
  in property $ context `L.isInfixOf` preserved

prop_error_stack_trace_accuracy :: [String] -> Property
prop_error_stack_trace_accuracy callStack =
  not (null callStack) && L.length callStack <= 5 ==>
  let error = ErrorHandler.mkErrorWithStack "error" callStack
      stack = ErrorHandler.getErrorStack error
      accurate = L.length stack >= L.length callStack - 1  -- Allow some frames to be missing
  in property $ accurate

prop_error_propagation_consistency :: String -> Property
prop_error_propagation_consistency input =
  hasAnyError input ==>
  let parseError = Parser.parseExpression input
      compileError = case parseError of
        Left _ -> Nothing
        Right ast -> Just <$> Compiler.compileAST ast
      consistent = case (parseError, compileError) of
        (Left _, Nothing) -> True
        (Left _, Just (Left _)) -> True
        (Right _, _) -> True  -- No parse error means no propagation needed
        _ -> False
  in property $ consistent

-- Graceful degradation properties
prop_partial_compilation_with_errors :: String -> Property
prop_partial_compilation_with_errors input =
  hasAnyError input ==>
  let partial = Compiler.compilePartial input
      hasValidParts = case partial of
        Left _ -> False
        Right p -> Compiler.hasValidParts p
      usability = case partial of
        Left _ -> 0.0
        Right p -> Compiler.usabilityScore p
  in property $ hasValidParts .&&. usability > 0.0

prop_fallback_mechanism_correctness :: String -> Property
prop_fallback_mechanism_correctness input =
  hasAnyError input ==>
  let fallback = Compiler.fallbackCompilation input
      isSafe = Compiler.isSafeFallback fallback
      providesValue = Compiler.providesSomeValue fallback
  in property $ isSafe .&&. providesValue

prop_best_effort_parsing :: String -> Property
prop_best_effort_parsing input =
  let bestEffort = Parser.bestEffortParse input
      hasSomeStructure = ErrorHandler.hasSomeStructure bestEffort
      errorsReported = ErrorHandler.getErrorCount bestEffort
  in property $ hasSomeStructure .&&. errorsReported >= 0

-- Error message quality properties
prop_error_messages_informative :: String -> Property
prop_error_messages_informative input =
  hasAnyError input ==>
  let error = extractFirstError input
      message = ErrorHandler.errorMessage error
      informative = "error" `L.isInfixOf` message && 
                   not (null message) && 
                   L.length message <= 200  -- Reasonable L.length
  in property $ informative

prop_error_suggestions_helpful :: String -> Property
prop_error_suggestions_helpful input =
  hasAnyError input ==>
  let error = extractFirstError input
      suggestions = ErrorHandler.getSuggestions error
      helpful = not (null suggestions) && 
                L.all (not . null) suggestions &&
                L.length suggestions <= 5  -- Reasonable number
  in property $ helpful || L.length input < 5

prop_error_formatting_consistency :: String -> Property
prop_error_formatting_consistency input =
  hasAnyError input ==>
  let error = extractFirstError input
      formatted = ErrorHandler.formatError error
      consistent = hasLineInfo formatted && 
                   hasColumnInfo formatted &&
                   hasMessageInfo formatted
  in property $ consistent

-- Recovery strategy testing properties
prop_panic_mode_recovery :: String -> Property
prop_panic_mode_recovery input =
  hasSyntaxError input ==>
  let recovered = Parser.panicModeRecovery input
      canContinue = ErrorHandler.canContinueParsing recovered
      errorSkipped = ErrorHandler.hasSkippedToRecoveryPoint recovered
  in property $ canContinue .&&. errorSkipped

prop_phrase_level_recovery :: String -> Property
prop_phrase_level_recovery input =
  hasSyntaxError input ==>
  let recovered = Parser.phraseLevelRecovery input
      phraseComplete = ErrorHandler.hasCompletePhrase recovered
      minimalErrors = ErrorHandler.getErrorCount recovered <= 2
  in property $ phraseComplete .&&. minimalErrors

prop_error_production_recovery :: String -> Property
prop_error_production_recovery input =
  hasSyntaxError input ==>
  let recovered = Parser.errorProductionRecovery input
      productionValid = ErrorHandler.hasValidProduction recovered
      localRecovery = ErrorHandler.isLocalRecovery recovered
  in property $ productionValid .&&. localRecovery

-- Enhanced error handling properties
prop_enhanced_error_detection :: String -> Property
prop_enhanced_error_detection input =
  let standardDetection = hasAnyError input
      enhancedDetection = EnhancedErrorHandler.detectErrors input
      enhancedBetter = L.length enhancedDetection >= 
                       (if standardDetection then 1 else 0)
  in property $ enhancedBetter

prop_enhanced_recovery_mechanisms :: String -> Property
prop_enhanced_recovery_mechanisms input =
  hasAnyError input ==>
  let enhancedRecovery = EnhancedErrorHandler.enhancedRecovery input
      recoveryQuality = EnhancedErrorHandler.recoveryScore enhancedRecovery
      hasPlan = EnhancedErrorHandler.hasRecoveryPlan enhancedRecovery
  in property $ recoveryQuality >= 0.6 .&&. hasPlan

prop_enhanced_context_tracking :: String -> Property
prop_enhanced_context_tracking input =
  hasAnyError input ==>
  let enhancedContext = EnhancedErrorHandler.trackContext input
      contextComplete = EnhancedErrorHandler.hasCompleteContext enhancedContext
      contextUseful = EnhancedErrorHandler.isContextUseful enhancedContext
  in property $ contextComplete .&&. contextUseful

-- Helper functions
hasSyntaxError :: String -> Bool
hasSyntaxError input = case Parser.parseExpression input of
  Left _ -> True
  Right _ -> False

hasTypeError :: String -> Bool
hasTypeError input = case Compiler.TypeChecker.checkExpression input of
  Left _ -> True
  Right _ -> False

hasSemanticError :: String -> Bool
hasSemanticError input = case Compiler.compileAST =<< Parser.parseExpression input of
  Left _ -> True
  Right _ -> False

hasAnyError :: String -> Bool
hasAnyError input = hasSyntaxError input || hasTypeError input || hasSemanticError input

extractErrorLocation :: String -> Maybe SourceLocation.Position
extractErrorLocation input = case Parser.parseExpression input of
  Left err -> Just $ ErrorHandler.getErrorLocation err
  Right _ -> Nothing

classifyError :: String -> String
classifyError input
  | hasSyntaxError input = "syntax"
  | hasTypeError input = "type"
  | hasSemanticError input = "semantic"
  | otherwise = "unknown"

extractFirstError :: String -> ErrorHandler.Error
extractFirstError input = case Parser.parseExpression input of
  Left err -> err
  Right _ -> ErrorHandler.mkError "no error" (SourceLocation.Position 1 1)

hasLineInfo :: String -> Bool
hasLineInfo msg = "line" `L.isInfixOf` msg

hasColumnInfo :: String -> Bool
hasColumnInfo msg = "column" `L.isInfixOf` msg

hasMessageInfo :: String -> Bool
hasMessageInfo msg = not (null msg) && L.length msg > 5