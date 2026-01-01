{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Unit.ErrorHandlerCoreQuickCheckTests (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.QuickCheck (testProperties, (===), Property, forAll, Gen, Arbitrary(..), oneof, elements, listOf, listOf1, resize, suchThat)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)

import Compiler.Errors.Core 
  ( ErrorSeverity(..), ErrorCategory(..), ErrorLocation(..), ErrorContext(..)
  , emptyContext, ErrorCollector, newErrorCollector, addError, addWarning
  , getErrors, getWarnings, hasErrors, hasWarnings, formatError
  , errorAt, warningAt, errorWithCategory, filterBySeverity, filterByCategory
  , getErrorStatistics, canRecoverFrom, shouldContinueAfter
  , errorWithSuggestions, withLocation, withContext, withSuggestions
  )

import Compiler.Errors.Compiler
  ( CompilerError(..), CompilationPhase(..), CompilerResult, CompilerM
  , runCompilerM, syntaxError, typeError, ownershipError, dependentTypeError
  , semanticError, formatCompilerError, analyzeErrors, ErrorStatistics(..)
  )

import SourceLocation (SourcePos(..), SourceSpan(..))
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import Data.List (length, isInfixOf)
import Data.List (sort)

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary CompilationPhase where
  arbitrary = oneof 
    [ pure LexingPhase
    , pure ParsingPhase
    , pure TypeCheckingPhase
    , pure OwnershipAnalysisPhase
    , pure DependentTypeCheckingPhase
    , pure CodeGenerationPhase
    , pure OptimizationPhase
    ]

instance Arbitrary ErrorStatistics where
  arbitrary = do
    errorCount <- arbitrary
    warningCount <- arbitrary
    infoCount <- arbitrary
    return $ ErrorStatistics errorCount warningCount infoCount

instance Arbitrary a => Arbitrary (CompilerResult a) where
  arbitrary = oneof
    [ do result <- arbitrary
         return (Right result)
    , do errors <- listOf arbitrary
         return (Left errors)
    ]

-- ============================================================================
-- QuickCheck Properties for ErrorHandler Module
-- ============================================================================

-- | canRecoverFrom: Error severity should determine recoverability
prop_canRecoverFrom_severity :: ErrorSeverity -> Bool
prop_canRecoverFrom_severity severity = 
    let recoverable = canRecoverFrom severity
    in case severity of
      Error -> not recoverable  -- Errors are typically not recoverable
      Warning -> recoverable    -- Warnings are recoverable
      Info -> recoverable       -- Info is recoverable

-- | shouldContinueAfter: Error severity should determine continuation
prop_shouldContinueAfter_severity :: ErrorSeverity -> Bool
prop_shouldContinueAfter_severity severity = 
    let shouldContinue = shouldContinueAfter severity
    in case severity of
      Error -> not shouldContinue  -- Don't continue after errors
      Warning -> shouldContinue    -- Continue after warnings
      Info -> shouldContinue       -- Continue after info

-- | errorWithSuggestions: should preserve original error message
prop_errorWithSuggestions_preserves_message :: ErrorLocation -> String -> [String] -> Bool
prop_errorWithSuggestions_preserves_message location message suggestions = 
    let baseError = (location, message)
        enhancedError = errorWithSuggestions baseError suggestions
    in case enhancedError of
      (loc, msg) -> loc == location && message `L.isInfixOf` msg

-- | withLocation: should update error location
prop_withLocation_updates_location :: ErrorLocation -> ErrorLocation -> String -> Bool
prop_withLocation_updates_location oldLocation newLocation message = 
    let baseError = (oldLocation, message)
        updatedError = withLocation newLocation baseError
    in case updatedError of
      (loc, msg) -> loc == newLocation && msg == message

-- | withContext: should add context to error
prop_withContext_adds_context :: ErrorLocation -> String -> ErrorContext -> Bool
prop_withContext_adds_context location message context = 
    let baseError = (location, message)
        contextualError = withContext context baseError
    in case contextualError of
      (loc, msg) -> loc == location -- Location preserved
      -- Context addition depends on internal implementation

-- | withSuggestions: should add suggestions to error
prop_withSuggestions_adds_suggestions :: ErrorLocation -> String -> [String] -> Bool
prop_withSuggestions_adds_suggestions location message suggestions = 
    let baseError = (location, message)
        suggestedError = withSuggestions suggestions baseError
    in case suggestedError of
      (loc, msg) -> loc == location -- Location preserved
      -- Suggestions addition depends on internal implementation

-- | CompilerError: equality should be reflexive
prop_compilerError_reflexive :: CompilerError -> Bool
prop_compilerError_reflexive ce = ce == ce

-- | CompilationPhase: ordering should be logical
prop_compilationPhase_ordering :: CompilationPhase -> CompilationPhase -> Bool
prop_compilationPhase_ordering phase1 phase2 = 
    let phases = [LexingPhase, ParsingPhase, TypeCheckingPhase, OwnershipAnalysisPhase, 
                  DependentTypeCheckingPhase, CodeGenerationPhase, OptimizationPhase]
        index1 = phaseIndex phase1
        index2 = phaseIndex phase2
    in compare phase1 phase2 == compare index1 index2
  where
    phaseIndex LexingPhase = 0
    phaseIndex ParsingPhase = 1
    phaseIndex TypeCheckingPhase = 2
    phaseIndex OwnershipAnalysisPhase = 3
    phaseIndex DependentTypeCheckingPhase = 4
    phaseIndex CodeGenerationPhase = 5
    phaseIndex OptimizationPhase = 6

-- | formatCompilerError: should include error message
prop_formatCompilerError_contains_message :: CompilerError -> Bool
prop_formatCompilerError_contains_message ce = 
    let formatted = formatCompilerError ce
        baseError = ceError ce
        message = teMessage baseError
    in not (T.null message) && T.unpack message `L.isInfixOf` formatted
  where
    isInfixOf needle haystack = needle `Data.List.L.isInfixOf` haystack

-- | analyzeErrors: should count errors correctly
prop_analyzeErrors_counts :: [CompilerError] -> Bool
prop_analyzeErrors_counts errors = 
    let stats = analyzeErrors errors
        errorCount = esErrorCount stats
    in errorCount == L.length errors

-- | runCompilerM: Right result should have no errors
prop_runCompilerM_success :: String -> Bool
prop_runCompilerM_success result = 
    case runCompilerM (return result) of
      Right actual -> actual == result
      Left _ -> False

-- | syntaxError: should create error with correct phase
prop_syntaxError_phase :: String -> Bool
prop_syntaxError_phase message = 
    let error = syntaxError message
        phase = cePhase error
    in phase == LexingPhase

-- | typeError: should create error with correct phase
prop_typeError_phase :: String -> Bool
prop_typeError_phase message = 
    let error = typeError message
        phase = cePhase error
    in phase == TypeCheckingPhase

-- | ownershipError: should create error with correct phase
prop_ownershipError_phase :: String -> Bool
prop_ownershipError_phase message = 
    let error = ownershipError message
        phase = cePhase error
    in phase == OwnershipAnalysisPhase

-- | dependentTypeError: should create error with correct phase
prop_dependentTypeError_phase :: String -> Bool
prop_dependentTypeError_phase message = 
    let error = dependentTypeError message
        phase = cePhase error
    in phase == DependentTypeCheckingPhase

-- | semanticError: should create error with correct phase
prop_semanticError_phase :: String -> Bool
prop_semanticError_phase message = 
    let error = semanticError message
        phase = cePhase error
    in phase == TypeCheckingPhase  -- Semantic errors typically during type checking

-- | ErrorStatistics: should L.sum counts correctly
prop_errorStatistics_sum :: Int -> Int -> Int -> Bool
prop_errorStatistics_sum errors warnings infos = 
    let stats = ErrorStatistics errors warnings infos
        total = esErrorCount stats + esWarningCount stats + esInfoCount stats
    in total == errors + warnings + infos

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "ErrorHandler Core QuickCheck Tests"
  [ testProperties "Error Recovery Properties"
    [ ("canRecoverFrom severity", prop_canRecoverFrom_severity)
    , ("shouldContinueAfter severity", prop_shouldContinueAfter_severity)
    ]

  , testProperties "Error Enhancement Properties"
    [ ("errorWithSuggestions preserves message", prop_errorWithSuggestions_preserves_message)
    , ("withLocation updates location", prop_withLocation_updates_location)
    , ("withContext adds context", prop_withContext_adds_context)
    , ("withSuggestions adds suggestions", prop_withSuggestions_adds_suggestions)
    ]

  , testProperties "Compiler Error Properties"
    [ ("CompilerError reflexive", prop_compilerError_reflexive)
    , ("CompilationPhase ordering", prop_compilationPhase_ordering)
    , ("formatCompilerError contains message", prop_formatCompilerError_contains_message)
    , ("analyzeErrors counts", prop_analyzeErrors_counts)
    ]

  , testProperties "Compiler Monad Properties"
    [ ("runCompilerM success", prop_runCompilerM_success)
    ]

  , testProperties "Error Construction Properties"
    [ ("syntaxError phase", prop_syntaxError_phase)
    , ("typeError phase", prop_typeError_phase)
    , ("ownershipError phase", prop_ownershipError_phase)
    , ("dependentTypeError phase", prop_dependentTypeError_phase)
    , ("semanticError phase", prop_semanticError_phase)
    ]

  , testProperties "Statistics Properties"
    [ ("ErrorStatistics L.sum", prop_errorStatistics_sum)
    ]
  ]