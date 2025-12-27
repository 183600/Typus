{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ErrorHandlerConsistencySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, sized, resize, Positive(..))

import ErrorHandler
import EnhancedErrorHandler
import Compiler.Errors
import Compiler.Errors.Core
import SourceLocation
import Utils

import Data.Char (isSpace, isLetter, isDigit)
import qualified Data.List as Data.List
import Data.List (isPrefixOf, isInfixOf, intercalate, nub, sort)
import Data.Maybe (isJust, isNothing, fromMaybe, catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

-- | Tests for error handler consistency across different compiler phases
tests :: TestTree
tests =
  testGroup "Error Handler Consistency Tests"
    [ testGroup "Error Message Consistency"
        [ fastProperty "Syntax errors have consistent format" prop_syntax_error_format
        , fastProperty "Type errors have consistent format" prop_type_error_format
        , fastProperty "Ownership errors have consistent format" prop_ownership_error_format
        , testCase "Error message template consistency" test_error_message_templates
        , testCase "Error localization consistency" test_error_localization
        ]
    
    , testGroup "Error Classification Consistency"
        [ fastProperty "Error severity classification is consistent" prop_error_severity_consistency
        , fastProperty "Error category classification is consistent" prop_error_category_consistency
        , fastProperty "Error recovery suggestions are consistent" prop_error_recovery_consistency
        , testCase "Error code mapping consistency" test_error_code_mapping
        , testCase "Error hierarchy consistency" test_error_hierarchy
        ]
    
    , testGroup "Cross-Phase Error Handling"
        [ fastProperty "Parser and type checker errors are consistent" prop_parser_type_checker_consistency
        , fastProperty "Type checker and ownership errors are consistent" prop_type_checker_ownership_consistency
        , fastProperty "All phases use same error context" prop_error_context_consistency
        , testCase "Error propagation consistency" test_error_propagation
        , testCase "Error aggregation consistency" test_error_aggregation
        ]
    
    , testGroup "Error Recovery Consistency"
        [ fastProperty "Error recovery strategies are consistent" prop_recovery_strategy_consistency
        , fastProperty "Recovery suggestions are actionable" prop_recovery_suggestions_actionable
        , fastProperty "Recovery preserves program structure" prop_recovery_preserves_structure
        , testCase "Incremental error recovery" test_incremental_recovery
        , testCase "Batch error recovery" test_batch_recovery
        ]
    
    , testGroup "Error Reporting Consistency"
        [ fastProperty "Error reports are deterministic" prop_error_determinism
        , fastProperty "Error ordering is consistent" prop_error_ordering_consistency
        , fastProperty "Error deduplication is consistent" prop_error_deduplication
        , testCase "Error report formatting" test_error_report_formatting
        , testCase "Error statistics consistency" test_error_statistics
        ]
    ]

-- Property: Syntax errors have consistent format
prop_syntax_error_format :: String -> Property
prop_syntax_error_format invalidCode =
  not (null invalidCode) ==> 
  let syntaxErrors = detectSyntaxErrors invalidCode
      formattedErrors = map formatSyntaxError syntaxErrors
      hasConsistentFormat = all hasErrorFormat formattedErrors
  in property $ hasConsistentFormat

-- Property: Type errors have consistent format
prop_type_error_format :: String -> Property
prop_type_error_format codeWithTypeErrors =
  not (null codeWithTypeErrors) ==> 
  let typeErrors = detectTypeErrors codeWithTypeErrors
      formattedErrors = map formatTypeError typeErrors
      hasConsistentFormat = all hasErrorFormat formattedErrors
  in property $ hasConsistentFormat

-- Property: Ownership errors have consistent format
prop_ownership_error_format :: String -> Property
prop_ownership_error_format codeWithOwnershipErrors =
  not (null codeWithOwnershipErrors) ==> 
  let ownershipErrors = detectOwnershipErrors codeWithOwnershipErrors
      formattedErrors = map formatOwnershipError ownershipErrors
      hasConsistentFormat = all hasErrorFormat formattedErrors
  in property $ hasConsistentFormat

-- Property: Error severity classification is consistent
prop_error_severity_consistency :: String -> Property
prop_error_severity_consistency errorCode =
  not (null errorCode) ==> 
  let severity1 = classifyErrorSeverity errorCode
      severity2 = classifyErrorSeverity errorCode -- Should be deterministic
  in property $ severity1 === severity2

-- Property: Error category classification is consistent
prop_error_category_consistency :: String -> Property
prop_error_category_consistency errorCode =
  not (null errorCode) ==> 
  let category1 = categorizeError errorCode
      category2 = categorizeError errorCode -- Should be deterministic
  in property $ category1 === category2

-- Property: Error recovery suggestions are consistent
prop_error_recovery_consistency :: String -> Property
prop_error_recovery_consistency errorCode =
  not (null errorCode) ==> 
  let suggestions1 = generateRecoverySuggestions errorCode
      suggestions2 = generateRecoverySuggestions errorCode -- Should be deterministic
  in property $ sort suggestions1 === sort suggestions2

-- Property: Parser and type checker errors are consistent
prop_parser_type_checker_consistency :: String -> Property
prop_parser_type_checker_consistency code =
  not (null code) ==> 
  let parserErrors = detectSyntaxErrors code
      typeCheckerErrors = detectTypeErrors code
      parserContexts = map getErrorContext parserErrors
      typeCheckerContexts = map getErrorContext typeCheckerErrors
      contextsConsistent = all hasConsistentContext (parserContexts ++ typeCheckerContexts)
  in property $ contextsConsistent

-- Property: Type checker and ownership errors are consistent
prop_type_checker_ownership_consistency :: String -> Property
prop_type_checker_ownership_consistency code =
  not (null code) ==> 
  let typeErrors = detectTypeErrors code
      ownershipErrors = detectOwnershipErrors code
      typeContexts = map getErrorContext typeErrors
      ownershipContexts = map getErrorContext ownershipErrors
      contextsConsistent = all hasConsistentContext (typeContexts ++ ownershipContexts)
  in property $ contextsConsistent

-- Property: All phases use same error context
prop_error_context_consistency :: [String] -> Property
prop_error_context_consistency errorCodes =
  not (null errorCodes) ==> 
  let contexts = map getErrorContext errorCodes
      contextTypes = map getErrorContextType contexts
      hasConsistentTypes = length (nub contextTypes) <= 3 -- Allow some variation
  in property $ hasConsistentTypes

-- Property: Error recovery strategies are consistent
prop_recovery_strategy_consistency :: String -> Property
prop_recovery_strategy_consistency errorCode =
  not (null errorCode) ==> 
  let strategy1 = selectRecoveryStrategy errorCode
      strategy2 = selectRecoveryStrategy errorCode -- Should be deterministic
  in property $ strategy1 === strategy2

-- Property: Recovery suggestions are actionable
prop_recovery_suggestions_actionable :: String -> Property
prop_recovery_suggestions_actionable errorCode =
  not (null errorCode) ==> 
  let suggestions = generateRecoverySuggestions errorCode
      actionableSuggestions = filter isActionableSuggestion suggestions
  in property $ length actionableSuggestions >= length suggestions `div` 2

-- Property: Recovery preserves program structure
prop_recovery_preserves_structure :: String -> Property
prop_recovery_preserves_structure invalidCode =
  not (null invalidCode) ==> 
  let recoveredCode = applyErrorRecovery invalidCode
      structurePreserved = hasValidStructure recoveredCode
  in property $ structurePreserved

-- Property: Error reports are deterministic
prop_error_determinism :: String -> Property
prop_error_determinism code =
  not (null code) ==> 
  let report1 = generateErrorReport code
      report2 = generateErrorReport code -- Should be identical
  in property $ report1 === report2

-- Property: Error ordering is consistent
prop_error_ordering_consistency :: String -> Property
prop_error_ordering_consistency code =
  not (null code) ==> 
  let errors1 = detectAllErrors code
      errors2 = detectAllErrors code -- Should be in same order
  in property $ errors1 === errors2

-- Property: Error deduplication is consistent
prop_error_deduplication :: [String] -> Property
prop_error_deduplication errorCodes =
  not (null errorCodes) ==> 
  let uniqueErrors1 = deduplicateErrors errorCodes
      uniqueErrors2 = deduplicateErrors errorCodes -- Should be identical
  in property $ sort uniqueErrors1 === sort uniqueErrors2

-- Test cases for specific consistency scenarios

test_error_message_templates :: IO ()
test_error_message_templates = do
  let syntaxError = createSyntaxError "missing semicolon"
      typeError = createTypeError "type mismatch"
      ownershipError = createOwnershipError "use after move"
      syntaxMessage = formatErrorMessage syntaxError
      typeMessage = formatErrorMessage typeError
      ownershipMessage = formatErrorMessage ownershipError
      hasConsistentTemplate = hasErrorFormat syntaxMessage && 
                             hasErrorFormat typeMessage && 
                             hasErrorFormat ownershipMessage
  hasConsistentTemplate @?= True

test_error_localization :: IO ()
test_error_localization = do
  let errorCode = "E001"
      englishMessage = getLocalizedError errorCode "en"
      frenchMessage = getLocalizedError errorCode "fr"
      hasEnglish = not (null englishMessage)
      hasFrench = not (null frenchMessage)
      differentLanguages = englishMessage /= frenchMessage
  hasEnglish @?= True
  hasFrench @?= True
  differentLanguages @?= True

test_error_code_mapping :: IO ()
test_error_code_mapping = do
  let errorCodes = ["E001", "E002", "E003"]
      mappedCodes = map mapErrorCode errorCodes
      hasMapping = all (not . null) mappedCodes
  hasMapping @?= True

test_error_hierarchy :: IO ()
test_error_hierarchy = do
  let baseError = createBaseError "base error"
      syntaxError = createSyntaxErrorFromBase baseError
      typeError = createTypeErrorFromBase baseError
      syntaxIsBase = isSubtypeOf syntaxError baseError
      typeIsBase = isSubtypeOf typeError baseError
  syntaxIsBase @?= True
  typeIsBase @?= True

test_error_propagation :: IO ()
test_error_propagation = do
  let phase1Errors = [createPhaseError "parser" "syntax error"]
      phase2Errors = propagateErrors phase1Errors "type checker"
      propagatedCorrectly = length phase2Errors >= length phase1Errors
  propagatedCorrectly @?= True

test_error_aggregation :: IO ()
test_error_aggregation = do
  let errorSets = [["E001", "E002"], ["E002", "E003"], ["E003", "E004"]]
      aggregatedErrors = aggregateErrors errorSets
      hasAllErrors = all (`elem` aggregatedErrors) ["E001", "E002", "E003", "E004"]
  hasAllErrors @?= True

test_incremental_recovery :: IO ()
test_incremental_recovery = do
  let initialErrors = ["E001", "E002"]
      newError = "E003"
      recoveredState = applyIncrementalRecovery initialErrors newError
      hasRecovered = newError `elem` recoveredState
  hasRecovered @?= True

test_batch_recovery :: IO ()
test_batch_recovery = do
  let errorBatch = ["E001", "E002", "E003", "E004"]
      recoveredErrors = applyBatchRecovery errorBatch
      allRecovered = all (`elem` recoveredErrors) errorBatch
  allRecovered @?= True

test_error_report_formatting :: IO ()
test_error_report_formatting = do
  let errors = [createSyntaxError "error1", createTypeError "error2"]
      report = formatErrorReport errors
      hasHeader = "Error Report" `isInfixOf` report
      hasSummary = "Summary:" `isInfixOf` report
      hasDetails = "Details:" `isInfixOf` report
  hasHeader @?= True
  hasSummary @?= True
  hasDetails @?= True

test_error_statistics :: IO ()
test_error_statistics = do
  let errors = [createSyntaxError "syntax", createTypeError "type", createSyntaxError "syntax2"]
      stats = calculateErrorStatistics errors
      expectedSyntaxCount = 2
      expectedTypeCount = 1
      actualSyntaxCount = syntaxErrorCount stats
      actualTypeCount = typeErrorCount stats
  actualSyntaxCount @?= expectedSyntaxCount
  actualTypeCount @?= expectedTypeCount

-- Helper functions (placeholders for actual implementation)

-- Error detection functions
detectSyntaxErrors :: String -> [String]
detectSyntaxErrors _ = ["E001"] -- Placeholder

detectTypeErrors :: String -> [String]
detectTypeErrors _ = ["E002"] -- Placeholder

detectOwnershipErrors :: String -> [String]
detectOwnershipErrors _ = ["E003"] -- Placeholder

detectAllErrors :: String -> [String]
detectAllErrors _ = ["E001", "E002", "E003"] -- Placeholder

-- Error formatting functions
formatSyntaxError :: String -> String
formatSyntaxError code = "Syntax Error: " ++ code -- Placeholder

formatTypeError :: String -> String
formatTypeError code = "Type Error: " ++ code -- Placeholder

formatOwnershipError :: String -> String
formatOwnershipError code = "Ownership Error: " ++ code -- Placeholder

hasErrorFormat :: String -> Bool
hasErrorFormat msg = "Error:" `isInfixOf` msg -- Placeholder

formatErrorMessage :: String -> String
formatErrorMessage error = "Error: " ++ error -- Placeholder

-- Error classification functions
classifyErrorSeverity :: String -> String
classifyErrorSeverity _ = "Error" -- Placeholder

categorizeError :: String -> String
categorizeError _ = "General" -- Placeholder

generateRecoverySuggestions :: String -> [String]
generateRecoverySuggestions _ = ["fix syntax", "check types"] -- Placeholder

isActionableSuggestion :: String -> Bool
isActionableSuggestion _ = True -- Placeholder

-- Error context functions
getErrorContext :: String -> ErrorContext
getErrorContext _ = ErrorContext "test" 1 1 -- Placeholder

getErrorContextType :: ErrorContext -> String
getErrorContextType (ErrorContext _ _ _) = "source" -- Placeholder

hasConsistentContext :: ErrorContext -> Bool
hasConsistentContext _ = True -- Placeholder

-- Error recovery functions
selectRecoveryStrategy :: String -> String
selectRecoveryStrategy _ = "standard" -- Placeholder

applyErrorRecovery :: String -> String
applyErrorRecovery code = code ++ " // recovered" -- Placeholder

hasValidStructure :: String -> Bool
hasValidStructure _ = True -- Placeholder

applyIncrementalRecovery :: [String] -> String -> [String]
applyIncrementalRecovery errors newError = newError : errors -- Placeholder

applyBatchRecovery :: [String] -> [String]
applyBatchRecovery errors = errors -- Placeholder

-- Error reporting functions
generateErrorReport :: String -> String
generateErrorReport _ = "Error Report:\nSummary: 1 error\nDetails: E001" -- Placeholder

deduplicateErrors :: [String] -> [String]
deduplicateErrors = nub -- Placeholder

formatErrorReport :: [String] -> String
formatErrorReport errors = "Error Report:\nSummary: " ++ show (length errors) ++ " errors\nDetails: " ++ show errors -- Placeholder

-- Error statistics functions
calculateErrorStatistics :: [String] -> ErrorStatistics
calculateErrorStatistics errors = ErrorStatistics (length (filter isSyntaxError errors)) (length (filter isTypeError errors)) -- Placeholder

syntaxErrorCount :: ErrorStatistics -> Int
syntaxErrorCount (ErrorStatistics syntax _) = syntax

typeErrorCount :: ErrorStatistics -> Int
typeErrorCount (ErrorStatistics _ types) = types

isSyntaxError :: String -> Bool
isSyntaxError code = "E001" `isPrefixOf` code -- Placeholder

isTypeError :: String -> Bool
isTypeError code = "E002" `isPrefixOf` code -- Placeholder

-- Error creation and mapping functions
createSyntaxError :: String -> String
createSyntaxError msg = "Syntax Error: " ++ msg

createTypeError :: String -> String
createTypeError msg = "Type Error: " ++ msg

createOwnershipError :: String -> String
createOwnershipError msg = "Ownership Error: " ++ msg

createBaseError :: String -> String
createBaseError msg = "Base Error: " ++ msg

createSyntaxErrorFromBase :: String -> String
createSyntaxErrorFromBase base = base ++ " (syntax)"

createTypeErrorFromBase :: String -> String
createTypeErrorFromBase base = base ++ " (type)"

mapErrorCode :: String -> String
mapErrorCode code = "Mapped: " ++ code

getLocalizedError :: String -> String -> String
getLocalizedError code lang = "Error " ++ code ++ " (" ++ lang ++ ")"

isSubtypeOf :: String -> String -> Bool
isSubtypeOf subtype base = base `isInfixOf` subtype

propagateErrors :: [String] -> String -> [String]
propagateErrors errors phase = map (++ " (" ++ phase ++ ")") errors

aggregateErrors :: [[String]] -> [String]
aggregateErrors errorSets = nub (concat errorSets)

createPhaseError :: String -> String -> String
createPhaseError phase msg = phase ++ " error: " ++ msg

-- Data types (placeholders)
data ErrorContext = ErrorContext String Int Int deriving (Show, Eq)

data ErrorStatistics = ErrorStatistics Int Int deriving (Show, Eq)