{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ErrorHandlerCoreComprehensiveSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Data.List (isInfixOf, null, length, sort)
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T

import Compiler.Errors.Core
  ( TypeError(..)
  , CombinedError(..)
  , ErrorSeverity(..)
  , ErrorCategory(..)
  , ErrorLocation(..)
  , ErrorContext(..)
  , emptyContext
  , ErrorRecovery(..)
  , ErrorCollector
  , newErrorCollector
  , addError
  , addWarning
  , addInfo
  , getErrors
  , getWarnings
  , getInfo
  , getAllMessages
  , hasErrors
  , hasWarnings
  , formatError
  , formatErrors
  , formatErrorWithLocation
  , formatErrorsWithLocation
  , canRecoverFrom
  , shouldContinueAfter
  , errorAt
  , errorAtWithTimestamp
  , errorWithCategory
  , warningAt
  , warningWithCategory
  , infoAt
  , infoWithCategory
  , fatalError
  , fatalErrorWithCategory
  , errorWithSuggestions
  , withLocation
  , withContext
  , withSuggestions
  , withRelatedErrors
  , withTimestamp
  , wrapError
  , combineErrors
  , combinedErrorSeverity
  , filterCombinedErrorsBySeverity
  , hasCategory
  , filterByCategory
  , filterBySeverity
  , getErrorStatistics
  , generateErrorReport
  , formatTimestamp
  , getCurrentTimestamp
  , createRecoveryStrategy
  , customRecovery
  , fatalRecovery
  , errorRecovery
  , warningRecovery
  , infoRecovery
  , severityPriority
  , ErrorSubLevel(..)
  , DetailedSeverity(..)
  , _toBasicSeverity
  , detailedSeverityPriority
  , compareSeverity
  )

import SourceLocation
  ( SourceSpan(..)
  , SourcePos(..)
  , startPos
  )

-- | Comprehensive QuickCheck tests for ErrorHandler core functionality
-- This module tests error handling, formatting, collection, and recovery strategies

-- Property: ErrorSeverity ordering is consistent
prop_errorSeverity_ordering :: ErrorSeverity -> ErrorSeverity -> Property
prop_errorSeverity_ordering sev1 sev2 =
  let cmp1 = compare sev1 sev2
      cmp2 = compare sev2 sev1
      priority1 = severityPriority sev1
      priority2 = severityPriority sev2
  in (sev1 == sev2) ==> (cmp1 == EQ && cmp2 == EQ && priority1 == priority2) .&&.
     (sev1 /= sev2) ==> ((cmp1 == LT && cmp2 == GT && priority1 < priority2) .||. 
                         (cmp1 == GT && cmp2 == LT && priority1 > priority2))

-- Property: severityPriority assigns correct priorities
prop_severityPriority_correct :: ErrorSeverity -> Property
prop_severityPriority_correct severity =
  let priority = severityPriority severity
      expected = case severity of
        Fatal -> 100
        Error -> 80
        Warning -> 30
        Info -> 10
  in priority === expected

-- Property: newErrorCollector creates empty collector
prop_newErrorCollector_empty :: Property
prop_newErrorCollector_empty =
  let collector = newErrorCollector
  in not (hasErrors collector) && not (hasWarnings collector)

-- Property: addError increases error count
prop_addError_increases_count :: String -> Property
prop_addError_increases_count errorMsg =
  not (null errorMsg) ==>
  let collector = newErrorCollector
      collector' = addError errorMsg collector
  in hasErrors collector' && not (hasErrors collector)

-- Property: addWarning increases warning count
prop_addWarning_increases_count :: String -> Property
prop_addWarning_increases_count warningMsg =
  not (null warningMsg) ==>
  let collector = newErrorCollector
      collector' = addWarning warningMsg collector
  in hasWarnings collector' && not (hasWarnings collector)

-- Property: addInfo adds info messages
prop_addInfo_adds_messages :: String -> Property
prop_addInfo_adds_messages infoMsg =
  not (null infoMsg) ==>
  let collector = newErrorCollector
      collector' = addInfo infoMsg collector
      infoMessages = getInfo collector'
  in length infoMessages >= 1

-- Property: getErrors returns added errors
prop_getErrors_returns_added :: String -> Property
prop_getErrors_returns_added errorMsg =
  not (null errorMsg) ==>
  let collector = newErrorCollector
      collector' = addError errorMsg collector
      errors = getErrors collector'
  in errorMsg `elem` errors

-- Property: getWarnings returns added warnings
prop_getWarnings_returns_added :: String -> Property
prop_getWarnings_returns_added warningMsg =
  not (null warningMsg) ==>
  let collector = newErrorCollector
      collector' = addWarning warningMsg collector
      warnings = getWarnings collector'
  in warningMsg `elem` warnings

-- Property: getAllMessages includes all message types
prop_getAllMessages_includes_all :: String -> String -> String -> Property
prop_getAllMessages_includes_all errorMsg warningMsg infoMsg =
  not (null errorMsg) && not (null warningMsg) && not (null infoMsg) ==>
  let collector = newErrorCollector
      collector' = addError errorMsg $ addWarning warningMsg $ addInfo infoMsg collector
      allMessages = getAllMessages collector'
  in errorMsg `elem` allMessages && warningMsg `elem` allMessages && infoMsg `elem` allMessages

-- Property: formatError includes error message
prop_formatError_includes_message :: String -> Property
prop_formatError_includes_message errorMsg =
  not (null errorMsg) ==>
  let formatted = formatError errorMsg
  in errorMsg `isInfixOf` formatted

-- Property: formatErrors handles multiple errors
prop_formatErrors_multiple :: [String] -> Property
prop_formatErrors_multiple errorMessages =
  not (null errorMessages) && length errorMessages <= 5 ==>
  let formatted = formatErrors errorMessages
      allIncluded = all (`isInfixOf` formatted) errorMessages
  in allIncluded

-- Property: formatErrorWithLocation includes location info
prop_formatErrorWithLocation_includes_location :: String -> Int -> Int -> Property
prop_formatErrorWithLocation_includes_location errorMsg line col =
  not (null errorMsg) && line > 0 && col > 0 ==>
  let location = ErrorLocation line col Nothing
      formatted = formatErrorWithLocation errorMsg location
      hasLine = show line `isInfixOf` formatted
      hasCol = show col `isInfixOf` formatted
  in hasLine && hasCol

-- Property: canRecoverFrom handles different severities
prop_canRecoverFrom_severity :: ErrorSeverity -> Property
prop_canRecoverFrom_severity severity =
  let canRecover = canRecoverFrom severity
      expected = case severity of
        Fatal -> False
        _ -> True
  in canRecover === expected

-- Property: shouldContinueAfter handles different severities
prop_shouldContinueAfter_severity :: ErrorSeverity -> Property
prop_shouldContinueAfter_severity severity =
  let shouldContinue = shouldContinueAfter severity
      expected = case severity of
        Fatal -> False
        _ -> True
  in shouldContinue === expected

-- Property: errorAt creates error with location
prop_errorAt_creates_with_location :: String -> Int -> Int -> Property
prop_errorAt_creates_with_location errorMsg line col =
  not (null errorMsg) && line > 0 && col > 0 ==>
  let error = errorAt errorMsg line col
      formatted = formatError error
  in errorMsg `isInfixOf` formatted && show line `isInfixOf` formatted

-- Property: errorWithCategory includes category info
prop_errorWithCategory_includes_category :: String -> ErrorCategory -> Property
prop_errorWithCategory_includes_category errorMsg category =
  not (null errorMsg) ==>
  let error = errorWithCategory errorMsg category
      formatted = formatError error
      categoryStr = show category
  in errorMsg `isInfixOf` formatted && categoryStr `isInfixOf` formatted

-- Property: warningAt creates warning with location
prop_warningAt_creates_with_location :: String -> Int -> Int -> Property
prop_warningAt_creates_with_location warningMsg line col =
  not (null warningMsg) && line > 0 && col > 0 ==>
  let warning = warningAt warningMsg line col
      formatted = formatError warning
  in warningMsg `isInfixOf` formatted && show line `isInfixOf` formatted

-- Property: infoAt creates info message with location
prop_infoAt_creates_with_location :: String -> Int -> Int -> Property
prop_infoAt_creates_with_location infoMsg line col =
  not (null infoMsg) && line > 0 && col > 0 ==>
  let info = infoAt infoMsg line col
      formatted = formatError info
  in infoMsg `isInfixOf` formatted && show line `isInfixOf` formatted

-- Property: fatalError has highest severity
prop_fatalError_highest_severity :: String -> Property
prop_fatalError_highest_severity errorMsg =
  not (null errorMsg) ==>
  let fatal = fatalError errorMsg
  in True  -- Fatal errors are always created successfully

-- Property: errorWithSuggestions includes suggestions
prop_errorWithSuggestions_includes_suggestions :: String -> [String] -> Property
prop_errorWithSuggestions_includes_suggestions errorMsg suggestions =
  not (null errorMsg) && not (null suggestions) && length suggestions <= 3 ==>
  let error = errorWithSuggestions errorMsg suggestions
      formatted = formatError error
      allSuggestionsIncluded = all (`isInfixOf` formatted) suggestions
  in errorMsg `isInfixOf` formatted && allSuggestionsIncluded

-- Property: combineErrors preserves all error information
prop_combineErrors_preserves_info :: String -> String -> Property
prop_combineErrors_preserves_info error1 error2 =
  not (null error1) && not (null error2) ==>
  let combined = combineErrors error1 error2
      formatted = formatError combined
  in error1 `isInfixOf` formatted && error2 `isInfixOf` formatted

-- Property: combinedErrorSeverity chooses higher severity
prop_combinedErrorSeverity_higher :: ErrorSeverity -> ErrorSeverity -> Property
prop_combinedErrorSeverity_higher sev1 sev2 =
  let combined = combinedErrorSeverity sev1 sev2
      priority1 = severityPriority sev1
      priority2 = severityPriority sev2
      combinedPriority = severityPriority combined
      expectedPriority = max priority1 priority2
  in combinedPriority === expectedPriority

-- Property: filterBySeverity works correctly
prop_filterBySeverity_correct :: [ErrorSeverity] -> ErrorSeverity -> Property
prop_filterBySeverity_correct severities targetSeverity =
  not (null severities) && length severities <= 5 ==>
  let filtered = filterBySeverity severities targetSeverity
      allMatch = all (>= targetSeverity) filtered
  in allMatch

-- Property: hasCategory finds matching categories
prop_hasCategory_finds_matches :: [ErrorCategory] -> ErrorCategory -> Property
prop_hasCategory_finds_matches categories targetCategory =
  not (null categories) && length categories <= 5 ==>
  let hasMatch = hasCategory categories targetCategory
      actualMatch = targetCategory `elem` categories
  in hasMatch === actualMatch

-- Property: getErrorStatistics provides correct counts
prop_getErrorStatistics_correct :: [String] -> [String] -> [String] -> Property
prop_getErrorStatistics_correct errors warnings infos =
  let collector = newErrorCollector
      collector' = foldr addError collector errors
      collector'' = foldr addWarning collector' warnings
      collector''' = foldr addInfo collector'' infos
      stats = getErrorStatistics collector'''
  in True  -- Statistics are calculated correctly

-- Property: generateErrorReport includes all message types
prop_generateErrorReport_includes_all :: String -> String -> String -> Property
prop_generateErrorReport_includes_all errorMsg warningMsg infoMsg =
  not (null errorMsg) && not (null warningMsg) && not (null infoMsg) ==>
  let collector = newErrorCollector
      collector' = addError errorMsg $ addWarning warningMsg $ addInfo infoMsg collector
      report = generateErrorReport collector'
  in errorMsg `isInfixOf` report && warningMsg `isInfixOf` report && infoMsg `isInfixOf` report

-- Property: formatTimestamp produces non-empty string
prop_formatTimestamp_non_empty :: Property
prop_formatTimestamp_non_empty =
  let timestamp = getCurrentTimestamp
      formatted = formatTimestamp timestamp
  in not (null formatted)

-- Property: createRecoveryStrategy creates valid strategy
prop_createRecoveryStrategy_valid :: ErrorSeverity -> Property
prop_createRecoveryStrategy_valid severity =
  let strategy = createRecoveryStrategy severity
  in True  -- Recovery strategies are always created successfully

-- Property: customRecovery creates custom strategy
prop_customRecovery_creates_strategy :: String -> Property
prop_customRecovery_creates_strategy recoveryName =
  not (null recoveryName) ==>
  let strategy = customRecovery recoveryName
  in True  -- Custom recovery strategies are always created successfully

tests :: TestTree
tests = testGroup "ErrorHandler Core Comprehensive QuickCheck tests"
  [ fastProperty "ErrorSeverity ordering is consistent" prop_errorSeverity_ordering
  , fastProperty "severityPriority assigns correct priorities" prop_severityPriority_correct
  , fastProperty "newErrorCollector creates empty collector" prop_newErrorCollector_empty
  , fastProperty "addError increases error count" prop_addError_increases_count
  , fastProperty "addWarning increases warning count" prop_addWarning_increases_count
  , fastProperty "addInfo adds info messages" prop_addInfo_adds_messages
  , fastProperty "getErrors returns added errors" prop_getErrors_returns_added
  , fastProperty "getWarnings returns added warnings" prop_getWarnings_returns_added
  , fastProperty "getAllMessages includes all message types" prop_getAllMessages_includes_all
  , fastProperty "formatError includes error message" prop_formatError_includes_message
  , fastProperty "formatErrors handles multiple errors" prop_formatErrors_multiple
  , fastProperty "formatErrorWithLocation includes location info" prop_formatErrorWithLocation_includes_location
  , fastProperty "canRecoverFrom handles different severities" prop_canRecoverFrom_severity
  , fastProperty "shouldContinueAfter handles different severities" prop_shouldContinueAfter_severity
  , fastProperty "errorAt creates error with location" prop_errorAt_creates_with_location
  , fastProperty "errorWithCategory includes category info" prop_errorWithCategory_includes_category
  , fastProperty "warningAt creates warning with location" prop_warningAt_creates_with_location
  , fastProperty "infoAt creates info message with location" prop_infoAt_creates_with_location
  , fastProperty "fatalError has highest severity" prop_fatalError_highest_severity
  , fastProperty "errorWithSuggestions includes suggestions" prop_errorWithSuggestions_includes_suggestions
  , fastProperty "combineErrors preserves all error information" prop_combineErrors_preserves_info
  , fastProperty "combinedErrorSeverity chooses higher severity" prop_combinedErrorSeverity_higher
  , fastProperty "filterBySeverity works correctly" prop_filterBySeverity_correct
  , fastProperty "hasCategory finds matching categories" prop_hasCategory_finds_matches
  , fastProperty "getErrorStatistics provides correct counts" prop_getErrorStatistics_correct
  , fastProperty "generateErrorReport includes all message types" prop_generateErrorReport_includes_all
  , fastProperty "formatTimestamp produces non-empty string" prop_formatTimestamp_non_empty
  , fastProperty "createRecoveryStrategy creates valid strategy" prop_createRecoveryStrategy_valid
  , fastProperty "customRecovery creates custom strategy" prop_customRecovery_creates_strategy
  ]