{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CoreErrorHandlerQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf, vectorOf, elements, oneof)

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
  , canRecoverFrom
  , shouldContinueAfter
  , infoRecovery
  )

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sort, nub)
import Data.Maybe (isJust, isNothing, fromMaybe)

-- ============================================================================
-- Generators
-- ============================================================================

genErrorSeverity :: Gen ErrorSeverity
genErrorSeverity = elements [ErrorInfo, ErrorWarning, ErrorError, ErrorFatal]

genErrorCategory :: Gen ErrorCategory
genErrorCategory = elements 
  [ SyntaxError
  , TypeError
  , NameError
  , ScopeError
  , OwnershipError
  , DependencyError
  , InternalError
  , Warning
  , Info
  ]

genErrorLocation :: Gen ErrorLocation
genErrorLocation = do
  line <- choose (1, 1000)
  col <- choose (1, 1000)
  file <- elements ["test.typus", "module.typus", ""]
  return $ ErrorLocation line col file

genErrorContext :: Gen ErrorContext
genErrorContext = do
  context <- listOf $ elements ["function", "module", "block", "expression"]
  return $ ErrorContext context

genErrorRecovery :: Gen ErrorRecovery
genErrorRecovery = elements 
  [ RecoveryContinue
  , RecoverySkip
  , RecoveryAbort
  , RecoveryRetry
  , CustomRecovery "custom"
  ]

genTypeError :: Gen TypeError
genTypeError = do
  severity <- genErrorSeverity
  category <- genErrorCategory
  location <- genErrorLocation
  context <- genErrorContext
  message <- elements ["Type mismatch", "Undefined variable", "Syntax error", "Ownership violation"]
  suggestions <- listOf $ elements ["Check types", "Define variable", "Fix syntax", "Review ownership"]
  relatedErrors <- listOf genTypeError
  recovery <- genErrorRecovery
  return $ TypeError
    { errorId = "test-id"
    , severity = severity
    , category = category
    , message = T.pack message
    , location = location
    , context = context
    , recovery = recovery
    , suggestions = map T.pack suggestions
    , relatedErrors = relatedErrors
    , errorChain = []
    , timestamp = Nothing
    }

genCombinedError :: Gen CombinedError
genCombinedError = do
  errors <- listOf1 genTypeError
  return $ CombinedError errors

-- ============================================================================
-- Properties for Error Creation L.and Manipulation
-- ============================================================================

prop_errorAt_creates_error_with_location :: ErrorSeverity -> String -> Property
prop_errorAt_creates_error_with_location severity message =
  let location = ErrorLocation (Just "test.typus") 10 20 Nothing Nothing
      error = errorAt "test-id" (T.pack message) location
  in property $ severity error === severity .&&.
               location error === location .&&.
               message error === T.pack message

prop_errorWithCategory_creates_error_with_category :: ErrorSeverity -> ErrorCategory -> String -> Property
prop_errorWithCategory_creates_error_with_category severity category message =
  let location = ErrorLocation (Just "module.typus") 5 10 Nothing Nothing
      error = errorWithCategory "test-id" category (T.pack message) location
  in property $ severity error === severity .&&.
               category error === category .&&.
               location error === location .&&.
               message error === T.pack message

prop_warningAt_creates_warning :: String -> Property
prop_warningAt_creates_warning message =
  let location = ErrorLocation (Just "test.typus") 1 1 Nothing Nothing
      warning = warningAt "test-id" (T.pack message) location
  in property $ severity warning === Warning .&&.
               message warning === T.pack message

prop_warningWithCategory_creates_warning_with_category :: ErrorCategory -> String -> Property
prop_warningWithCategory_creates_warning_with_category category message =
  let location = ErrorLocation 15 30 "file.typus"
      warning = warningWithCategory category location (T.pack message)
  in property $ errorSeverity warning === ErrorWarning .&&.
               errorCategory warning === category .&&.
               errorMessage warning === T.pack message

prop_infoAt_creates_info :: String -> Property
prop_infoAt_creates_info message =
  let location = ErrorLocation 100 200 "info.typus"
      info = infoAt "test-id" (T.pack message)
  in property $ errorSeverity info === ErrorInfo .&&.
               errorMessage info === T.pack message

prop_infoWithCategory_creates_info_with_category :: ErrorCategory -> String -> Property
prop_infoWithCategory_creates_info_with_category category message =
  let location = ErrorLocation 50 75 "data.typus"
      info = infoWithCategory category location (T.pack message)
  in property $ errorSeverity info === ErrorInfo .&&.
               errorCategory info === category .&&.
               errorMessage info === T.pack message

prop_fatalError_creates_fatal_error :: String -> Property
prop_fatalError_creates_fatal_error message =
  let location = ErrorLocation 25 50 "fatal.typus"
      fatal = fatalError location (T.pack message)
  in property $ errorSeverity fatal === ErrorFatal .&&.
               errorMessage fatal === T.pack message

prop_fatalErrorWithCategory_creates_fatal_error_with_category :: ErrorCategory -> String -> Property
prop_fatalErrorWithCategory_creates_fatal_error_with_category category message =
  let location = ErrorLocation 33 66 "critical.typus"
      fatal = fatalErrorWithCategory category location (T.pack message)
  in property $ errorSeverity fatal === ErrorFatal .&&.
               errorCategory error === category .&&.
               errorMessage fatal === T.pack message

-- ============================================================================
-- Properties for Error Modification
-- ============================================================================

prop_withLocation_updates_error_location :: TypeError -> ErrorLocation -> Property
prop_withLocation_updates_error_location error newLocation =
  let updatedError = withLocation newLocation error
  in property $ errorLocation updatedError === newLocation .&&.
               errorMessage updatedError === errorMessage error .&&.
               errorSeverity updatedError === errorSeverity error

prop_withContext_updates_error_context :: TypeError -> ErrorContext -> Property
prop_withContext_updates_error_context error newContext =
  let updatedError = withContext newContext error
  in property $ errorContext updatedError === newContext .&&.
               errorMessage updatedError === errorMessage error .&&.
               errorSeverity updatedError === errorSeverity error

prop_withSuggestions_adds_suggestions :: TypeError -> [String] -> Property
prop_withSuggestions_adds_suggestions error suggestions =
  let newSuggestions = map T.pack suggestions
      updatedError = withSuggestions newSuggestions error
  in property $ errorSuggestions updatedError === newSuggestions .&&.
               errorMessage updatedError === errorMessage error .&&.
               errorSeverity updatedError === errorSeverity error

prop_withRelatedErrors_adds_related_errors :: TypeError -> [TypeError] -> Property
prop_withRelatedErrors_adds_related_errors error related =
  let updatedError = withRelatedErrors related error
  in property $ relatedErrors updatedError === related .&&.
               errorMessage updatedError === errorMessage error .&&.
               errorSeverity updatedError === errorSeverity error

-- ============================================================================
-- Properties for Error Combination
-- ============================================================================

prop_combineErrors_merges_errors :: [TypeError] -> Property
prop_combineErrors_merges_errors errors =
  not (null errors) ==>
  let combined = combineErrors errors
      combinedErrors = combinedErrors combined
  in property $ L.length combinedErrors === L.length errors .&&.
               all (`elem` combinedErrors) errors

prop_combinedErrorSeverity_returns_max_severity :: [TypeError] -> Property
prop_combinedErrorSeverity_returns_max_severity errors =
  not (null errors) ==>
  let combined = combineErrors errors
      severity = combinedErrorSeverity combined
      severities = map errorSeverity errors
      maxSeverity = L.maximum severities
  in property $ severity === maxSeverity

prop_filterCombinedErrorsBySeverity_filters_correctly :: CombinedError -> ErrorSeverity -> Property
prop_filterCombinedErrorsBySeverity_filters_correctly combined severity =
  let filtered = filterCombinedErrorsBySeverity severity combined
      filteredErrors = combinedErrors filtered
      originalErrors = combinedErrors combined
  in property $ L.all (\e -> errorSeverity e <= severity) filteredErrors .&&.
               all (\e -> errorSeverity e <= severity ==> e `elem` filteredErrors) originalErrors

-- ============================================================================
-- Properties for Error Collection
-- ============================================================================

prop_error_collector_manages_errors :: [TypeError] -> [TypeError] -> [TypeError] -> Property
prop_error_collector_manages_errors errors warnings infos =
  let collector = newErrorCollector
      withErrors = L.foldl (\c e -> addError e c) collector errors
      withWarnings = L.foldl (\c w -> addWarning w c) withErrors warnings
      withInfos = L.foldl (\c i -> addInfo i c) withWarnings infos
      
      collectedErrors = getErrors withInfos
      collectedWarnings = getWarnings withInfos
      collectedInfos = getInfo withInfos
  in property $ L.length collectedErrors === L.length errors .&&.
               length collectedWarnings === L.length warnings .&&.
               length collectedInfos === L.length infos .&&.
               hasErrors withInfos === not (null errors) .&&.
               hasWarnings withInfos === not (null warnings)

-- ============================================================================
-- Properties for Error Filtering
-- ============================================================================

prop_hasCategory_checks_category_correctly :: TypeError -> ErrorCategory -> Property
prop_hasCategory_checks_category_correctly error category =
  let hasCat = hasCategory category error
      errorCat = errorCategory error
  in property $ hasCat === (errorCat == category)

prop_filterByCategory_filters_correctly :: [TypeError] -> ErrorCategory -> Property
prop_filterByCategory_filters_correctly errors category =
  let filtered = filterByCategory category errors
  in property $ L.all (\e -> errorCategory e == category) filtered .&&.
               all (\e -> errorCategory e == category ==> e `elem` filtered) errors

prop_filterBySeverity_filters_correctly :: [TypeError] -> ErrorSeverity -> Property
prop_filterBySeverity_filters_correctly errors severity =
  let filtered = filterBySeverity severity errors
  in property $ L.all (\e -> errorSeverity e <= severity) filtered .&&.
               all (\e -> errorSeverity e <= severity ==> e `elem` filtered) errors

-- ============================================================================
-- Properties for Error Recovery
-- ============================================================================

prop_canRecoverFrom_checks_recovery_possibility :: TypeError -> Property
prop_canRecoverFrom_checks_recovery_possibility error =
  let recovery = recoveryStrategy error
      canRecover = canRecoverFrom error
  in property $ canRecover === (recovery /= RecoveryAbort && recovery /= CustomRecovery "fatal")

prop_shouldContinueAfter_checks_continuation :: TypeError -> Property
prop_shouldContinueAfter_checks_continuation error =
  let recovery = recoveryStrategy error
      shouldContinue = shouldContinueAfter error
  in property $ shouldContinue === (recovery == RecoveryContinue || recovery == RecoveryRetry)

prop_customRecovery_creates_custom_strategy :: String -> Property
prop_customRecovery_creates_custom_strategy strategyName =
  let recovery = customRecovery strategyName
  in case recovery of
    CustomRecovery name -> property $ name === T.pack strategyName
    _ -> property $ False

-- ============================================================================
-- Properties for Error Formatting
-- ============================================================================

prop_formatError_includes_message :: TypeError -> Property
prop_formatError_includes_message error =
  let formatted = formatError error
      message = errorMessage error
  in property $ message `L.isInfixOf` formatted

prop_formatErrors_includes_all_messages :: [TypeError] -> Property
prop_formatErrors_includes_all_messages errors =
  not (null errors) ==>
  let formatted = formatErrors errors
      messages = map errorMessage errors
  in property $ L.all (`L.isInfixOf` formatted) messages

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Core ErrorHandler QuickCheck Tests"
  [ testGroup "Error Creation Properties"
    [ fastProperty "errorAt "test-id" messages" prop_formatErrors_includes_all_messages
    ]
  ]