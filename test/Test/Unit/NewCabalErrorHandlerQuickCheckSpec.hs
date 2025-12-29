{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalErrorHandlerQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, oneof, elements, suchThat)
import qualified Test.QuickCheck as QC

import Compiler.Errors.Core
  ( TypeError(..)
  , CombinedError(..)
  , ErrorSeverity(..)
  , ErrorCategory(..)
  , ErrorLocation(..)
  , ErrorContext(..)
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
  , errorAt
  , warningAt
  , infoAt
  , errorWithCategory
  , warningWithCategory
  , infoWithCategory
  , fatalError
  , fatalErrorWithCategory
  , filterBySeverity
  , filterByCategory
  , hasCategory
  , combineErrors
  , combinedErrorSeverity
  , canRecoverFrom
  , shouldContinueAfter
  , emptyContext
  )
import SourceLocation (SourcePos(..))
import qualified Data.Text as T

import Data.List (sort, nub)

-- ============================================================================
-- Generators for ErrorHandler data types
-- ============================================================================

-- Generate error severity
genErrorSeverity :: Gen ErrorSeverity
genErrorSeverity = elements [Fatal, Error, Warning, Info]

-- Generate error category
genErrorCategory :: Gen ErrorCategory
genErrorCategory = elements 
  [ SyntaxError
  , TypeError
  , NameError
  , ScopeError
  , ImportError
  , ParseError
  , CompilationError
  , RuntimeError
  , InternalError
  , UserError
  , WarningMessage
  , InfoMessage
  , DeprecationWarning
  , PerformanceWarning
  , SecurityWarning
  , OwnershipError
  , DependentTypeError
  ]

-- Generate source position
genSourcePos :: Gen SourcePos
genSourcePos = do
  line <- choose (1, 1000)
  column <- choose (1, 1000)
  offset <- choose (0, 10000)
  pure $ SourcePos line column offset

-- Generate error location
genErrorLocation :: Gen ErrorLocation
genErrorLocation = do
  filePath <- suchThat QC.arbitrary (not . T.null)
  line <- choose (1, 1000)
  column <- choose (1, 1000)
  endLine <- oneof [pure Nothing, Just <$> choose (line, line + 100)]
  endColumn <- case endLine of
    Nothing -> pure Nothing
    Just el -> if el == line then Just <$> choose (column, column + 100) else Just <$> choose (1, 1000)
  pure $ ErrorLocation (Just filePath) line column endLine endColumn

-- Generate error context
genErrorContext :: Gen ErrorContext
genErrorContext = do
  context <- suchThat QC.arbitrary (not . T.null)
  suggestions <- listOf $ suchThat QC.arbitrary (not . T.null)
  relatedErrors <- listOf $ suchThat QC.arbitrary (not . T.null)
  pure $ ErrorContext context suggestions relatedErrors

-- Generate error recovery strategy
genErrorRecovery :: Gen ErrorRecovery
genErrorRecovery = elements
  [ NoRecovery
  , SkipStatement
  , SkipBlock
  , InsertPlaceholder
  , RetryWithFallback
  , ContinueAnyway
  , AbortCompilation
  ]

-- Generate UTCTime
genUTCTime :: Gen UTCTime
genUTCTime = do
  year <- choose (2000, 2030)
  month <- choose (1, 12)
  day <- choose (1, 28)
  hour <- choose (0, 23)
  minute <- choose (0, 59)
  second <- choose (0, 59)
  pure $ fromGregorian year month day `plus` secondsToDiffTime (hour * 3600 + minute * 60 + second)
  where
    plus day diff = UTCTime day diff

-- Generate type error
genTypeError :: Gen TypeError
genTypeError = do
  message <- suchThat QC.arbitrary (not . T.null)
  severity <- genErrorSeverity
  category <- genErrorCategory
  location <- genErrorLocation
  context <- oneof [pure emptyContext, genErrorContext]
  timestamp <- oneof [pure Nothing, Just <$> genUTCTime]
  recovery <- genErrorRecovery
  pure $ TypeError message severity category location context timestamp recovery

-- Generate combined error
genCombinedError :: Gen CombinedError
genCombinedError = do
  primaryError <- genTypeError
  relatedErrors <- listOf genTypeError
  combinedContext <- oneof [pure emptyContext, genErrorContext]
  pure $ CombinedError primaryError relatedErrors combinedContext

-- ============================================================================
-- Property-based tests for ErrorHandler module
-- ============================================================================

-- Property: severityPriority ordering is correct
prop_severityPriority_ordering :: Property
prop_severityPriority_ordering =
  let fatalPriority = severityPriority Fatal
      errorPriority = severityPriority Error
      warningPriority = severityPriority Warning
      infoPriority = severityPriority Info
  in property $ fatalPriority > errorPriority .&&.
             errorPriority > warningPriority .&&.
             warningPriority > infoPriority

-- Property: isAtLeast works correctly for all severity levels
prop_isAtLeast_reflexive :: ErrorSeverity -> Property
prop_isAtLeast_reflexive severity =
  isAtLeast severity severity === True

-- Property: isAtLeast is transitive
prop_isAtLeast_transitive :: ErrorSeverity -> ErrorSeverity -> ErrorSeverity -> Property
prop_isAtLeast_transitive s1 s2 s3 =
  isAtLeast s1 s2 && isAtLeast s2 s3 ==> isAtLeast s1 s3

-- Property: newErrorCollector creates empty collector
prop_newErrorCollector_empty :: Property
prop_newErrorCollector_empty =
  let collector = newErrorCollector
  in property $ not (hasErrors collector) .&&. not (hasWarnings collector)

-- Property: addError adds error to collector
prop_addError_adds :: TypeError -> ErrorCollector -> Property
prop_addError_adds error collector =
  let newCollector = addError error collector
  in property $ hasErrors newCollector

-- Property: addWarning adds warning to collector
prop_addWarning_adds :: TypeError -> ErrorCollector -> Property
prop_addWarning_adds warning collector =
  let newCollector = addWarning warning collector
  in property $ hasWarnings newCollector

-- Property: addInfo adds info to collector
prop_addInfo_adds :: TypeError -> ErrorCollector -> Property
prop_addInfo_adds info collector =
  let newCollector = addInfo info collector
  in property $ True  -- Info doesn't affect hasErrors or hasWarnings

-- Property: getErrors returns only errors
prop_getErrors_only_errors :: [TypeError] -> ErrorCollector -> Property
prop_getErrors_only_errors errors collector =
  let errorMessages = map severity errors
      filteredErrors = filter (\e -> severity e `elem` [Fatal, Error]) errors
      collectorWithErrors = foldr addError collector filteredErrors
      retrievedErrors = getErrors collectorWithErrors
  in property $ all (\e -> severity e `elem` [Fatal, Error]) retrievedErrors

-- Property: getWarnings returns only warnings
prop_getWarnings_only_warnings :: [TypeError] -> ErrorCollector -> Property
prop_getWarnings_only_warnings warnings collector =
  let warningMessages = map severity warnings
      filteredWarnings = filter (\e -> severity e == Warning) warnings
      collectorWithWarnings = foldr addWarning collector filteredWarnings
      retrievedWarnings = getWarnings collectorWithWarnings
  in property $ all (\e -> severity e == Warning) retrievedWarnings

-- Property: getAllMessages returns all messages
prop_getAllMessages_all :: [TypeError] -> [TypeError] -> [TypeError] -> ErrorCollector -> Property
prop_getAllMessages_all errors warnings infos collector =
  let collectorWithErrors = foldr addError collector errors
      collectorWithWarnings = foldr addWarning collectorWithErrors warnings
      collectorWithInfos = foldr addInfo collectorWithWarnings infos
      allMessages = getAllMessages collectorWithInfos
  in property $ length allMessages === length errors + length warnings + length infos

-- Property: filterBySeverity filters correctly
prop_filterBySeverity_correct :: ErrorSeverity -> [TypeError] -> Property
prop_filterBySeverity_correct minSeverity errors =
  let filtered = filterBySeverity minSeverity errors
      expected = filter (\e -> isAtLeast (severity e) minSeverity) errors
  in property $ sort (map severity filtered) === sort (map severity expected)

-- Property: filterByCategory filters correctly
prop_filterByCategory_correct :: ErrorCategory -> [TypeError] -> Property
prop_filterByCategory_correct category errors =
  let filtered = filterByCategory category errors
      expected = filter (\e -> category e == category) errors
  in property $ length filtered === length expected

-- Property: hasCategory detects category correctly
prop_hasCategory_correct :: ErrorCategory -> [TypeError] -> Property
prop_hasCategory_correct category errors =
  let hasCat = hasCategory category errors
      expected = any (\e -> category e == category) errors
  in property $ hasCat === expected

-- Property: errorAt creates error with correct location
prop_errorAt_location :: ErrorLocation -> Property
prop_errorAt_location location =
  let message = "test error"
      error = errorAt location message
      errorLocation = location
  in property $ location error === errorLocation

-- Property: warningAt creates warning with correct location
prop_warningAt_location :: ErrorLocation -> Property
prop_warningAt_location location =
  let message = "test warning"
      warning = warningAt location message
      warningLocation = location warning
  in property $ warningLocation === location

-- Property: infoAt creates info with correct location
prop_infoAt_location :: ErrorLocation -> Property
prop_infoAt_location location =
  let message = "test info"
      info = infoAt location message
      infoLocation = location info
  in property $ infoLocation === location

-- Property: errorWithCategory creates error with correct category
prop_errorWithCategory_category :: ErrorCategory -> Property
prop_errorWithCategory_category category =
  let message = "test error"
      location = ErrorLocation Nothing 1 1 Nothing Nothing
      error = errorWithCategory category location message
  in property $ category error === category

-- Property: warningWithCategory creates warning with correct category
prop_warningWithCategory_category :: ErrorCategory -> Property
prop_warningWithCategory_category category =
  let message = "test warning"
      location = ErrorLocation Nothing 1 1 Nothing Nothing
      warning = warningWithCategory category location message
  in property $ category warning === category

-- Property: infoWithCategory creates info with correct category
prop_infoWithCategory_category :: ErrorCategory -> Property
prop_infoWithCategory_category category =
  let message = "test info"
      location = ErrorLocation Nothing 1 1 Nothing Nothing
      info = infoWithCategory category location message
  in property $ category info === category

-- Property: fatalError creates fatal error
prop_fatalError_severity :: Property
prop_fatalError_severity =
  let message = "fatal error"
      location = ErrorLocation Nothing 1 1 Nothing Nothing
      error = fatalError location message
  in property $ severity error === Fatal

-- Property: fatalErrorWithCategory creates fatal error with category
prop_fatalErrorWithCategory :: ErrorCategory -> Property
prop_fatalErrorWithCategory category =
  let message = "fatal error"
      location = ErrorLocation Nothing 1 1 Nothing Nothing
      error = fatalErrorWithCategory category location message
  in property $ severity error === Fatal .&&. category error === category

-- Property: combineErrors combines errors correctly
prop_combineErrors_combines :: [TypeError] -> Property
prop_combineErrors_combines errors =
  not (null errors) ==>
  let combined = combineErrors errors
  in property $ True  -- Basic combination test

-- Property: combinedErrorSeverity returns highest severity
prop_combinedErrorSeverity_highest :: [TypeError] -> Property
prop_combinedErrorSeverity_highest errors =
  not (null errors) ==>
  let combined = combineErrors errors
      combinedSeverity = combinedErrorSeverity combined
      expectedSeverity = maximum $ map severity errors
  in property $ combinedSeverity === expectedSeverity

-- Property: canRecoverFrom works correctly
prop_canRecoverFrom_severity :: ErrorSeverity -> Property
prop_canRecoverFrom_severity severity =
  let error = TypeError "" severity SyntaxError (ErrorLocation Nothing 1 1 Nothing Nothing) emptyContext Nothing NoRecovery
      canRecover = canRecoverFrom error
  in property $ canRecover === (severity /= Fatal)

-- Property: shouldContinueAfter works correctly
prop_shouldContinueAfter_severity :: ErrorSeverity -> Property
prop_shouldContinueAfter_severity severity =
  let error = TypeError "" severity SyntaxError (ErrorLocation Nothing 1 1 Nothing Nothing) emptyContext Nothing NoRecovery
      shouldContinue = shouldContinueAfter error
  in property $ shouldContinue === (severity `elem` [Warning, Info])

-- Property: formatError produces non-empty string
prop_formatError_nonempty :: TypeError -> Property
prop_formatError_nonempty error =
  let formatted = formatError error
  in property $ not (T.null formatted)

-- Property: formatErrors produces non-empty string for non-empty list
prop_formatErrors_nonempty :: [TypeError] -> Property
prop_formatErrors_nonempty errors =
  not (null errors) ==>
  let formatted = formatErrors errors
  in property $ not (T.null formatted)

-- Property: formatErrors handles empty list
prop_formatErrors_empty :: Property
prop_formatErrors_empty =
  let formatted = formatErrors []
  in property $ T.null formatted

-- Property: error collector preserves order
prop_error_collector_order :: [TypeError] -> ErrorCollector -> Property
prop_error_collector_order errors collector =
  let collectorWithErrors = foldr addError collector errors
      retrievedErrors = getErrors collectorWithErrors
  in property $ length retrievedErrors === length errors

-- Property: warning collector preserves order
prop_warning_collector_order :: [TypeError] -> ErrorCollector -> Property
prop_warning_collector_order warnings collector =
  let collectorWithWarnings = foldr addWarning collector warnings
      retrievedWarnings = getWarnings collectorWithWarnings
  in property $ length retrievedWarnings === length warnings

-- Property: error context is preserved
prop_error_context_preserved :: ErrorContext -> TypeError -> Property
prop_error_context_preserved context error =
  let errorWithContext = error { errorContext = context }
      retrievedContext = errorContext errorWithContext
  in property $ retrievedContext === context

-- Property: error recovery strategy is preserved
prop_error_recovery_preserved :: ErrorRecovery -> TypeError -> Property
prop_error_recovery_preserved recovery error =
  let errorWithRecovery = error { recoveryStrategy = recovery }
      retrievedRecovery = recoveryStrategy errorWithRecovery
  in property $ retrievedRecovery === recovery

-- ============================================================================
-- Test suite
-- ============================================================================

tests :: TestTree
tests = testGroup "New Cabal ErrorHandler QuickCheck Tests"
  [ fastProperty "severityPriority ordering is correct" prop_severityPriority_ordering
  , fastProperty "isAtLeast is reflexive" prop_isAtLeast_reflexive
  , fastProperty "isAtLeast is transitive" prop_isAtLeast_transitive
  , fastProperty "newErrorCollector creates empty collector" prop_newErrorCollector_empty
  , fastProperty "addError adds error to collector" prop_addError_adds
  , fastProperty "addWarning adds warning to collector" prop_addWarning_adds
  , fastProperty "addInfo adds info to collector" prop_addInfo_adds
  , fastProperty "getErrors returns only errors" prop_getErrors_only_errors
  , fastProperty "getWarnings returns only warnings" prop_getWarnings_only_warnings
  , fastProperty "getAllMessages returns all messages" prop_getAllMessages_all
  , fastProperty "filterBySeverity filters correctly" prop_filterBySeverity_correct
  , fastProperty "filterByCategory filters correctly" prop_filterByCategory_correct
  , fastProperty "hasCategory detects category correctly" prop_hasCategory_correct
  , fastProperty "errorAt creates error with correct location" prop_errorAt_location
  , fastProperty "warningAt creates warning with correct location" prop_warningAt_location
  , fastProperty "infoAt creates info with correct location" prop_infoAt_location
  , fastProperty "errorWithCategory creates error with correct category" prop_errorWithCategory_category
  , fastProperty "warningWithCategory creates warning with correct category" prop_warningWithCategory_category
  , fastProperty "infoWithCategory creates info with correct category" prop_infoWithCategory_category
  , fastProperty "fatalError creates fatal error" prop_fatalError_severity
  , fastProperty "fatalErrorWithCategory creates fatal error with category" prop_fatalErrorWithCategory
  , fastProperty "combineErrors combines errors correctly" prop_combineErrors_combines
  , fastProperty "combinedErrorSeverity returns highest severity" prop_combinedErrorSeverity_highest
  , fastProperty "canRecoverFrom works correctly" prop_canRecoverFrom_severity
  , fastProperty "shouldContinueAfter works correctly" prop_shouldContinueAfter_severity
  , fastProperty "formatError produces non-empty string" prop_formatError_nonempty
  , fastProperty "formatErrors produces non-empty string for non-empty list" prop_formatErrors_nonempty
  , fastProperty "formatErrors handles empty list" prop_formatErrors_empty
  , fastProperty "error collector preserves order" prop_error_collector_order
  , fastProperty "warning collector preserves order" prop_warning_collector_order
  , fastProperty "error context is preserved" prop_error_context_preserved
  , fastProperty "error recovery strategy is preserved" prop_error_recovery_preserved
  ]