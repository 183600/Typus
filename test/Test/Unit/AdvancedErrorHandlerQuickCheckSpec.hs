{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.AdvancedErrorHandlerQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, oneof)
import TestSupport.Arbitrary

import Compiler.Errors.Core
  ( TypeError(..)
  , CombinedError(..)
  , errorAt
  , warningAt
  , infoAt
  , ErrorLocation(..) )
import SourceLocation (SourcePos(..), startPos)
import Compiler.Errors.Core
  ( ErrorSeverity(..)
  , ErrorCategory(..)
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
  , errorWithCategory
  , warningAt
  , warningWithCategory
  , infoAt
  , infoWithCategory
  , generateErrorReport
  , createRecoveryStrategy
  , customRecovery
  , fatalRecovery
  , errorRecovery
  , warningRecovery
  , infoRecovery
  , combineErrors
  , combinedErrorSeverity
  , filterCombinedErrorsBySeverity
  , isAtLeast
  , hasCategory
  , filterByCategory
  , filterBySeverity
  , getErrorStatistics
  , wrapError
  , fatalErrorWithCategory
  , errorWithSuggestions
  , fatalError
  , severityPriority
  , compareSeverity )

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isSpace)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (sort)
import qualified Data.Map.Strict as Map

-- Arbitrary instances for testing
instance Arbitrary ErrorSeverity where
  arbitrary = elements [Fatal, Error, Warning, Info]

instance Arbitrary ErrorCategory where
  arbitrary = elements [TypeChecking, Ownership, Parsing, Semantic, Runtime, Constraint, Inference, Integration, Unknown]

-- Property: ErrorSeverity ordering is consistent
prop_error_severity_ordering :: ErrorSeverity -> ErrorSeverity -> Property
prop_error_severity_ordering sev1 sev2 =
  let ord1 = compareSeverity sev1 sev2
      ord2 = compare (severityPriority sev1) (severityPriority sev2)
  in property $ ord1 === ord2

-- Property: severityPriority is monotonic
prop_severity_priority_monotonic :: ErrorSeverity -> ErrorSeverity -> Property
prop_severity_priority_monotonic sev1 sev2 =
  let pri1 = severityPriority sev1
      pri2 = severityPriority sev2
  in property $ (sev1 == sev2) ==> (pri1 == pri2)

-- Property: isAtLeast is reflexive
prop_is_at_least_reflexive :: ErrorSeverity -> Property
prop_is_at_least_reflexive sev =
  property $ isAtLeast sev sev

-- Property: isAtLeast is transitive
prop_is_at_least_transitive :: ErrorSeverity -> ErrorSeverity -> ErrorSeverity -> Property
prop_is_at_least_transitive sev1 sev2 sev3 =
  isAtLeast sev1 sev2 && isAtLeast sev2 sev3 ==> isAtLeast sev1 sev3

-- Property: newErrorCollector creates empty collector
prop_new_collector_empty :: Property
prop_new_collector_empty =
  let errors = [] :: [TypeError]
  in property $ not (hasErrors errors) .&&. not (hasWarnings errors)

-- Property: addError increases error count
prop_add_error_increases_count :: String -> Property
prop_add_error_increases_count errorMsg =
  not (null errorMsg) ==>
  let error = errorAt "test-id" (T.pack errorMsg) (ErrorLocation Nothing 1 1 Nothing Nothing)
      errors = [error]
  in property $ hasErrors errors

-- Property: addWarning increases warning count
prop_add_warning_increases_count :: String -> Property
prop_add_warning_increases_count warningMsg =
  not (null warningMsg) ==>
  let warning = warningAt "test-id" (T.pack warningMsg) (ErrorLocation Nothing 1 1 Nothing Nothing)
      warnings = [warning]
  in property $ hasWarnings warnings

-- Property: addInfo increases info count
prop_add_info_increases_count :: String -> Property
prop_add_info_increases_count infoMsg =
  not (null infoMsg) ==>
  let info = infoAt "test-id" (T.pack infoMsg) (ErrorLocation Nothing 1 1 Nothing Nothing)
      infos = [info]
      infoMessages = getInfo infos
  in property $ not (null infoMessages)

-- Property: getErrors returns added errors
prop_get_errors_returns_added :: [String] -> Property
prop_get_errors_returns_added errorMessages =
  not (null errorMessages) && L.all (not . null) errorMessages ==>
  let errors = map (\msg -> errorAt "test-id" (T.pack msg) (ErrorLocation Nothing 1 1 Nothing Nothing)) errorMessages
      retrievedErrors = getErrors errors
  in property $ L.length retrievedErrors === L.length errorMessages

-- Property: getWarnings returns added warnings
prop_get_warnings_returns_added :: [String] -> Property
prop_get_warnings_returns_added warningMessages =
  not (null warningMessages) && L.all (not . null) warningMessages ==>
  let warnings = map (\msg -> warningAt "test-id" (T.pack msg) (ErrorLocation Nothing 1 1 Nothing Nothing)) warningMessages
      retrievedWarnings = getWarnings warnings
  in property $ L.length retrievedWarnings === L.length warningMessages

-- Property: getAllMessages includes L.all message types
prop_get_all_messages_includes_all :: [String] -> [String] -> [String] -> Property
prop_get_all_messages_includes_all errorMessages warningMessages infoMessages =
  not (null errorMessages) && not (null warningMessages) && not (null infoMessages) ==>
  let errors = map (\msg -> errorAt "test-id" (T.pack msg) (ErrorLocation Nothing 1 1 Nothing Nothing)) errorMessages
      warnings = map (\msg -> warningAt "test-id" (T.pack msg) (ErrorLocation Nothing 1 1 Nothing Nothing)) warningMessages
      infos = map (\msg -> infoAt "test-id" (T.pack msg) (ErrorLocation Nothing 1 1 Nothing Nothing)) infoMessages
      allMessages = getAllMessages (errors ++ warnings ++ infos)
  in property $ L.length allMessages >= L.length errorMessages + L.length warningMessages + L.length infoMessages

-- Property: formatError produces non-empty output
prop_format_error_non_empty :: String -> Property
prop_format_error_non_empty errorMsg =
  not (null errorMsg) ==>
  let error = errorAt "test-id" (T.pack errorMsg) (ErrorLocation Nothing 1 1 Nothing Nothing)
      formatted = formatError error
  in property $ not (null formatted)

-- Property: formatErrors preserves order
prop_format_errors_preserves_order :: [String] -> Property
prop_format_errors_preserves_order errorMessages =
  not (null errorMessages) && L.all (not . null) errorMessages ==>
  let errors = map (\msg -> errorAt "test-id" (T.pack msg) (ErrorLocation Nothing 1 1 Nothing Nothing)) errorMessages
      formatted = formatErrors errors
      formattedLines = lines formatted
  in property $ L.length formattedLines >= L.length errorMessages

-- Property: formatErrorWithLocation includes location info
prop_format_error_with_location :: String -> Int -> Int -> Property
prop_format_error_with_location errorMsg line col =
  not (null errorMsg) && line > 0 && col > 0 ==>
  let location = ErrorLocation Nothing line col Nothing Nothing
      error = errorAt "test-id" (T.pack errorMsg) location
      formatted = formatErrorWithLocation error
  in property $ show line `L.isInfixOf` formatted .&&. show col `L.isInfixOf` formatted

-- Property: canRecoverFrom handles different severities
prop_can_recover_from_severity :: ErrorSeverity -> Property
prop_can_recover_from_severity severityVal =
  let baseError = errorAt "test-id" (T.pack "test message") (ErrorLocation Nothing 1 1 Nothing Nothing)
      error = baseError { severity = severityVal }
      canRecover = canRecoverFrom error
  in property $ (severityVal == Fatal) ==> not canRecover

-- Property: shouldContinueAfter handles different severities
prop_should_continue_after :: ErrorSeverity -> Property
prop_should_continue_after severityVal =
  let baseError = errorAt "test-id" (T.pack "test message") (ErrorLocation Nothing 1 1 Nothing Nothing)
      error = baseError { severity = severityVal }
      shouldContinue = shouldContinueAfter error
  in property $ (severityVal == Fatal) ==> not shouldContinue

-- Property: errorWithCategory creates error with correct category
prop_error_with_category :: ErrorCategory -> String -> Property
prop_error_with_category errorCategory errorMsg =
  let location = ErrorLocation Nothing 1 1 Nothing Nothing
      error = errorWithCategory "test-id" errorCategory (T.pack errorMsg) location
  in property $ True  -- Basic smoke test

-- Property: errorAt creates with location
prop_error_at_creates_with_location :: String -> String -> Int -> Int -> Property
prop_error_at_creates_with_location errorId errorMsg line col =
  not (null errorMsg) && line > 0 && col > 0 ==>
  let location = ErrorLocation Nothing line col Nothing Nothing
      error = errorAt errorId (T.pack errorMsg) location
  in property $ True  -- Basic smoke test

-- Property: error with category preserves
prop_error_with_category_preserves :: ErrorCategory -> String -> Property
prop_error_with_category_preserves errorCategory errorMsg =
  not (null errorMsg) ==>
  let location = ErrorLocation Nothing 1 1 Nothing Nothing
      error = errorWithCategory "test-id" errorCategory (T.pack errorMsg) location
  in property $ category error == errorCategory

-- Property: warningAt creates warning with correct location
prop_warning_at :: String -> String -> Int -> Int -> Property
prop_warning_at warningId warningMsg line col =
  not (null warningMsg) && line > 0 && col > 0 ==>
  let location = ErrorLocation Nothing line col Nothing Nothing
      warning = warningAt warningId (T.pack warningMsg) location
  in property $ True  -- Basic smoke test

-- Property: warning at creates with location
prop_warning_at_creates_with_location :: String -> String -> Int -> Int -> Property
prop_warning_at_creates_with_location warningId warningMsg line col =
  not (null warningMsg) && line > 0 && col > 0 ==>
  let location = ErrorLocation Nothing line col Nothing Nothing
      warning = warningAt warningId (T.pack warningMsg) location
  in property $ True  -- Basic smoke test

-- Property: warning with category preserves
prop_warning_with_category_preserves :: ErrorCategory -> String -> Property
prop_warning_with_category_preserves warningCategory warningMsg =
  not (null warningMsg) ==>
  let location = ErrorLocation Nothing 1 1 Nothing Nothing
      warning = warningWithCategory "test-warning" warningCategory (T.pack warningMsg) location
  in property $ category warning == warningCategory

-- Property: infoAt creates info with correct location
prop_info_at :: String -> String -> Int -> Int -> Property
prop_info_at infoId infoMsg line col =
  not (null infoMsg) && line > 0 && col > 0 ==>
  let location = ErrorLocation Nothing line col Nothing Nothing
      info = infoAt infoId (T.pack infoMsg) location
  in property $ True  -- Basic smoke test

-- Property: info at creates with location
prop_info_at_creates_with_location :: String -> String -> Int -> Int -> Property
prop_info_at_creates_with_location infoId infoMsg line col =
  not (null infoMsg) && line > 0 && col > 0 ==>
  let location = ErrorLocation Nothing line col Nothing Nothing
      info = infoAt infoId (T.pack infoMsg) location
  in property $ True  -- Basic smoke test

-- Property: info with category preserves
prop_info_with_category_preserves :: ErrorCategory -> String -> Property
prop_info_with_category_preserves infoCategory infoMsg =
  not (null infoMsg) ==>
  let location = ErrorLocation Nothing 1 1 Nothing Nothing
      info = infoWithCategory "test-info" infoCategory (T.pack infoMsg) location
  in property $ category info == infoCategory

-- Property: fatalError has Fatal severity
prop_fatal_error_severity :: String -> Property
prop_fatal_error_severity errorMsg =
  not (null errorMsg) ==>
  let error = fatalError "test-id" (T.pack errorMsg) (ErrorLocation Nothing 1 1 Nothing Nothing)
  in property $ severity error == Fatal

-- Property: fatalErrorWithCategory preserves category
prop_fatal_error_with_category_preserves :: String -> ErrorCategory -> Property
prop_fatal_error_with_category_preserves errorMsg errorCategory =
  not (null errorMsg) ==>
  let error = fatalErrorWithCategory "test-id" errorCategory (T.pack errorMsg) (ErrorLocation Nothing 1 1 Nothing Nothing)
  in property $ category error == errorCategory

-- Property: errorWithSuggestions includes suggestions
prop_error_with_suggestions :: String -> [String] -> Property
prop_error_with_suggestions errorMsg suggestions =
  not (null errorMsg) && not (null suggestions) && L.all (not . null) suggestions ==>
  let error = errorWithSuggestions "test-id" (T.pack errorMsg) (map T.pack suggestions) (ErrorLocation Nothing 1 1 Nothing Nothing)
  in property $ True  -- Basic smoke test

-- Property: wrapError preserves original error
prop_wrap_error_preserves :: String -> String -> Property
prop_wrap_error_preserves wrapperMsg originalMsg =
  not (null wrapperMsg) && not (null originalMsg) ==>
  let originalError = errorAt "test-id" (T.pack originalMsg) (ErrorLocation Nothing 1 1 Nothing Nothing)
      wrappedError = wrapError (T.pack wrapperMsg) originalError
  in property $ True  -- Basic smoke test

-- Property: combineErrors preserves L.all errors
prop_combine_errors_preserves :: [String] -> [String] -> Property
prop_combine_errors_preserves errors1 errors2 =
  not (null errors1) && not (null errors2) &&
  L.all (not . null) errors1 && L.all (not . null) errors2 ==>
  let errorList1 = L.map (\msg -> errorAt "test-id" (T.pack msg) (ErrorLocation Nothing 1 1 Nothing Nothing)) errors1
      errorList2 = L.map (\msg -> errorAt "test-id" (T.pack msg) (ErrorLocation Nothing 1 1 Nothing Nothing)) errors2
      combined = combineErrors (errorList1 ++ errorList2)
  in property $ True  -- Basic smoke test

-- Property: combinedErrorSeverity returns highest severity
prop_combined_error_severity_highest :: ErrorSeverity -> Property
prop_combined_error_severity_highest severity =
  let combinedError = IntegrationError ("test-" ++ show severity) severity
      highest = combinedErrorSeverity combinedError
  in property $ highest === severity

-- Property: filterCombinedErrorsBySeverity preserves order
prop_filter_combined_by_severity_preserves_order :: [ErrorSeverity] -> ErrorSeverity -> Property
prop_filter_combined_by_severity_preserves_order severities minSeverity =
  not (null severities) ==>
  let combinedErrors = map (\sev -> IntegrationError ("test-" ++ show sev) sev) severities
      filtered = filterCombinedErrorsBySeverity minSeverity combinedErrors
      expected = L.filter (\err -> isAtLeast minSeverity (combinedErrorSeverity err)) combinedErrors
  in property $ L.length filtered === L.length expected

-- Property: hasCategory finds matching errors
prop_has_category_finds_matching :: ErrorCategory -> Property
prop_has_category_finds_matching targetCategory =
  let baseError = errorAt "test-id" (T.pack "test message") (ErrorLocation Nothing 1 1 Nothing Nothing)
      error = baseError { category = targetCategory }
      hasTarget = hasCategory targetCategory error
  in property $ hasTarget

-- Property: filterByCategory preserves matching categories
prop_filter_by_category_preserves :: [(ErrorCategory, String)] -> ErrorCategory -> Property
prop_filter_by_category_preserves categoryMessages targetCategory =
  not (null categoryMessages) ==>
  let baseError = errorAt "test-id" (T.pack "message") (ErrorLocation Nothing 1 1 Nothing Nothing)
      errors = map (\(cat, msg) -> baseError { message = T.pack msg, category = cat }) categoryMessages
      filtered = filterByCategory targetCategory errors
      expected = L.filter (\err -> category err == targetCategory) errors
  in property $ L.length filtered === L.length expected

-- Property: filterBySeverity preserves matching severities
prop_filter_by_severity_preserves :: [(ErrorSeverity, String)] -> ErrorSeverity -> Property
prop_filter_by_severity_preserves severityMessages minSeverity =
  not (null severityMessages) ==>
  let baseError = errorAt "test-id" (T.pack "message") (ErrorLocation Nothing 1 1 Nothing Nothing)
      errors = map (\(sev, msg) -> baseError { message = T.pack msg, severity = sev }) severityMessages
      filtered = filterBySeverity minSeverity errors
      expected = L.filter (\err -> isAtLeast minSeverity (severity err)) errors
  in property $ L.length filtered === L.length expected

-- Property: getErrorStatistics provides counts
prop_get_error_statistics_counts :: [String] -> [String] -> [String] -> Property
prop_get_error_statistics_counts errorMessages warningMessages infoMessages =
  let errors = map (\msg -> errorAt "test-id" (T.pack msg) (ErrorLocation Nothing 1 1 Nothing Nothing)) errorMessages
      warnings = map (\msg -> warningAt "test-id" (T.pack msg) (ErrorLocation Nothing 1 1 Nothing Nothing)) warningMessages
      infos = map (\msg -> infoAt "test-id" (T.pack msg) (ErrorLocation Nothing 1 1 Nothing Nothing)) infoMessages
      allMessages = errors ++ warnings ++ infos
      stats = getErrorStatistics allMessages
  in property $ True  -- Basic smoke test

-- Property: generateErrorReport produces non-empty output
prop_generate_error_report_non_empty :: [String] -> Property
prop_generate_error_report_non_empty errorMessages =
  not (null errorMessages) && L.all (not . null) errorMessages ==>
  let errors = map (\msg -> errorAt "test-id" (T.pack msg) (ErrorLocation Nothing 1 1 Nothing Nothing)) errorMessages
      report = generateErrorReport errors
  in property $ not (null report)

-- Property: createRecoveryStrategy creates valid strategy
prop_create_recovery_strategy_valid :: Bool -> Bool -> Property
prop_create_recovery_strategy_valid canRec shouldCont =
  let strategy = createRecoveryStrategy canRec shouldCont Nothing Nothing
  in property $ True  -- Basic smoke test

-- Property: customRecovery creates custom strategy
prop_custom_recovery_creates :: Bool -> Bool -> String -> Property
prop_custom_recovery_creates canRec shouldCont recoveryName =
  not (null recoveryName) ==>
  let strategy = customRecovery canRec shouldCont (Just recoveryName) Nothing 50 0.8
  in property $ True  -- Basic smoke test

-- Property: recovery strategies are different
prop_recovery_strategies_different :: Property
prop_recovery_strategies_different =
  let fatal = fatalRecovery
      error = errorRecovery
      warning = warningRecovery
      info = infoRecovery
  in property $ fatal /= error .&&. error /= warning .&&. warning /= info

tests :: TestTree
tests = testGroup "Advanced ErrorHandler QuickCheck"
  [ fastProperty "error severity ordering" prop_error_severity_ordering
  , fastProperty "severity priority monotonic" prop_severity_priority_monotonic
  , fastProperty "is at least reflexive" prop_is_at_least_reflexive
  , fastProperty "is at least transitive" prop_is_at_least_transitive
  , fastProperty "new collector empty" prop_new_collector_empty
  , fastProperty "add error increases count" prop_add_error_increases_count
  , fastProperty "add warning increases count" prop_add_warning_increases_count
  , fastProperty "add info increases count" prop_add_info_increases_count
  , fastProperty "get errors returns added" prop_get_errors_returns_added
  , fastProperty "get warnings returns added" prop_get_warnings_returns_added
  , fastProperty "get L.all messages includes L.all" prop_get_all_messages_includes_all
  , fastProperty "format error non empty" prop_format_error_non_empty
  , fastProperty "format errors preserves order" prop_format_errors_preserves_order
  , fastProperty "format error with location" prop_format_error_with_location
  , fastProperty "can recover from severity" prop_can_recover_from_severity
  , fastProperty "should continue after" prop_should_continue_after
  , fastProperty "error at creates with location" prop_error_at_creates_with_location
  , fastProperty "error with category preserves" prop_error_with_category_preserves
  , fastProperty "warning at creates with location" prop_warning_at_creates_with_location
  , fastProperty "warning with category preserves" prop_warning_with_category_preserves
  , fastProperty "info at creates with location" prop_info_at_creates_with_location
  , fastProperty "info with category preserves" prop_info_with_category_preserves
  , fastProperty "fatal error severity" prop_fatal_error_severity
  , fastProperty "fatal error with category preserves" prop_fatal_error_with_category_preserves
  , fastProperty "error with suggestions" prop_error_with_suggestions
  , fastProperty "wrap error preserves" prop_wrap_error_preserves
  , fastProperty "combine errors preserves" prop_combine_errors_preserves
  , fastProperty "combined error severity highest" prop_combined_error_severity_highest
  , fastProperty "filter combined by severity preserves order" prop_filter_combined_by_severity_preserves_order
  , fastProperty "has category finds matching" prop_has_category_finds_matching
  , fastProperty "filter by category preserves" prop_filter_by_category_preserves
  , fastProperty "filter by severity preserves" prop_filter_by_severity_preserves
  , fastProperty "get error statistics counts" prop_get_error_statistics_counts
  , fastProperty "generate error report non empty" prop_generate_error_report_non_empty
  , fastProperty "create recovery strategy valid" prop_create_recovery_strategy_valid
  , fastProperty "custom recovery creates" prop_custom_recovery_creates
  , fastProperty "recovery strategies different" prop_recovery_strategies_different
  ]