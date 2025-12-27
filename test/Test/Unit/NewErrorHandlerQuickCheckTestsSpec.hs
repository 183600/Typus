{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewErrorHandlerQuickCheckTestsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, oneof, sized)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sort, nub)
import Data.Maybe (isJust, isNothing)
import Data.Time (UTCTime)

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
  , wrapError
  , combineErrors
  , combinedErrorSeverity
  , filterCombinedErrorsBySeverity
  , hasCategory
  , filterByCategory
  , filterBySeverity
  , getErrorStatistics
  , generateErrorReport
  , createRecoveryStrategy
  , customRecovery
  , fatalRecovery
  , errorRecovery
  , warningRecovery
  , infoRecovery
  )

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary ErrorSeverity where
  arbitrary = elements [ErrorInfo, Warning, Error, FatalError]

instance Arbitrary ErrorCategory where
  arbitrary = oneof
    [ return SyntaxError
    , return TypeError
    , return SemanticError
    , return OwnershipError
    , return DependencyError
    , return InternalError
    , return ConfigError
    , return IOError
    , return UserError
    , return WarningCategory
    , return InfoCategory
    ]

instance Arbitrary ErrorLocation where
  arbitrary = do
    line <- choose (1, 1000)
    column <- choose (1, 1000)
    file <- listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_-.")
    return $ ErrorLocation (T.pack file) line column

instance Arbitrary ErrorContext where
  arbitrary = do
    functionName <- listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")
    modulePath <- listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "/_")
    additionalInfo <- listOf (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " ")
    return $ ErrorContext (T.pack functionName) (T.pack modulePath) (T.pack additionalInfo)

instance Arbitrary ErrorRecovery where
  arbitrary = oneof
    [ return NoRecovery
    , return SkipCurrentBlock
    , return TryAlternative
    , return ContinueProcessing
    , return AbortCompilation
    , CustomRecovery <$> listOf (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " ")
    ]

instance Arbitrary TypeError where
  arbitrary = do
    message <- listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " .,!?")
    severity <- arbitrary
    category <- arbitrary
    location <- arbitrary
    context <- arbitrary
    suggestions <- listOf (listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "))
    relatedErrors <- listOf arbitrary
    recovery <- arbitrary
    return $ TypeError (T.pack message) severity category location context suggestions relatedErrors recovery

instance Arbitrary CombinedError where
  arbitrary = do
    errors <- listOf arbitrary
    return $ CombinedError errors

-- Generate error messages for testing
genErrorMessage :: Gen Text
genErrorMessage = T.pack <$> listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " .,!?")

-- Generate suggestions for testing
genSuggestions :: Gen [Text]
genSuggestions = listOf $ T.pack <$> listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " ")

-- ============================================================================
-- Property Tests
-- ============================================================================

-- Property: emptyContext has empty fields
prop_empty_context_correct :: Property
prop_empty_context_correct =
  let ErrorContext fn modulePath info = emptyContext
  in T.null fn .&&. T.null modulePath .&&. T.null info

-- Property: newErrorCollector creates empty collector
prop_new_error_collector_empty :: Property
prop_new_error_collector_empty =
  let collector = newErrorCollector
  in not (hasErrors collector) .&&. not (hasWarnings collector)

-- Property: addError increases error count
prop_add_error_increases_count :: TypeError -> Property
prop_add_error_increases_count error =
  let collector1 = newErrorCollector
      collector2 = addError error collector1
  in not (hasErrors collector1) .&&. hasErrors collector2

-- Property: addWarning increases warning count
prop_add_warning_increases_count :: TypeError -> Property
prop_add_warning_increases_count warning =
  let collector1 = newErrorCollector
      collector2 = addWarning warning collector1
  in not (hasWarnings collector1) .&&. hasWarnings collector2

-- Property: addInfo doesn't affect error/warning counts
prop_add_info_preserves_counts :: TypeError -> Property
prop_add_info_preserves_counts info =
  let collector1 = newErrorCollector
      collector2 = addInfo info collector1
  in hasErrors collector1 === hasErrors collector2 .&&.
     hasWarnings collector1 === hasWarnings collector2

-- Property: getErrors returns only errors
prop_get_errors_filters_correctly :: [TypeError] -> [TypeError] -> [TypeError] -> Property
prop_get_errors_filters_correctly errors warnings infos =
  let collector = foldl (\c e -> addError e c) newErrorCollector errors
      collector2 = foldl (\c w -> addWarning w c) collector warnings
      collector3 = foldl (\c i -> addInfo i c) collector2 infos
      retrievedErrors = getErrors collector3
  in length retrievedErrors === length errors

-- Property: getWarnings returns only warnings
prop_get_warnings_filters_correctly :: [TypeError] -> [TypeError] -> [TypeError] -> Property
prop_get_warnings_filters_correctly errors warnings infos =
  let collector = foldl (\c e -> addError e c) newErrorCollector errors
      collector2 = foldl (\c w -> addWarning w c) collector warnings
      collector3 = foldl (\c i -> addInfo i c) collector2 infos
      retrievedWarnings = getWarnings collector3
  in length retrievedWarnings === length warnings

-- Property: getInfo returns only info messages
prop_get_info_filters_correctly :: [TypeError] -> [TypeError] -> [TypeError] -> Property
prop_get_info_filters_correctly errors warnings infos =
  let collector = foldl (\c e -> addError e c) newErrorCollector errors
      collector2 = foldl (\c w -> addWarning w c) collector warnings
      collector3 = foldl (\c i -> addInfo i c) collector2 infos
      retrievedInfo = getInfo collector3
  in length retrievedInfo === length infos

-- Property: getAllMessages returns all messages
prop_get_all_messages_returns_all :: [TypeError] -> [TypeError] -> [TypeError] -> Property
prop_get_all_messages_returns_all errors warnings infos =
  let collector = foldl (\c e -> addError e c) newErrorCollector errors
      collector2 = foldl (\c w -> addWarning w c) collector warnings
      collector3 = foldl (\c i -> addInfo i c) collector2 infos
      allMessages = getAllMessages collector3
  in length allMessages === length errors + length warnings + length infos

-- Property: errorAt creates error with location
prop_error_at_has_location :: ErrorLocation -> Text -> Property
prop_error_at_has_location location message =
  let error = errorAt location message
  in errorLocation error === location

-- Property: errorWithCategory creates error with category
prop_error_with_category_has_category :: ErrorCategory -> Text -> Property
prop_error_with_category_has_category category message =
  let error = errorWithCategory category message
  in errorCategory error === category

-- Property: warningAt creates warning with correct severity
prop_warning_at_has_warning_severity :: ErrorLocation -> Text -> Property
prop_warning_at_has_warning_severity location message =
  let warning = warningAt location message
  in errorSeverity warning === Warning

-- Property: warningWithCategory creates warning with category
prop_warning_with_category_has_category :: ErrorCategory -> Text -> Property
prop_warning_with_category_has_category category message =
  let warning = warningWithCategory category message
  in errorCategory warning === category .&&. errorSeverity warning === Warning

-- Property: infoAt creates info with correct severity
prop_info_at_has_info_severity :: ErrorLocation -> Text -> Property
prop_info_at_has_info_severity location message =
  let info = infoAt location message
  in errorSeverity info === ErrorInfo

-- Property: infoWithCategory creates info with category
prop_info_with_category_has_category :: ErrorCategory -> Text -> Property
prop_info_with_category_has_category category message =
  let info = infoWithCategory category message
  in errorCategory info === category .&&. errorSeverity info === ErrorInfo

-- Property: fatalError has FatalError severity
prop_fatal_error_has_fatal_severity :: Text -> Property
prop_fatal_error_has_fatal_severity message =
  let error = fatalError message
  in errorSeverity error === FatalError

-- Property: fatalErrorWithCategory has both fatal severity and category
prop_fatal_error_with_category_has_both :: ErrorCategory -> Text -> Property
prop_fatal_error_with_category_has_both category message =
  let error = fatalErrorWithCategory category message
  in errorSeverity error === FatalError .&&. errorCategory error === category

-- Property: errorWithSuggestions preserves suggestions
prop_error_with_suggestions_preserves :: Text -> [Text] -> Property
prop_error_with_suggestions_preserves message suggestions =
  let error = errorWithSuggestions message suggestions
  in errorSuggestions error === suggestions

-- Property: withLocation updates location
prop_with_location_updates :: TypeError -> ErrorLocation -> Property
prop_with_location_updates error newLocation =
  let updatedError = withLocation newLocation error
  in errorLocation updatedError === newLocation

-- Property: withContext updates context
prop_with_context_updates :: TypeError -> ErrorContext -> Property
prop_with_context_updates error newContext =
  let updatedError = withContext newContext error
  in errorContext updatedError === newContext

-- Property: withSuggestions updates suggestions
prop_with_suggestions_updates :: TypeError -> [Text] -> Property
prop_with_suggestions_updates error newSuggestions =
  let updatedError = withSuggestions newSuggestions error
  in errorSuggestions updatedError === newSuggestions

-- Property: withRelatedErrors updates related errors
prop_with_related_errors_updates :: TypeError -> [TypeError] -> Property
prop_with_related_errors_updates error relatedErrors =
  let updatedError = withRelatedErrors relatedErrors error
  in errorRelatedErrors updatedError === relatedErrors

-- Property: wrapError adds context
prop_wrap_error_adds_context :: TypeError -> Text -> Property
prop_wrap_error_adds_context error contextMessage =
  let wrappedError = wrapError contextMessage error
      originalContext = errorContext error
      wrappedContext = errorContext wrappedError
  in not (T.null (ecAdditionalInfo wrappedContext))

-- Property: combineErrors combines all errors
prop_combine_errors_combines_all :: [TypeError] -> [TypeError] -> Property
prop_combine_errors_combines_all errors1 errors2 =
  let combined = combineErrors errors1 errors2
      expectedLength = length errors1 + length errors2
  in case combined of
       CombinedError errs -> length errs === expectedLength

-- Property: combinedErrorSeverity returns highest severity
prop_combined_error_severity_highest :: [TypeError] -> Property
prop_combined_error_severity_highest errors =
  not (null errors) ==>
  let combined = CombinedError errors
      severity = combinedErrorSeverity combined
      severities = map errorSeverity errors
      highest = maximum severities
  in severity === highest

-- Property: filterCombinedErrorsBySeverity filters correctly
prop_filter_combined_by_severity :: [TypeError] -> ErrorSeverity -> Property
prop_filter_combined_by_severity errors severity =
  let combined = CombinedError errors
      filtered = filterCombinedErrorsBySeverity severity combined
  in case filtered of
       CombinedError filteredErrors -> all (\e -> errorSeverity e <= severity) filteredErrors

-- Property: hasCategory checks category correctly
prop_has_category_checks :: TypeError -> ErrorCategory -> Property
prop_has_category_checks error category =
  let sameCategory = errorCategory error == category
      hasIt = hasCategory category error
  in hasIt === sameCategory

-- Property: filterByCategory filters correctly
prop_filter_by_category_filters :: [TypeError] -> ErrorCategory -> Property
prop_filter_by_category_filters errors category =
  let filtered = filterByCategory category errors
  in all (\e -> errorCategory e == category) filtered

-- Property: filterBySeverity filters correctly
prop_filter_by_severity_filters :: [TypeError] -> ErrorSeverity -> Property
prop_filter_by_severity_filters errors severity =
  let filtered = filterBySeverity severity errors
  in all (\e -> errorSeverity e <= severity) filtered

-- Property: getErrorStatistics returns correct counts
prop_get_error_statistics_correct :: [TypeError] -> [TypeError] -> [TypeError] -> Property
prop_get_error_statistics_correct errors warnings infos =
  let collector = foldl (\c e -> addError e c) newErrorCollector errors
      collector2 = foldl (\c w -> addWarning w c) collector warnings
      collector3 = foldl (\c i -> addInfo i c) collector2 infos
      stats = getErrorStatistics collector3
  in stats.errorCount === length errors .&&.
     stats.warningCount === length warnings .&&.
     stats.infoCount === length infos

-- Property: generateErrorReport includes all messages
prop_generate_error_report_includes_all :: [TypeError] -> [TypeError] -> [TypeError] -> Property
prop_generate_error_report_includes_all errors warnings infos =
  let collector = foldl (\c e -> addError e c) newErrorCollector errors
      collector2 = foldl (\c w -> addWarning w c) collector warnings
      collector3 = foldl (\c i -> addInfo i c) collector2 infos
      report = generateErrorReport collector3
  in not (T.null report)

-- Property: createRecoveryStrategy creates strategy
prop_create_recovery_strategy_creates :: ErrorRecovery -> Property
prop_create_recovery_strategy_creates recovery =
  let strategy = createRecoveryStrategy recovery
  in strategy recovery === recovery

-- Property: customRecovery creates custom recovery
prop_custom_recovery_creates :: Text -> Property
prop_custom_recovery_creates message =
  let recovery = customRecovery message
  in case recovery of
       CustomRecovery msg -> msg === message
       _ -> property False

-- Property: Standard recovery functions create correct recovery types
prop_standard_recovery_functions :: Property
prop_standard_recovery_functions =
  let fatal = fatalRecovery
      errorRec = errorRecovery
      warningRec = warningRecovery
      infoRec = infoRecovery
  in fatal === AbortCompilation .&&.
     errorRec === SkipCurrentBlock .&&.
     warningRec === ContinueProcessing .&&.
     infoRec === ContinueProcessing

-- Property: formatError returns non-empty string
prop_format_error_non_empty :: TypeError -> Property
prop_format_error_non_empty error =
  let formatted = formatError error
  in not (T.null formatted)

-- Property: formatErrors returns non-empty string for non-empty list
prop_format_errors_non_empty :: [TypeError] -> Property
prop_format_errors_non_empty errors =
  not (null errors) ==>
  let formatted = formatErrors errors
  in not (T.null formatted)

-- Property: formatErrorWithLocation includes location info
prop_format_error_with_location_includes_location :: TypeError -> Property
prop_format_error_with_location_includes_location error =
  let formatted = formatErrorWithLocation error
      location = errorLocation error
      locationStr = T.pack $ show (elLine location) ++ ":" ++ show (elColumn location)
  in locationStr `T.isInfixOf` formatted

-- Property: formatErrorsWithLocation handles empty list
prop_format_errors_with_location_empty :: Property
prop_format_errors_with_location_empty =
  let formatted = formatErrorsWithLocation []
  in T.null formatted

-- Property: canRecoverFrom handles different severities
prop_can_recover_from_severity :: ErrorSeverity -> Property
prop_can_recover_from_severity severity =
  let testError = TypeError "test" severity SyntaxError (ErrorLocation "" 1 1) emptyContext [] [] NoRecovery
      canRecover = canRecoverFrom testError
  in case severity of
       FatalError -> not canRecover
       _ -> canRecover

-- Property: shouldContinueAfter handles different severities
prop_should_continue_after_severity :: ErrorSeverity -> Property
prop_should_continue_after_severity severity =
  let testError = TypeError "test" severity SyntaxError (ErrorLocation "" 1 1) emptyContext [] [] NoRecovery
      shouldContinue = shouldContinueAfter testError
  in case severity of
       FatalError -> not shouldContinue
       _ -> shouldContinue

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "ErrorHandler QuickCheck Tests"
  [ fastProperty "emptyContext has empty fields" prop_empty_context_correct
  , fastProperty "newErrorCollector creates empty collector" prop_new_error_collector_empty
  , fastProperty "addError increases error count" prop_add_error_increases_count
  , fastProperty "addWarning increases warning count" prop_add_warning_increases_count
  , fastProperty "addInfo doesn't affect error/warning counts" prop_add_info_preserves_counts
  , fastProperty "getErrors returns only errors" prop_get_errors_filters_correctly
  , fastProperty "getWarnings returns only warnings" prop_get_warnings_filters_correctly
  , fastProperty "getInfo returns only info messages" prop_get_info_filters_correctly
  , fastProperty "getAllMessages returns all messages" prop_get_all_messages_returns_all
  , fastProperty "errorAt creates error with location" prop_error_at_has_location
  , fastProperty "errorWithCategory creates error with category" prop_error_with_category_has_category
  , fastProperty "warningAt creates warning with correct severity" prop_warning_at_has_warning_severity
  , fastProperty "warningWithCategory creates warning with category" prop_warning_with_category_has_category
  , fastProperty "infoAt creates info with correct severity" prop_info_at_has_info_severity
  , fastProperty "infoWithCategory creates info with category" prop_info_with_category_has_category
  , fastProperty "fatalError has FatalError severity" prop_fatal_error_has_fatal_severity
  , fastProperty "fatalErrorWithCategory has both fatal severity and category" prop_fatal_error_with_category_has_both
  , fastProperty "errorWithSuggestions preserves suggestions" prop_error_with_suggestions_preserves
  , fastProperty "withLocation updates location" prop_with_location_updates
  , fastProperty "withContext updates context" prop_with_context_updates
  , fastProperty "withSuggestions updates suggestions" prop_with_suggestions_updates
  , fastProperty "withRelatedErrors updates related errors" prop_with_related_errors_updates
  , fastProperty "wrapError adds context" prop_wrap_error_adds_context
  , fastProperty "combineErrors combines all errors" prop_combine_errors_combines_all
  , fastProperty "combinedErrorSeverity returns highest severity" prop_combined_error_severity_highest
  , fastProperty "filterCombinedErrorsBySeverity filters correctly" prop_filter_combined_by_severity
  , fastProperty "hasCategory checks category correctly" prop_has_category_checks
  , fastProperty "filterByCategory filters correctly" prop_filter_by_category_filters
  , fastProperty "filterBySeverity filters correctly" prop_filter_by_severity_filters
  , fastProperty "getErrorStatistics returns correct counts" prop_get_error_statistics_correct
  , fastProperty "generateErrorReport includes all messages" prop_generate_error_report_includes_all
  , fastProperty "createRecoveryStrategy creates strategy" prop_create_recovery_strategy_creates
  , fastProperty "customRecovery creates custom recovery" prop_custom_recovery_creates
  , fastProperty "Standard recovery functions create correct recovery types" prop_standard_recovery_functions
  , fastProperty "formatError returns non-empty string" prop_format_error_non_empty
  , fastProperty "formatErrors returns non-empty string for non-empty list" prop_format_errors_non_empty
  , fastProperty "formatErrorWithLocation includes location info" prop_format_error_with_location_includes_location
  , fastProperty "formatErrorsWithLocation handles empty list" prop_format_errors_with_location_empty
  , fastProperty "canRecoverFrom handles different severities" prop_can_recover_from_severity
  , fastProperty "shouldContinueAfter handles different severities" prop_should_continue_after_severity
  ]