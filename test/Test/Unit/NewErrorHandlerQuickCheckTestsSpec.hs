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
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose)
import Test.QuickCheck.Gen (Gen(..), vectorOf)

import Compiler.Errors.Core
  ( TypeError(..)
  , ErrorSeverity(..)
  , ErrorCategory(..)
  , ErrorLocation(..)
  , ErrorContext(..)
  , ErrorRecovery(..)
  , CombinedError(..)
  , emptyContext
  , severityPriority
  , isAtLeast
  , compareSeverity
  , canRecoverFrom
  , shouldContinueAfter
  , errorAt
  , warningAt
  , infoAt
  , fatalError
  , filterBySeverity
  , filterByCategory
  , hasErrors
  , hasWarnings
  , formatError
  , combinedErrorSeverity
  , filterCombinedErrorsBySeverity
  , getErrorLine
  , getErrorColumn
  , _unknownLocation
  , fatalRecovery
  , errorRecovery
  , warningRecovery
  , infoRecovery
  )

import SourceLocation (SourcePos(..), startPos)

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sort, nub)
import Data.Maybe (isJust, isNothing)

-- ============================================================================
-- Arbitrary instances
-- ============================================================================

instance Arbitrary ErrorSeverity where
  arbitrary = elements [Fatal, Error, Warning, Info]

instance Arbitrary ErrorCategory where
  arbitrary = elements [TypeChecking, Ownership, Parsing, Semantic, Runtime, Constraint, Inference, Integration, Unknown]

instance Arbitrary ErrorLocation where
  arbitrary = do
    line <- choose (1, 1000)
    column <- choose (1, 1000)
    endLine <- choose (1, 1000)
    endColumn <- choose (1, 1000)
    filePath <- oneof [return Nothing, Just <$> listOf (elements ['a'..'z'])]
    return $ ErrorLocation filePath line column (Just endLine) (Just endColumn)

instance Arbitrary ErrorContext where
  arbitrary = do
    code <- oneof [return Nothing, Just <$> listOf (elements ['a'..'z'])]
    func <- oneof [return Nothing, Just <$> listOf (elements ['a'..'z'])]
    var <- oneof [return Nothing, Just <$> listOf (elements ['a'..'z'])]
    typ <- oneof [return Nothing, Just <$> listOf (elements ['a'..'z'])]
    additional <- listOf ((,) <$> listOf (elements ['a'..'z']) <*> listOf (elements ['a'..'z']))
    return $ ErrorContext code func var typ additional

instance Arbitrary ErrorRecovery where
  arbitrary = do
    canRec <- arbitrary
    shouldCont <- arbitrary
    action <- oneof [return Nothing, Just <$> listOf (elements ['a'..'z'])]
    hint <- oneof [return Nothing, Just <$> listOf (elements ['a'..'z'])]
    cost <- choose (0, 100)
    confidence <- choose (0.0, 1.0)
    return $ RecoveryStrategy canRec shouldCont action hint cost confidence

instance Arbitrary TypeError where
  arbitrary = do
    errorId <- listOf (elements ['a'..'z'])
    severity <- arbitrary
    category <- arbitrary
    message <- T.pack <$> listOf (elements ['a'..'z'])
    location <- arbitrary
    context <- arbitrary
    recovery <- arbitrary
    suggestions <- listOf (T.pack <$> listOf (elements ['a'..'z']))
    relatedErrors <- listOf arbitrary
    errorChain <- listOf arbitrary
    timestamp <- oneof [return Nothing, Just <$> listOf (elements ['0'..'9'])]
    return $ TypeError errorId severity category message location context recovery suggestions relatedErrors errorChain timestamp

instance Arbitrary CombinedError where
  arbitrary = oneof
    [ OwnershipErrorCombined <$> arbitrary <*> arbitrary
    , DependentTypeErrorCombined <$> arbitrary <*> arbitrary
    , IntegrationError <$> listOf (elements ['a'..'z']) <*> arbitrary
    , CrossAnalyzerError <$> listOf (elements ['a'..'z']) <*> arbitrary <*> listOf arbitrary
    ]

-- Generate valid error ID
validErrorId :: Gen String
validErrorId = listOf $ elements ['a'..'z'] ++ ['0'..'9'] ++ ['_']

-- Generate valid error message
validErrorMessage :: Gen Text
validErrorMessage = T.pack <$> listOf (elements ['a'..'z'] ++ [' '])

-- ============================================================================
-- Error Severity Property Tests
-- ============================================================================

-- Property: severity priority ordering
prop_severity_priority_ordering :: ErrorSeverity -> ErrorSeverity -> Property
prop_severity_priority_ordering sev1 sev2 =
  let priority1 = severityPriority sev1
      priority2 = severityPriority sev2
      ordering = compareSeverity sev1 sev2
  in property $ (priority1 > priority2) === (ordering == GT) .&&.
               (priority1 == priority2) === (ordering == EQ) .&&.
               (priority1 < priority2) === (ordering == LT)

-- Property: severity priority values are correct
prop_severity_priority_values :: Property
prop_severity_priority_values =
  property $ severityPriority Fatal === 100 .&&.
               severityPriority Error === 80 .&&.
               severityPriority Warning === 30 .&&.
               severityPriority Info === 10

-- Property: isAtLeast is reflexive
prop_isAtLeast_reflexive :: ErrorSeverity -> Property
prop_isAtLeast_reflexive sev =
  isAtLeast sev sev === True

-- Property: isAtLeast is transitive
prop_isAtLeast_transitive :: ErrorSeverity -> ErrorSeverity -> ErrorSeverity -> Property
prop_isAtLeast_transitive sev1 sev2 sev3 =
  isAtLeast sev1 sev2 && isAtLeast sev2 sev3 ==> isAtLeast sev1 sev3

-- Property: Fatal is at least as severe as any other severity
prop_fatal_is_most_severe :: ErrorSeverity -> Property
prop_fatal_is_most_severe sev =
  isAtLeast Fatal sev === True

-- Property: Info is not at least as severe as any other severity except itself
prop_info_is_least_severe :: ErrorSeverity -> Property
prop_info_is_least_severe sev =
  (sev == Info) === (isAtLeast Info sev)

-- ============================================================================
-- Error Location Property Tests
-- ============================================================================

-- Property: getErrorLine returns line number
prop_getErrorLine_correct :: ErrorLocation -> Property
prop_getErrorLine_correct loc =
  getErrorLine loc === line loc

-- Property: getErrorColumn returns column number
prop_getErrorColumn_correct :: ErrorLocation -> Property
prop_getErrorColumn_correct loc =
  getErrorColumn loc === column loc

-- Property: unknown location has zero values
prop_unknown_location_values :: Property
prop_unknown_location_values =
  property $ filePath _unknownLocation === Nothing .&&.
               getErrorLine _unknownLocation === 0 .&&.
               getErrorColumn _unknownLocation === 0 .&&.
               endLine _unknownLocation === Nothing .&&.
               endColumn _unknownLocation === Nothing

-- ============================================================================
-- Error Recovery Property Tests
-- ============================================================================

-- Property: fatal recovery cannot recover
prop_fatal_recovery_cannot_recover :: Property
prop_fatal_recovery_cannot_recover =
  property $ canRecover fatalRecovery === False .&&.
               shouldContinue fatalRecovery === False

-- Property: error recovery can recover
prop_error_recovery_can_recover :: Property
prop_error_recovery_can_recover =
  property $ canRecover errorRecovery === True .&&.
               shouldContinue errorRecovery === True

-- Property: warning recovery can recover
prop_warning_recovery_can_recover :: Property
prop_warning_recovery_can_recover =
  property $ canRecover warningRecovery === True .&&.
               shouldContinue warningRecovery === True

-- Property: info recovery can recover
prop_info_recovery_can_recover :: Property
prop_info_recovery_can_recover =
  property $ canRecover infoRecovery === True .&&.
               shouldContinue infoRecovery === True

-- ============================================================================
-- Error Creation Property Tests
-- ============================================================================

-- Property: errorAt creates error with correct severity
prop_errorAt_severity :: Text -> ErrorLocation -> Property
prop_errorAt_severity msg loc =
  let err = errorAt msg loc
  in severity err === Error

-- Property: warningAt creates warning with correct severity
prop_warningAt_severity :: Text -> ErrorLocation -> Property
prop_warningAt_severity msg loc =
  let err = warningAt msg loc
  in severity err === Warning

-- Property: infoAt creates info with correct severity
prop_infoAt_severity :: Text -> ErrorLocation -> Property
prop_infoAt_severity msg loc =
  let err = infoAt msg loc
  in severity err === Info

-- Property: fatalError creates fatal error with correct severity
prop_fatalError_severity :: Text -> ErrorLocation -> Property
prop_fatalError_severity msg loc =
  let err = fatalError msg loc
  in severity err === Fatal

-- ============================================================================
-- Error Filtering Property Tests
-- ============================================================================

-- Property: filterBySeverity preserves order of matching elements
prop_filterBySeverity_preserves_order :: [TypeError] -> ErrorSeverity -> Property
prop_filterBySeverity_preserves_order errors minSeverity =
  let filtered = filterBySeverity minSeverity errors
      matching = filter (\e -> isAtLeast (severity e) minSeverity) errors
  in property $ filtered === matching

-- Property: filterBySeverity with Fatal returns only fatal errors
prop_filterBySeverity_fatal :: [TypeError] -> Property
prop_filterBySeverity_fatal errors =
  let filtered = filterBySeverity Fatal errors
  in all (\e -> severity e == Fatal) filtered

-- Property: filterBySeverity with Info returns all errors
prop_filterBySeverity_info :: [TypeError] -> Property
prop_filterBySeverity_info errors =
  let filtered = filterBySeverity Info errors
  in property $ filtered === errors

-- Property: filterByCategory preserves order of matching elements
prop_filterByCategory_preserves_order :: [TypeError] -> ErrorCategory -> Property
prop_filterByCategory_preserves_order errors cat =
  let filtered = filterByCategory cat errors
      matching = filter (\e -> category e == cat) errors
  in property $ filtered === matching

-- Property: filterByCategory only returns errors with specified category
prop_filterByCategory_correct :: [TypeError] -> ErrorCategory -> Property
prop_filterByCategory_correct errors cat =
  let filtered = filterByCategory cat errors
  in all (\e -> category e == cat) filtered

-- ============================================================================
-- Error Collection Property Tests
-- ============================================================================

-- Property: hasErrors is true if there are errors
prop_hasErrors_with_errors :: [TypeError] -> Property
prop_hasErrors_with_errors errors =
  let hasErrorOrFatal = any (\e -> severity e == Error || severity e == Fatal) errors
  in hasErrors errors === hasErrorOrFatal

-- Property: hasWarnings is true if there are warnings
prop_hasWarnings_with_warnings :: [TypeError] -> Property
prop_hasWarnings_with_warnings errors =
  let hasWarning = any (\e -> severity e == Warning) errors
  in hasWarnings errors === hasWarning

-- Property: hasErrors is false for only info messages
prop_hasErrors_false_for_info :: [TypeError] -> Property
prop_hasErrors_false_for_info errors =
  all (\e -> severity e == Info) errors ==> hasErrors errors === False

-- Property: hasWarnings is false for only errors and fatals
prop_hasWarnings_false_for_errors :: [TypeError] -> Property
prop_hasWarnings_false_for_errors errors =
  all (\e -> severity e == Error || severity e == Fatal) errors ==> hasWarnings errors === False

-- ============================================================================
-- Combined Error Property Tests
-- ============================================================================

-- Property: combinedErrorSeverity returns correct severity
prop_combinedErrorSeverity_correct :: CombinedError -> Property
prop_combinedErrorSeverity_correct combinedErr =
  let expected = case combinedErr of
        OwnershipErrorCombined sev _ -> sev
        DependentTypeErrorCombined sev _ -> sev
        IntegrationError _ sev -> sev
        CrossAnalyzerError _ sev _ -> sev
  in combinedErrorSeverity combinedErr === expected

-- Property: filterCombinedErrorsBySeverity preserves order
prop_filterCombinedErrorsBySeverity_preserves_order :: [CombinedError] -> ErrorSeverity -> Property
prop_filterCombinedErrorsBySeverity_preserves_order errors minSeverity =
  let filtered = filterCombinedErrorsBySeverity minSeverity errors
      matching = filter (\e -> isAtLeast (combinedErrorSeverity e) minSeverity) errors
  in property $ filtered === matching

-- ============================================================================
-- Error Formatting Property Tests
-- ============================================================================

-- Property: formatError returns non-empty string
prop_formatError_non_empty :: TypeError -> Property
prop_formatError_non_empty err =
  let formatted = formatError err
  in property $ not (null formatted)

-- Property: formatError includes error message
prop_formatError_includes_message :: TypeError -> Property
prop_formatError_includes_message err =
  let formatted = formatError err
      msgStr = T.unpack (message err)
  in property $ msgStr `isInfixOf` formatted

-- ============================================================================
-- Advanced Property Tests
-- ============================================================================

-- Property: empty context has all Nothing values
prop_empty_context_nothing :: Property
prop_empty_context_nothing =
  property $ contextCode emptyContext === Nothing .&&.
               contextFunction emptyContext === Nothing .&&.
               contextVariable emptyContext === Nothing .&&.
               contextType emptyContext === Nothing .&&.
               null (contextAdditional emptyContext)

-- Property: error recovery cost is within bounds
prop_recovery_cost_bounds :: ErrorRecovery -> Property
prop_recovery_cost_bounds recovery =
  let cost = recoveryCost recovery
  in property $ cost >= 0 && cost <= 100

-- Property: error recovery confidence is within bounds
prop_recovery_confidence_bounds :: ErrorRecovery -> Property
prop_recovery_confidence_bounds recovery =
  let confidence = recoveryConfidence recovery
  in property $ confidence >= 0.0 && confidence <= 1.0

-- Property: error ID is preserved
prop_error_id_preserved :: String -> Text -> ErrorLocation -> Property
prop_error_id_preserved errorId msg loc =
  let err = errorAt msg loc
      errWithId = err { errorId = errorId }
  in errorId errWithId === errorId

-- Property: error suggestions are preserved
prop_error_suggestions_preserved :: [Text] -> Text -> ErrorLocation -> Property
prop_error_suggestions_preserved suggestions msg loc =
  let err = errorAt msg loc
      errWithSuggestions = err { suggestions = suggestions }
  in suggestions errWithSuggestions === suggestions

-- Property: error context is preserved
prop_error_context_preserved :: ErrorContext -> Text -> ErrorLocation -> Property
prop_error_context_preserved ctx msg loc =
  let err = errorAt msg loc
      errWithContext = err { context = ctx }
  in context errWithContext === ctx

-- Property: error location is preserved
prop_error_location_preserved :: Text -> ErrorLocation -> Property
prop_error_location_preserved msg loc =
  let err = errorAt msg loc
  in location err === loc

-- Property: error category is preserved
prop_error_category_preserved :: ErrorCategory -> Text -> ErrorLocation -> Property
prop_error_category_preserved cat msg loc =
  let err = errorAt msg loc
      errWithCategory = err { category = cat }
  in category errWithCategory === cat

-- Property: canRecoverFrom matches recovery strategy
prop_canRecoverFrom_matches :: TypeError -> Property
prop_canRecoverFrom_matches err =
  canRecoverFrom err === canRecover (recovery err)

-- Property: shouldContinueAfter matches recovery strategy
prop_shouldContinueAfter_matches :: TypeError -> Property
prop_shouldContinueAfter_matches err =
  shouldContinueAfter err === shouldContinue (recovery err)

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New ErrorHandler QuickCheck Tests"
  [ fastProperty "severity priority ordering" prop_severity_priority_ordering
  , fastProperty "severity priority values are correct" prop_severity_priority_values
  , fastProperty "isAtLeast is reflexive" prop_isAtLeast_reflexive
  , fastProperty "isAtLeast is transitive" prop_isAtLeast_transitive
  , fastProperty "Fatal is at least as severe as any other severity" prop_fatal_is_most_severe
  , fastProperty "Info is not at least as severe as any other severity except itself" prop_info_is_least_severe
  , fastProperty "getErrorLine returns line number" prop_getErrorLine_correct
  , fastProperty "getErrorColumn returns column number" prop_getErrorColumn_correct
  , fastProperty "unknown location has zero values" prop_unknown_location_values
  , fastProperty "fatal recovery cannot recover" prop_fatal_recovery_cannot_recover
  , fastProperty "error recovery can recover" prop_error_recovery_can_recover
  , fastProperty "warning recovery can recover" prop_warning_recovery_can_recover
  , fastProperty "info recovery can recover" prop_info_recovery_can_recover
  , fastProperty "errorAt creates error with correct severity" prop_errorAt_severity
  , fastProperty "warningAt creates warning with correct severity" prop_warningAt_severity
  , fastProperty "infoAt creates info with correct severity" prop_infoAt_severity
  , fastProperty "fatalError creates fatal error with correct severity" prop_fatalError_severity
  , fastProperty "filterBySeverity preserves order of matching elements" prop_filterBySeverity_preserves_order
  , fastProperty "filterBySeverity with Fatal returns only fatal errors" prop_filterBySeverity_fatal
  , fastProperty "filterBySeverity with Info returns all errors" prop_filterBySeverity_info
  , fastProperty "filterByCategory preserves order of matching elements" prop_filterByCategory_preserves_order
  , fastProperty "filterByCategory only returns errors with specified category" prop_filterByCategory_correct
  , fastProperty "hasErrors is true if there are errors" prop_hasErrors_with_errors
  , fastProperty "hasWarnings is true if there are warnings" prop_hasWarnings_with_warnings
  , fastProperty "hasErrors is false for only info messages" prop_hasErrors_false_for_info
  , fastProperty "hasWarnings is false for only errors and fatals" prop_hasWarnings_false_for_errors
  , fastProperty "combinedErrorSeverity returns correct severity" prop_combinedErrorSeverity_correct
  , fastProperty "filterCombinedErrorsBySeverity preserves order" prop_filterCombinedErrorsBySeverity_preserves_order
  , fastProperty "formatError returns non-empty string" prop_formatError_non_empty
  , fastProperty "formatError includes error message" prop_formatError_includes_message
  , fastProperty "empty context has all Nothing values" prop_empty_context_nothing
  , fastProperty "error recovery cost is within bounds" prop_recovery_cost_bounds
  , fastProperty "error recovery confidence is within bounds" prop_recovery_confidence_bounds
  , fastProperty "error ID is preserved" prop_error_id_preserved
  , fastProperty "error suggestions are preserved" prop_error_suggestions_preserved
  , fastProperty "error context is preserved" prop_error_context_preserved
  , fastProperty "error location is preserved" prop_error_location_preserved
  , fastProperty "error category is preserved" prop_error_category_preserved
  , fastProperty "canRecoverFrom matches recovery strategy" prop_canRecoverFrom_matches
  , fastProperty "shouldContinueAfter matches recovery strategy" prop_shouldContinueAfter_matches
  ]