{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ErrorHandlerNewQuickCheckTests (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, choose, listOf, suchThat)

import Compiler.Errors.Core
  ( TypeError(..)
  , CombinedError(..)
  , ErrorSeverity(..)
  , ErrorCategory(..)
  , ErrorLocation(..)
  , ErrorContext(..)
  , ErrorRecovery(..)
  , emptyContext
  , severityPriority
  , compareSeverity
  , isAtLeast
  , canRecoverFrom
  , shouldContinueAfter
  , filterBySeverity
  , filterByCategory
  , hasErrors
  , hasWarnings
  , formatError
  , errorAt
  , warningAt
  , infoAt
  , fatalError
  , errorWithCategory
  , warningWithCategory
  , infoWithCategory
  , errorWithSuggestions
  , withLocation
  , withContext
  , withSuggestions
  , withRelatedErrors
  , wrapError
  , combineErrors
  , combinedErrorSeverity
  , filterCombinedErrorsBySeverity
  , getErrorLine
  , getErrorColumn
  , fatalRecovery
  , errorRecovery
  , warningRecovery
  , infoRecovery
  , customRecovery
  , _unknownLocation
  , _atLocation
  , _atFileLocation
  , _atRange
  )

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sort, nub)
import Data.Maybe (isJust, isNothing)
import Data.Char (isAlphaNum)

-- ============================================================================
-- Arbitrary Instances for ErrorHandler Types
-- ============================================================================

instance Arbitrary ErrorSeverity where
  arbitrary = elements [Fatal, Error, Warning, Info]

instance Arbitrary ErrorCategory where
  arbitrary = elements [TypeChecking, Ownership, Parsing, Semantic, Runtime, Constraint, Inference, Integration, Unknown]

instance Arbitrary ErrorLocation where
  arbitrary = do
    filePath <- oneof [return Nothing, fmap Just arbitrary]
    line <- choose (1, 1000)
    column <- choose (1, 1000)
    endLine <- oneof [return Nothing, fmap Just (choose (line, line + 100))]
    endColumn <- oneof [return Nothing, fmap Just (choose (column, column + 100))]
    return $ ErrorLocation filePath line column endLine endColumn

instance Arbitrary ErrorContext where
  arbitrary = do
    code <- oneof [return Nothing, fmap Just arbitrary]
    function <- oneof [return Nothing, fmap Just arbitrary]
    variable <- oneof [return Nothing, fmap Just arbitrary]
    typ <- oneof [return Nothing, fmap Just arbitrary]
    additional <- listOf ((,) <$> arbitrary <*> arbitrary)
    return $ ErrorContext code function variable typ additional

instance Arbitrary ErrorRecovery where
  arbitrary = do
    canRec <- arbitrary
    shouldCont <- arbitrary
    action <- oneof [return Nothing, fmap Just arbitrary]
    hint <- oneof [return Nothing, fmap Just arbitrary]
    cost <- choose (0, 100)
    confidence <- choose (0.0, 1.0)
    return $ RecoveryStrategy canRec shouldCont action hint cost confidence

instance Arbitrary TypeError where
  arbitrary = do
    errorId <- arbitrary `suchThat` (not . null)
    severity <- arbitrary
    category <- arbitrary
    message <- T.pack <$> arbitrary
    location <- arbitrary
    context <- arbitrary
    recovery <- arbitrary
    suggestions <- listOf (T.pack <$> arbitrary)
    relatedErrors <- listOf arbitrary
    errorChain <- listOf arbitrary
    timestamp <- oneof [return Nothing, fmap Just arbitrary]
    return $ TypeError errorId severity category message location context recovery suggestions relatedErrors errorChain timestamp

instance Arbitrary CombinedError where
  arbitrary = oneof
    [ OwnershipErrorCombined <$> arbitrary <*> arbitrary
    , DependentTypeErrorCombined <$> arbitrary <*> arbitrary
    , IntegrationError <$> arbitrary <*> arbitrary
    , CrossAnalyzerError <$> arbitrary <*> arbitrary <*> listOf arbitrary
    ]

-- ============================================================================
-- ErrorSeverity Properties
-- ============================================================================

-- Property: severityPriority ordering is consistent
prop_severity_priority_ordering :: Property
prop_severity_priority_ordering =
  property $ severityPriority Fatal > severityPriority Error .&&.
             severityPriority Error > severityPriority Warning .&&.
             severityPriority Warning > severityPriority Info

-- Property: compareSeverity is consistent with severityPriority
prop_compareSeverity_consistent :: ErrorSeverity -> ErrorSeverity -> Property
prop_compareSeverity_consistent sev1 sev2 =
  let cmp = compareSeverity sev1 sev2
      pri1 = severityPriority sev1
      pri2 = severityPriority sev2
  in property $ (cmp == EQ) === (pri1 == pri2) .&&.
             (cmp == LT) === (pri1 < pri2) .&&.
             (cmp == GT) === (pri1 > pri2)

-- Property: isAtLeast transitivity
prop_isAtLeast_transitive :: ErrorSeverity -> ErrorSeverity -> ErrorSeverity -> Property
prop_isAtLeast_transitive minSev midSev maxSev =
  isAtLeast minSev midSev && isAtLeast midSev maxSev ==> isAtLeast minSev maxSev

-- Property: isAtLeast reflexivity
prop_isAtLeast_reflexive :: ErrorSeverity -> Property
prop_isAtLeast_reflexive sev = isAtLeast sev sev

-- ============================================================================
-- ErrorLocation Properties
-- ============================================================================

-- Property: _atLocation creates correct location
prop_atLocation_correct :: Int -> Int -> Property
prop_atLocation_correct line col =
  line > 0 && col > 0 ==>
  let loc = _atLocation line col
  in property $ filePath loc === Nothing .&&.
             getErrorLine loc === line .&&.
             getErrorColumn loc === col .&&.
             endLine loc === Nothing .&&.
             endColumn loc === Nothing

-- Property: _atFileLocation creates correct location
prop_atFileLocation_correct :: String -> Int -> Int -> Property
prop_atFileLocation_correct file line col =
  line > 0 && col > 0 && not (null file) ==>
  let loc = _atFileLocation file line col
  in property $ filePath loc === Just file .&&.
             getErrorLine loc === line .&&.
             getErrorColumn loc === col .&&.
             endLine loc === Nothing .&&.
             endColumn loc === Nothing

-- Property: _atRange creates correct location
prop_atRange_correct :: Int -> Int -> Int -> Int -> Property
prop_atRange_correct startLine startCol endLine endCol =
  startLine > 0 && startCol > 0 && endLine >= startLine && 
  (endLine > startLine ==> endCol > startCol) ==>
  let loc = _atRange startLine startCol endLine endCol
  in property $ filePath loc === Nothing .&&.
             getErrorLine loc === startLine .&&.
             getErrorColumn loc === startCol .&&.
             endLine loc === Just endLine .&&.
             endColumn loc === Just endCol

-- ============================================================================
-- ErrorRecovery Properties
-- ============================================================================

-- Property: fatalRecovery has correct properties
prop_fatal_recovery_properties :: Property
prop_fatal_recovery_properties =
  property $ not (canRecover fatalRecovery) .&&.
             not (shouldContinue fatalRecovery) .&&.
             recoveryCost fatalRecovery === 100 .&&.
             recoveryConfidence fatalRecovery === 0.0

-- Property: errorRecovery has correct properties
prop_error_recovery_properties :: Property
prop_error_recovery_properties =
  property $ canRecover errorRecovery .&&.
             shouldContinue errorRecovery .&&.
             recoveryCost errorRecovery === 50 .&&.
             recoveryConfidence errorRecovery === 0.7

-- Property: warningRecovery has correct properties
prop_warning_recovery_properties :: Property
prop_warning_recovery_properties =
  property $ canRecover warningRecovery .&&.
             shouldContinue warningRecovery .&&.
             recoveryCost warningRecovery === 10 .&&.
             recoveryConfidence warningRecovery === 0.9

-- Property: infoRecovery has correct properties
prop_info_recovery_properties :: Property
prop_info_recovery_properties =
  property $ canRecover infoRecovery .&&.
             shouldContinue infoRecovery .&&.
             recoveryCost infoRecovery === 0 .&&.
             recoveryConfidence infoRecovery === 1.0

-- Property: customRecovery creates correct recovery
prop_custom_recovery_correct :: Bool -> Bool -> Maybe String -> Maybe String -> Int -> Property
prop_custom_recovery_correct canRec shouldCont action hint cost =
  cost >= 0 && cost <= 100 ==>
  let recovery = customRecovery canRec shouldCont action hint cost 0.5
  in property $ canRecover recovery === canRec .&&.
             shouldContinue recovery === shouldCont .&&.
             recoveryAction recovery === action .&&.
             recoveryHint recovery === hint .&&.
             recoveryCost recovery === cost .&&.
             recoveryConfidence recovery === 0.5

-- ============================================================================
-- Error Creation Properties
-- ============================================================================

-- Property: errorAt creates error with correct severity
prop_errorat_correct_severity :: String -> Int -> Int -> Property
prop_errorat_correct_severity msg line col =
  line > 0 && col > 0 && not (null msg) ==>
  let err = errorAt msg line col
  in property $ severity err === Error .&&.
             T.unpack (message err) === msg .&&.
             getErrorLine (location err) === line .&&.
             getErrorColumn (location err) === col

-- Property: warningAt creates warning with correct severity
prop_warningat_correct_severity :: String -> Int -> Int -> Property
prop_warningat_correct_severity msg line col =
  line > 0 && col > 0 && not (null msg) ==>
  let err = warningAt msg line col
  in property $ severity err === Warning .&&.
             T.unpack (message err) === msg .&&.
             getErrorLine (location err) === line .&&.
             getErrorColumn (location err) === col

-- Property: infoAt creates info with correct severity
prop_infoat_correct_severity :: String -> Int -> Int -> Property
prop_infoat_correct_severity msg line col =
  line > 0 && col > 0 && not (null msg) ==>
  let err = infoAt msg line col
  in property $ severity err === Info .&&.
             T.unpack (message err) === msg .&&.
             getErrorLine (location err) === line .&&.
             getErrorColumn (location err) === col

-- Property: fatalError creates fatal error with correct severity
prop_fatal_error_correct_severity :: String -> Property
prop_fatal_error_correct_severity msg =
  not (null msg) ==>
  let err = fatalError msg
  in property $ severity err === Fatal .&&.
             T.unpack (message err) === msg .&&.
             location err === _unknownLocation

-- Property: errorWithCategory creates error with correct category
prop_error_with_category_correct :: String -> ErrorCategory -> Property
prop_error_with_category_correct msg cat =
  not (null msg) ==>
  let err = errorWithCategory msg cat
  in property $ severity err === Error .&&.
             category err === cat .&&.
             T.unpack (message err) === msg .&&.
             location err === _unknownLocation

-- ============================================================================
-- Error Modification Properties
-- ============================================================================

-- Property: withLocation changes location correctly
prop_with_location_changes_location :: TypeError -> Int -> Int -> Property
prop_with_location_changes_location err line col =
  line > 0 && col > 0 ==>
  let newErr = withLocation line col err
      newLoc = location newErr
  in property $ getErrorLine newLoc === line .&&.
             getErrorColumn newLoc === col .&&.
             message newErr === message err .&&.
             severity newErr === severity err .&&.
             category newErr === category err

-- Property: withContext changes context correctly
prop_with_context_changes_context :: TypeError -> ErrorContext -> Property
prop_with_context_changes_context err ctx =
  let newErr = withContext ctx err
  in property $ context newErr === ctx .&&.
             message newErr === message err .&&.
             severity newErr === severity err .&&.
             category newErr === category err .&&.
             location newErr === location err

-- Property: withSuggestions adds suggestions correctly
prop_with_suggestions_adds_suggestions :: TypeError -> [String] -> Property
prop_with_suggestions_adds_suggestions err suggs =
  let newErr = withSuggestions (map T.pack suggs) err
  in property $ suggestions newErr === map T.pack suggs .&&.
             message newErr === message err .&&.
             severity newErr === severity err .&&.
             category newErr === category err .&&.
             location newErr === location err

-- Property: wrapError creates error chain correctly
prop_wrap_error_creates_chain :: TypeError -> TypeError -> Property
prop_wrap_error_creates_chain wrapper inner =
  let wrapped = wrapError wrapper inner
  in property $ errorChain wrapped === [inner] .&&.
             message wrapped === message wrapper .&&.
             severity wrapped === severity wrapper .&&.
             category wrapped === category wrapper .&&.
             location wrapped === location wrapper

-- ============================================================================
-- Error Collection Properties
-- ============================================================================

-- Property: filterBySeverity works correctly
prop_filter_by_severity_correct :: [TypeError] -> ErrorSeverity -> Property
prop_filter_by_severity_correct errors minSev =
  let filtered = filterBySeverity minSev errors
  in property $ all (\e -> isAtLeast minSev (severity e)) filtered

-- Property: filterByCategory works correctly
prop_filter_by_category_correct :: [TypeError] -> ErrorCategory -> Property
prop_filter_by_category_correct errors cat =
  let filtered = filterByCategory cat errors
  in property $ all (\e -> category e == cat) filtered

-- Property: hasErrors detects errors correctly
prop_has_errors_detects_errors :: [TypeError] -> Property
prop_has_errors_detects_errors errors =
  let hasErrs = hasErrors errors
      actualErrors = filter (\e -> severity e == Error || severity e == Fatal) errors
  in property $ hasErrs === not (null actualErrors)

-- Property: hasWarnings detects warnings correctly
prop_has_warnings_detects_warnings :: [TypeError] -> Property
prop_has_warnings_detects_warnings errors =
  let hasWarns = hasWarnings errors
      actualWarnings = filter (\e -> severity e == Warning) errors
  in property $ hasWarns === not (null actualWarnings)

-- ============================================================================
-- CombinedError Properties
-- ============================================================================

-- Property: combinedErrorSeverity extracts severity correctly
prop_combined_error_severity_correct :: CombinedError -> Property
prop_combined_error_severity_correct combinedErr =
  let extractedSev = combinedErrorSeverity combinedErr
      expectedSev = case combinedErr of
        OwnershipErrorCombined sev _ -> sev
        DependentTypeErrorCombined sev _ -> sev
        IntegrationError _ sev -> sev
        CrossAnalyzerError _ sev _ -> sev
  in property $ extractedSev === expectedSev

-- Property: filterCombinedErrorsBySeverity works correctly
prop_filter_combined_by_severity_correct :: [CombinedError] -> ErrorSeverity -> Property
prop_filter_combined_by_severity_correct combinedErrors minSev =
  let filtered = filterCombinedErrorsBySeverity minSev combinedErrors
  in property $ all (\e -> isAtLeast minSev (combinedErrorSeverity e)) filtered

-- ============================================================================
-- Error Formatting Properties
-- ============================================================================

-- Property: formatError includes severity information
prop_format_error_includes_severity :: TypeError -> Property
prop_format_error_includes_severity err =
  let formatted = formatError err
      severityStr = case severity err of
        Fatal -> "FATAL"
        Error -> "ERROR"
        Warning -> "WARNING"
        Info -> "INFO"
  in property $ severityStr `isInfixOf` formatted

-- Property: formatError includes message
prop_format_error_includes_message :: TypeError -> Property
prop_format_error_includes_message err =
  let formatted = formatError err
      msg = T.unpack (message err)
  in property $ msg `isInfixOf` formatted

-- Property: formatError includes category
prop_format_error_includes_category :: TypeError -> Property
prop_format_error_includes_category err =
  let formatted = formatError err
      catStr = "[" ++ show (category err) ++ "]"
  in property $ catStr `isInfixOf` formatted

-- ============================================================================
-- Complex Interaction Properties
-- ============================================================================

-- Property: Error wrapping chain preserves order
prop_error_wrapping_chain_order :: [TypeError] -> Property
prop_error_wrapping_chain_order errors =
  not (null errors) ==>
  let wrapped = foldl wrapError (head errors) (tail errors)
      chain = errorChain wrapped
  in property $ length chain === length errors - 1

-- Property: Multiple modifications compose correctly
prop_multiple_modifications_compose :: TypeError -> ErrorContext -> [String] -> Int -> Int -> Property
prop_multiple_modifications_compose err ctx suggs line col =
  line > 0 && col > 0 ==>
  let modified = withLocation line col $ withSuggestions (map T.pack suggs) $ withContext ctx err
  in property $ context modified === ctx .&&.
             suggestions modified === map T.pack suggs .&&.
             getErrorLine (location modified) === line .&&.
             getErrorColumn (location modified) === col .&&.
             message modified === message err .&&.
             severity modified === severity err

-- Property: Error filtering preserves ordering
prop_error_filtering_preserves_ordering :: [TypeError] -> ErrorSeverity -> Property
prop_error_filtering_preserves_ordering errors minSev =
  let filtered = filterBySeverity minSev errors
      originalOrdering = map severity $ filter (\e -> isAtLeast minSev (severity e)) errors
      filteredOrdering = map severity filtered
  in property $ originalOrdering === filteredOrdering

-- Property: Complex error recovery scenarios
prop_complex_recovery_scenarios :: ErrorSeverity -> Bool -> Bool -> Int -> Float -> Property
prop_complex_recovery_scenarios sev canRec shouldCont cost confidence =
  cost >= 0 && cost <= 100 && confidence >= 0.0 && confidence <= 1.0 ==>
  let recovery = customRecovery canRec shouldCont Nothing Nothing cost confidence
      err = errorAt "test error" 1 1
      errWithRecovery = err { recovery = recovery }
  in property $ canRecoverFrom errWithRecovery === canRec .&&.
             shouldContinueAfter errWithRecovery === shouldCont .&&.
             recoveryCost (recovery errWithRecovery) === cost .&&.
             recoveryConfidence (recovery errWithRecovery) === confidence

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "ErrorHandler New QuickCheck Tests"
  [ testGroup "ErrorSeverity Properties"
    [ fastProperty "severityPriority ordering is consistent" prop_severity_priority_ordering
    , fastProperty "compareSeverity is consistent with severityPriority" prop_compareSeverity_consistent
    , fastProperty "isAtLeast transitivity" prop_isAtLeast_transitive
    , fastProperty "isAtLeast reflexivity" prop_isAtLeast_reflexive
    ]

  , testGroup "ErrorLocation Properties"
    [ fastProperty "_atLocation creates correct location" prop_atLocation_correct
    , fastProperty "_atFileLocation creates correct location" prop_atFileLocation_correct
    , fastProperty "_atRange creates correct location" prop_atRange_correct
    ]

  , testGroup "ErrorRecovery Properties"
    [ fastProperty "fatalRecovery has correct properties" prop_fatal_recovery_properties
    , fastProperty "errorRecovery has correct properties" prop_error_recovery_properties
    , fastProperty "warningRecovery has correct properties" prop_warning_recovery_properties
    , fastProperty "infoRecovery has correct properties" prop_info_recovery_properties
    , fastProperty "customRecovery creates correct recovery" prop_custom_recovery_correct
    ]

  , testGroup "Error Creation Properties"
    [ fastProperty "errorAt creates error with correct severity" prop_errorat_correct_severity
    , fastProperty "warningAt creates warning with correct severity" prop_warningat_correct_severity
    , fastProperty "infoAt creates info with correct severity" prop_infoat_correct_severity
    , fastProperty "fatalError creates fatal error with correct severity" prop_fatal_error_correct_severity
    , fastProperty "errorWithCategory creates error with correct category" prop_error_with_category_correct
    ]

  , testGroup "Error Modification Properties"
    [ fastProperty "withLocation changes location correctly" prop_with_location_changes_location
    , fastProperty "withContext changes context correctly" prop_with_context_changes_context
    , fastProperty "withSuggestions adds suggestions correctly" prop_with_suggestions_adds_suggestions
    , fastProperty "wrapError creates error chain correctly" prop_wrap_error_creates_chain
    ]

  , testGroup "Error Collection Properties"
    [ fastProperty "filterBySeverity works correctly" prop_filter_by_severity_correct
    , fastProperty "filterByCategory works correctly" prop_filter_by_category_correct
    , fastProperty "hasErrors detects errors correctly" prop_has_errors_detects_errors
    , fastProperty "hasWarnings detects warnings correctly" prop_has_warnings_detects_warnings
    ]

  , testGroup "CombinedError Properties"
    [ fastProperty "combinedErrorSeverity extracts severity correctly" prop_combined_error_severity_correct
    , fastProperty "filterCombinedErrorsBySeverity works correctly" prop_filter_combined_by_severity_correct
    ]

  , testGroup "Error Formatting Properties"
    [ fastProperty "formatError includes severity information" prop_format_error_includes_severity
    , fastProperty "formatError includes message" prop_format_error_includes_message
    , fastProperty "formatError includes category" prop_format_error_includes_category
    ]

  , testGroup "Complex Interaction Properties"
    [ fastProperty "Error wrapping chain preserves order" prop_error_wrapping_chain_order
    , fastProperty "Multiple modifications compose correctly" prop_multiple_modifications_compose
    , fastProperty "Error filtering preserves ordering" prop_error_filtering_preserves_ordering
    , fastProperty "Complex error recovery scenarios" prop_complex_recovery_scenarios
    ]
  ]