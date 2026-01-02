{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ErrorRecoveryQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import Compiler.Errors.Core
  ( TypeError(..)
  , ErrorSeverity(..)
  , ErrorCategory(..)
  , ErrorLocation(..)
  , ErrorContext(..)
  , ErrorRecovery(..)
  , emptyContext
  , canRecoverFrom
  , shouldContinueAfter
  , customRecovery
  )
import qualified Data.Text as T
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.List (sort)
import Data.Char (isSpace, isDigit)

-- Property: Error recovery strategies are consistent
prop_recovery_strategy_consistency :: Bool -> Bool -> Property
prop_recovery_strategy_consistency canRec shouldCont =
  let strategy = createRecoveryStrategy canRec shouldCont Nothing Nothing
      error = errorAt "TEST" (T.pack "test") (ErrorLocation Nothing 1 1 Nothing Nothing) { recovery = strategy }
  in property (canRecoverFrom error === canRec && shouldContinueAfter error === shouldCont)

-- Property: Fatal errors cannot be recovered from
prop_fatal_error_no_recovery :: String -> Property
prop_fatal_error_no_recovery errorMsg =
  not (null errorMsg) ==>
  let location = ErrorLocation Nothing 1 1 Nothing Nothing
      fatalError = errorAt "test-id" (T.pack errorMsg) location { severity = Fatal }
  in property (not (canRecoverFrom fatalError) && not (shouldContinueAfter fatalError))

-- Property: Warning errors can be recovered from
prop_warning_error_can_recover :: String -> Property
prop_warning_error_can_recover warningMsg =
  not (null warningMsg) ==>
  let location = ErrorLocation Nothing 1 1 Nothing Nothing
      warningError = warningAt "test-id" (T.pack warningMsg) location
  in property (canRecoverFrom warningError && shouldContinueAfter warningError)

-- Property: Info errors can be recovered from
prop_info_error_can_recover :: String -> Property
prop_info_error_can_recover infoMsg =
  not (null infoMsg) ==>
  let location = ErrorLocation Nothing 1 1 Nothing Nothing
      infoError = infoAt "test-id" (T.pack infoMsg) location
  in property (canRecoverFrom infoError && shouldContinueAfter infoError)

-- Property: Error filtering by severity works correctly
prop_filter_by_severity :: [ErrorSeverity] -> ErrorSeverity -> Property
prop_filter_by_severity severities targetSeverity =
  not (null severities) ==>
  let errors = L.map (\sev -> errorAt "TEST" (T.pack "test") (ErrorLocation Nothing 1 1 Nothing Nothing) { severity = sev }) severities
      filtered = filterBySeverity targetSeverity errors
  in property (L.length filtered === L.length (L.filter (\e -> severity e == targetSeverity) errors))

-- Property: Error filtering by category works correctly
prop_filter_by_category :: [ErrorCategory] -> ErrorCategory -> Property
prop_filter_by_category categories targetCategory =
  not (null categories) ==>
  let errors = L.map (\cat -> errorWithCategory "TEST" cat (T.pack "test") (ErrorLocation Nothing 1 1 Nothing Nothing)) categories
      filtered = filterByCategory targetCategory errors
  in property (L.length filtered === L.length (L.filter (\e -> category e == targetCategory) errors))

-- Property: Error statistics are accurate
prop_error_statistics_accuracy :: [ErrorSeverity] -> [ErrorCategory] -> Property
prop_error_statistics_accuracy severities categories =
  not (null severities) && not (null categories) && L.length severities == L.length categories ==>
  let errors = zipWith (\sev cat -> errorAt "TEST" (T.pack "test") (ErrorLocation Nothing 1 1 Nothing Nothing) { severity = sev, category = cat }) severities categories
      stats = getErrorStatistics errors
  in property (stats "total" === L.length errors)

-- Property: Error formatting includes essential information
prop_error_formatting_essentials :: String -> ErrorSeverity -> ErrorCategory -> Property
prop_error_formatting_essentials errorMsg sev cat =
  not (null errorMsg) ==>
  let error = errorAt "test-id" (T.pack errorMsg) (ErrorLocation Nothing 1 1 Nothing Nothing) { severity = sev, category = cat }
      formatted = formatError error
  in property (errorMsg `L.isInfixOf` formatted && show sev `L.isInfixOf` formatted && show cat `L.isInfixOf` formatted)

-- Property: Error formatting with location includes location info
prop_error_formatting_with_location :: String -> Int -> Int -> Property
prop_error_formatting_with_location errorMsg line col =
  not (null errorMsg) && line > 0 && col > 0 ==>
  let location = ErrorLocation Nothing line col Nothing Nothing
      error = errorAt "test-id" (T.pack errorMsg) location
      formatted = formatErrorWithLocation error
  in property (show line `L.isInfixOf` formatted && show col `L.isInfixOf` formatted && errorMsg `L.isInfixOf` formatted)

-- Property: Error suggestions are preserved in formatting
prop_error_suggestions_preserved :: String -> [String] -> Property
prop_error_suggestions_preserved errorMsg suggestions =
  not (null errorMsg) && not (null suggestions) && L.all (not . null) suggestions ==>
  let error = errorWithSuggestions "TEST" (T.pack errorMsg) (map T.pack suggestions) (ErrorLocation Nothing 1 1 Nothing Nothing)
      formatted = formatError error
  in property (L.all (`L.isInfixOf` formatted) suggestions)

-- Property: Custom recovery strategies work as expected
prop_custom_recovery_strategy :: Bool -> Bool -> String -> String -> Int -> Float -> Property
prop_custom_recovery_strategy canRec shouldCont action hint cost confidence =
  cost >= 0 && cost <= 100 && confidence >= 0.0 && confidence <= 1.0 ==>
  let strategy = customRecovery canRec shouldCont (Just action) (Just hint) cost confidence
      error = errorAt "TEST" (T.pack "test") (ErrorLocation Nothing 1 1 Nothing Nothing) { recovery = strategy }
  in property (canRecoverFrom error === canRec && shouldContinueAfter error === shouldCont)

-- Property: Recovery strategy selection based on severity
prop_recovery_by_severity :: ErrorSeverity -> Property
prop_recovery_by_severity sev =
  let error = errorAt "TEST" (T.pack "test") (ErrorLocation Nothing 1 1 Nothing Nothing) { severity = sev }
      canRec = canRecoverFrom error
      shouldCont = shouldContinueAfter error
  in case sev of
    Fatal -> property (not canRec && not shouldCont)
    Error -> property (canRec && shouldCont)
    Warning -> property (canRec && shouldCont)
    Info -> property (canRec && shouldCont)

tests :: TestTree
tests = testGroup "Error Recovery QuickCheck tests"
  [ fastProperty "Error recovery strategies are consistent" prop_recovery_strategy_consistency
  , fastProperty "Fatal errors cannot be recovered from" prop_fatal_error_no_recovery
  , fastProperty "Warning errors can be recovered from" prop_warning_error_can_recover
  , fastProperty "Info errors can be recovered from" prop_info_error_can_recover
  , fastProperty "Error filtering by severity works correctly" prop_filter_by_severity
  , fastProperty "Error filtering by category works correctly" prop_filter_by_category
  , fastProperty "Error statistics are accurate" prop_error_statistics_accuracy
  , fastProperty "Error formatting includes essential information" prop_error_formatting_essentials
  , fastProperty "Error formatting with location includes location info" prop_error_formatting_with_location
  , fastProperty "Error suggestions are preserved in formatting" prop_error_suggestions_preserved
  , fastProperty "Custom recovery strategies work as expected" prop_custom_recovery_strategy
  , fastProperty "Recovery strategy selection based on severity" prop_recovery_by_severity
  ]