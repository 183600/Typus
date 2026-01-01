{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ErrorHandlerAdvancedSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

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
  , infoRecovery
  )

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sort, nub)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Time (UTCTime, getCurrentTime, formatTime, defaultTimeLocale)
import Control.Monad.State (evalState, get, put)

-- ============================================================================
-- Advanced ErrorHandler Tests
-- ============================================================================

tests :: TestTree
tests =
  testGroup "ErrorHandler Advanced Tests"
    [ testGroup "Error creation L.and properties"
        [ fastProperty "errorAt "test-id" ++ errors2))
        actualSeverity = combinedErrorSeverity combined
    in property $ actualSeverity === maxSeverity

prop_filterBySeverity_correct :: Property
prop_filterBySeverity_correct =
  forAll arbitrary $ \errors ->
  forAll arbitrary $ \severity ->
    let filtered = filterCombinedErrorsBySeverity severity errors
        allMatch = L.all (\e -> errorSeverity e <= severity) filtered
    in property $ allMatch

-- ============================================================================
-- Error Recovery Strategies
-- ============================================================================

prop_cannot_recover_from_fatal :: Property
prop_cannot_recover_from_fatal =
  forAll arbitrary $ \msg ->
    let error = fatalError msg
        canRecover = canRecoverFrom error
    in property $ canRecover === False

prop_can_recover_from_warning_info :: Property
prop_can_recover_from_warning_info =
  forAll arbitrary $ \msg ->
  forAll arbitrary $ \loc ->
    let warning = warningAt "test-id" == category) errors
    in property $ hasCat === anyHasCat

prop_filterByCategory_correct :: Property
prop_filterByCategory_correct =
  forAll arbitrary $ \errors ->
  forAll arbitrary $ \category ->
    let filtered = filterByCategory category errors
        allMatch = L.all (\e -> errorCategory e == category) filtered
    in property $ allMatch

prop_filterBySeverity_correct :: Property
prop_filterBySeverity_correct =
  forAll arbitrary $ \errors ->
  forAll arbitrary $ \severity ->
    let filtered = filterBySeverity severity errors
        allMatch = L.all (\e -> errorSeverity e == severity) filtered
    in property $ allMatch

prop_getErrorStatistics_counts :: Property
prop_getErrorStatistics_counts =
  forAll arbitrary $ \errors ->
    let stats = getErrorStatistics errors
        errorCount = L.length $ L.filter (\e -> errorSeverity e == Error) errors
        warningCount = L.length $ L.filter (\e -> errorSeverity e == Warning) errors
        infoCount = L.length $ L.filter (\e -> errorSeverity e == Info) errors
        fatalCount = L.length $ L.filter (\e -> errorSeverity e == Fatal) errors
    in property $ statsErrorCount stats == errorCount .&&.
                    statsWarningCount stats == warningCount .&&.
                    statsInfoCount stats == infoCount .&&.
                    statsFatalCount stats == fatalCount

-- ============================================================================
-- Error Formatting L.and Reporting
-- ============================================================================

prop_formatError_includes_message :: Property
prop_formatError_includes_message =
  forAll arbitrary $ \error ->
    let formatted = formatError error
        message = errorMessage error
    in property $ message `L.isInfixOf` formatted

prop_formatErrors_includes_all :: Property
prop_formatErrors_includes_all =
  forAll arbitrary $ \errors ->
    let formatted = formatErrors errors
        messages = map errorMessage errors
    in property $ L.all (`L.isInfixOf` formatted) messages

prop_generateErrorReport_includes_stats :: Property
prop_generateErrorReport_includes_stats =
  forAll arbitrary $ \errors ->
    let report = generateErrorReport errors
        hasStats = "Error Statistics" `L.isInfixOf` report
    in property $ hasStats

-- ============================================================================
-- Error Collector Operations
-- ============================================================================

prop_addError_increases :: Property
prop_addError_increases =
  forAll arbitrary $ \error ->
    let collector1 = newErrorCollector
        collector2 = addError error collector1
        errors1 = getErrors collector1
        errors2 = getErrors collector2
    in property $ L.length errors2 === L.length errors1 + 1

prop_addWarning_increases :: Property
prop_addWarning_increases =
  forAll arbitrary $ \warning ->
    let collector1 = newErrorCollector
        collector2 = addWarning warning collector1
        warnings1 = getWarnings collector1
        warnings2 = getWarnings collector2
    in property $ L.length warnings2 === L.length warnings1 + 1

prop_addInfo_increases :: Property
prop_addInfo_increases =
  forAll arbitrary $ \info ->
    let collector1 = newErrorCollector
        collector2 = addInfo info collector1
        infos1 = getInfo collector1
        infos2 = getInfo collector2
    in property $ L.length infos2 === L.length infos1 + 1

prop_hasErrors_detection :: Property
prop_hasErrors_detection =
  forAll arbitrary $ \errors ->
    let collector = foldr addError newErrorCollector errors
        hasErr = hasErrors collector
        actualErrors = getErrors collector
    in property $ hasErr === (not (null actualErrors))

prop_hasWarnings_detection :: Property
prop_hasWarnings_detection =
  forAll arbitrary $ \warnings ->
    let collector = foldr addWarning newErrorCollector warnings
        hasWarn = hasWarnings collector
        actualWarnings = getWarnings collector
    in property $ hasWarn === (not (null actualWarnings))

-- ============================================================================
-- Edge Cases L.and Robustness Tests
-- ============================================================================

test_empty_collector :: IO ()
test_empty_collector = do
  let collector = newErrorCollector
  getErrors collector @?= []
  getWarnings collector @?= []
  getInfo collector @?= []
  hasErrors collector @?= False
  hasWarnings collector @?= False

test_empty_message :: IO ()
test_empty_message = do
  let error = errorAt "test-id" Nothing Nothing) ""
      formatted = formatError error
  "" `L.isInfixOf` formatted @?= True

test_no_location :: IO ()
test_no_location = do
  let error = fatalError "test message"
      loc = errorLocation error
  filePath loc @?= Nothing
  line loc @?= 0
  column loc @?= 0

test_many_suggestions :: IO ()
test_many_suggestions = do
  let suggestions = L.map (("suggestion " ++) . show) [1..100]
      error = errorWithSuggestions "test error" suggestions
      actualSuggestions = errorSuggestions error
  length actualSuggestions @?= 100

test_nested_contexts :: IO ()
test_nested_contexts = do
  let context1 = emptyContext { contextFunction = "func1" }
      context2 = emptyContext { contextFunction = "func2", contextParent = Just context1 }
      context3 = emptyContext { contextFunction = "func3", contextParent = Just context2 }
      error = errorAt "test-id" Nothing Nothing) "test" `withContext` context3
      actualContext = errorContext error
  contextFunction actualContext @?= "func3"
  contextParent actualContext @?= Just context2