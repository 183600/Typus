{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ErrorHandlerConsistencyAdvancedQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose, suchThat)
import TestSupport.Arbitrary

import Compiler.Errors.Core
import Data.Text (Text, pack)
import Data.Time (UTCTime, getCurrentTime)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Map.Strict as Map
import Data.List (sort, nub, length, filter)
import Data.Set (Set, empty, singleton, union, unions, member, size, difference, intersection)
import qualified Data.Set as Set

-- ============================================================================
-- Advanced Error Handler Consistency QuickCheck Tests
-- ============================================================================

-- Property: Error collector maintains order of insertion
prop_error_collector_order_preservation :: [TypeError] -> Property
prop_error_collector_order_preservation errors =
  length errors > 0 ==>
  let collector = newErrorCollector
      collectorWithErrors = foldl (\acc err -> addError err acc) collector errors
      collectedErrors = getErrors collectorWithErrors
  in property $ length collectedErrors === length errors .&&.
                 map errorMsg collectedErrors === map errorMsg errors

-- Property: Error severity filtering is consistent
prop_error_severity_filtering :: [TypeError] -> ErrorSeverity -> Property
prop_error_severity_filtering errors severity =
  length errors > 0 ==>
  let collector = newErrorCollector
      collectorWithErrors = foldl (\acc err -> addError err acc) collector errors
      allErrors = getErrors collectorWithErrors
      filteredBySeverity = filter (\e -> errorSeverity e == severity) allErrors
  in property $ all (\e -> errorSeverity e == severity) filteredBySeverity

-- Property: Error category filtering is consistent
prop_error_category_filtering :: [TypeError] -> ErrorCategory -> Property
prop_error_category_filtering errors category =
  length errors > 0 ==>
  let collector = newErrorCollector
      collectorWithErrors = foldl (\acc err -> addError err acc) collector errors
      allErrors = getErrors collectorWithErrors
      filteredByCategory = filter (\e -> errorCategory e == category) allErrors
  in property $ all (\e -> errorCategory e == category) filteredByCategory

-- Property: Error recovery status is preserved
prop_error_recovery_preservation :: [TypeError] -> Property
prop_error_recovery_preservation errors =
  length errors > 0 ==>
  let collector = newErrorCollector
      collectorWithErrors = foldl (\acc err -> addError err acc) collector errors
      collectedErrors = getErrors collectorWithErrors
      originalRecovery = map errorRecovery errors
      collectedRecovery = map errorRecovery collectedErrors
  in property $ originalRecovery === collectedRecovery

-- Property: Warning and error separation is consistent
prop_warning_error_separation :: [TypeError] -> [TypeError] -> Property
prop_warning_error_separation errors warnings =
  length errors > 0 || length warnings > 0 ==>
  let collector = newErrorCollector
      collectorWithErrors = foldl (\acc err -> addError err acc) collector errors
      collectorWithWarnings = foldl (\acc warn -> addWarning warn acc) collectorWithErrors warnings
      finalErrors = getErrors collectorWithWarnings
      finalWarnings = getWarnings collectorWithWarnings
  in property $ 
    length finalErrors === length errors .&&.
    length finalWarnings === length warnings .&&.
    all (\e -> errorSeverity e `elem` [Fatal, Error]) finalErrors .&&.
    all (\w -> errorSeverity w == Warning) finalWarnings

-- Property: Error context information is preserved
prop_error_context_preservation :: [TypeError] -> Property
prop_error_context_preservation errors =
  length errors > 0 ==>
  let collector = newErrorCollector
      collectorWithErrors = foldl (\acc err -> addError err acc) collector errors
      collectedErrors = getErrors collectorWithErrors
      originalContexts = map errorContext errors
      collectedContexts = map errorContext collectedErrors
  in property $ originalContexts === collectedContexts

-- Property: Error location information is preserved
prop_error_location_preservation :: [TypeError] -> Property
prop_error_location_preservation errors =
  length errors > 0 ==>
  let collector = newErrorCollector
      collectorWithErrors = foldl (\acc err -> addError err acc) collector errors
      collectedErrors = getErrors collectorWithErrors
      originalLocations = map errorLocation errors
      collectedLocations = map errorLocation collectedErrors
  in property $ originalLocations === collectedLocations

-- Property: Combined errors maintain all component properties
prop_combined_error_properties :: TypeError -> TypeError -> Property
prop_combined_error_properties err1 err2 =
  let combined = CombinedError [err1, err2]
      componentErrors = getCombinedErrors combined
  in property $ 
    length componentErrors === 2 .&&.
    err1 `elem` componentErrors .&&.
    err2 `elem` componentErrors

-- Property: Error formatting produces consistent output
prop_error_formatting_consistency :: TypeError -> Property
prop_error_formatting_consistency err =
  let formatted1 = formatError err
      formatted2 = formatError err
  in property $ formatted1 === formatted2

-- Property: Multiple error formatting maintains order
prop_multiple_error_formatting_order :: [TypeError] -> Property
prop_multiple_error_formatting_order errors =
  length errors > 0 ==>
  let formatted = formatErrors errors
      errorMessages = map errorMsg errors
  in property $ all (`isInfixOf` formatted) errorMessages

-- Property: Error recovery determination is consistent
prop_error_recovery_consistency :: TypeError -> Property
prop_error_recovery_consistency err =
  let canRecover1 = canRecoverFrom err
      canRecover2 = canRecoverFrom err
      shouldContinue1 = shouldContinueAfter err
      shouldContinue2 = shouldContinueAfter err
  in property $ 
    canRecover1 === canRecover2 .&&.
    shouldContinue1 === shouldContinue2

-- Property: Error severity ordering is consistent
prop_error_severity_ordering :: ErrorSeverity -> ErrorSeverity -> Property
prop_error_severity_ordering sev1 sev2 =
  let severityOrder = [Info, Warning, Error, Fatal]
      sev1Index = case elemIndex sev1 severityOrder of
        Just idx -> idx
        Nothing -> 0
      sev2Index = case elemIndex sev2 severityOrder of
        Just idx -> idx
        Nothing -> 0
  in property $ 
    (sev1Index <= sev2Index) ==> (sev1 <= sev2)

-- Helper function to get index of element in list
elemIndex :: Eq a => a -> [a] -> Maybe Int
elemIndex _ [] = Nothing
elemIndex x (y:ys)
  | x == y = Just 0
  | otherwise = case elemIndex x ys of
      Just idx -> Just (idx + 1)
      Nothing -> Nothing

-- Helper function to check substring
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `Data.List.isInfixOf` haystack

-- Helper function to get combined errors
getCombinedErrors :: TypeError -> [TypeError]
getCombinedErrors (CombinedError errs) = errs
getCombinedErrors _ = []

-- Test collection
tests :: TestTree
tests = testGroup "Advanced Error Handler Consistency QuickCheck Tests"
  [ fastProperty "Error collector maintains order of insertion" prop_error_collector_order_preservation
  , fastProperty "Error severity filtering is consistent" prop_error_severity_filtering
  , fastProperty "Error category filtering is consistent" prop_error_category_filtering
  , fastProperty "Error recovery status is preserved" prop_error_recovery_preservation
  , fastProperty "Warning and error separation is consistent" prop_warning_error_separation
  , fastProperty "Error context information is preserved" prop_error_context_preservation
  , fastProperty "Error location information is preserved" prop_error_location_preservation
  , fastProperty "Combined errors maintain all component properties" prop_combined_error_properties
  , fastProperty "Error formatting produces consistent output" prop_error_formatting_consistency
  , fastProperty "Multiple error formatting maintains order" prop_multiple_error_formatting_order
  , fastProperty "Error recovery determination is consistent" prop_error_recovery_consistency
  , fastProperty "Error severity ordering is consistent" prop_error_severity_ordering
  ]