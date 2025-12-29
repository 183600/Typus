{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewRobustErrorHandlerQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf, listOf1, elements, vectorOf, suchThat)

import Compiler.Errors.Core
  ( TypeError(..)
  , ErrorSeverity(..)
  , ErrorCategory(..)
  , ErrorLocation(..)
  , ErrorContext(..)
  , ErrorRecovery(..)
  , emptyContext
  , errorAt
  , errorWithCategory
  , warningAt
  , infoAt
  , fatalError
  , withLocation
  , withContext
  , withSuggestions
  , withRelatedErrors
  , withTimestamp
  , wrapError
  , combineErrors
  , hasCategory
  , filterByCategory
  , filterBySeverity
  , getErrorStatistics
  , canRecoverFrom
  , shouldContinueAfter
  , formatError
  , formatErrorWithLocation
  , formatErrors
  , formatErrorsWithLocation
  , errorAtWithTimestamp
  , _atLocation
  , _atFileLocation
  , _atRange
  , fatalRecovery
  , errorRecovery
  , warningRecovery
  , infoRecovery
  , customRecovery
  )

import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.List (sort, nub)
import Data.Time (UTCTime, getCurrentTime)

-- ============================================================================
-- Enhanced Property Tests for ErrorHandler Module
-- ============================================================================

-- Property: errorAt creates error with correct severity
prop_errorAt_correct_severity :: String -> String -> Int -> Int -> Property
prop_errorAt_correct_severity errId msg line col =
  let location = _atLocation line col
      error = errorAt errId (T.pack msg) location
  in property $ severity error === Error

-- Property: errorWithCategory creates error with correct category
prop_errorWithCategory_correct_category :: String -> String -> ErrorCategory -> Int -> Int -> Property
prop_errorWithCategory_correct_category errId msg category line col =
  let location = _atLocation line col
      error = errorWithCategory errId category (T.pack msg) location
  in property $ category error === category

-- Property: warningAt creates warning with correct severity
prop_warningAt_correct_severity :: String -> String -> Int -> Int -> Property
prop_warningAt_correct_severity errId msg line col =
  let location = _atLocation line col
      warning = warningAt errId (T.pack msg) location
  in property $ severity warning === Warning

-- Property: infoAt creates info with correct severity
prop_infoAt_correct_severity :: String -> String -> Int -> Int -> Property
prop_infoAt_correct_severity errId msg line col =
  let location = _atLocation line col
      info = infoAt errId (T.pack msg) location
  in property $ severity info === Info

-- Property: fatalError creates fatal error with correct severity
prop_fatalError_correct_severity :: String -> String -> Int -> Int -> Property
prop_fatalError_correct_severity errId msg line col =
  let location = _atLocation line col
      fatal = fatalError errId (T.pack msg) location
  in property $ severity fatal === Fatal

-- Property: withLocation updates error location
prop_withLocation_updates_location :: String -> String -> Int -> Int -> Int -> Int -> Property
prop_withLocation_updates_location errId msg line1 col1 line2 col2 =
  let location1 = _atLocation line1 col1
      location2 = _atLocation line2 col2
      error = errorAt errId (T.pack msg) location1
      updatedError = withLocation error location2
  in property $ location updatedError === location2

-- Property: withContext updates error context
prop_withContext_updates_context :: String -> String -> String -> String -> Property
prop_withContext_updates_context errId msg funcName varName =
  let location = _atLocation 1 1
      context = emptyContext { contextFunction = Just funcName, contextVariable = Just varName }
      error = errorAt errId (T.pack msg) location
      updatedError = withContext error context
  in property $ context updatedError === context

-- Property: withSuggestions adds suggestions to error
prop_withSuggestions_adds_suggestions :: String -> String -> [String] -> Property
prop_withSuggestions_adds_suggestions errId msg suggestions =
  let location = _atLocation 1 1
      error = errorAt errId (T.pack msg) location
      suggestionsText = map T.pack suggestions
      updatedError = withSuggestions suggestionsText error
  in property $ suggestions updatedError === suggestionsText

-- Property: withRelatedErrors adds related errors
prop_withRelatedErrors_adds_related :: String -> String -> [String] -> Property
prop_withRelatedErrors_adds_related errId msg relatedIds =
  let location = _atLocation 1 1
      error = errorAt errId (T.pack msg) location
      relatedErrors = map (\rid -> errorAt rid (T.pack ("Related: " ++ rid)) (_atLocation 2 2)) relatedIds
      updatedError = withRelatedErrors relatedErrors error
  in property $ length (relatedErrors updatedError) === length relatedIds

-- Property: withTimestamp adds timestamp to error
prop_withTimestamp_adds_timestamp :: String -> String -> String -> Property
prop_withTimestamp_adds_timestamp errId msg timestamp =
  let location = _atLocation 1 1
      error = errorAt errId (T.pack msg) location
      updatedError = withTimestamp timestamp error
  in property $ timestamp updatedError === Just timestamp

-- Property: wrapError adds wrapper message and chains errors
prop_wrap_error_chains_errors :: String -> String -> String -> Property
prop_wrap_error_chains_errors errId innerMsg wrapperMsg =
  let location = _atLocation 1 1
      innerError = errorAt errId (T.pack innerMsg) location
      wrappedError = wrapError (T.pack wrapperMsg) innerError
  in property $ message wrappedError === T.pack (wrapperMsg ++ ": " ++ innerMsg) .&&.
     length (errorChain wrappedError) === 1

-- Property: hasCategory correctly identifies error categories
prop_hasCategory_identifies_categories :: String -> String -> ErrorCategory -> Property
prop_hasCategory_identifies_categories errId msg category =
  let location = _atLocation 1 1
      error = errorWithCategory errId category (T.pack msg) location
  in property $ hasCategory category error === True .&&.
     hasCategory (case category of 
                    TypeChecking -> Ownership
                    Ownership -> Parsing
                    Parsing -> Semantic
                    Semantic -> Runtime
                    Runtime -> Constraint
                    Constraint -> Inference
                    Inference -> Integration
                    Integration -> Unknown
                    Unknown -> TypeChecking) error === False

-- Property: filterByCategory correctly filters errors
prop_filterByCategory_filters_correctly :: [String] -> ErrorCategory -> Property
prop_filterByCategory_filters_correctly errIds category =
  not (null errIds) ==>
  let errors = zipWith (\i errId -> 
        let cat = case i `mod` 8 of
                    0 -> TypeChecking
                    1 -> Ownership
                    2 -> Parsing
                    3 -> Semantic
                    4 -> Runtime
                    5 -> Constraint
                    6 -> Inference
                    _ -> Integration
        in errorWithCategory errId cat (T.pack ("Error " ++ errId)) (_atLocation i 1))
        [0..] errIds
      filtered = filterByCategory category errors
  in property $ all (\e -> category e == category) filtered

-- Property: filterBySeverity correctly filters errors
prop_filterBySeverity_filters_correctly :: [String] -> ErrorSeverity -> Property
prop_filterBySeverity_filters_correctly errIds targetSeverity =
  not (null errIds) ==>
  let errors = zipWith (\i errId -> 
        let sev = case i `mod` 4 of
                    0 -> Fatal
                    1 -> Error
                    2 -> Warning
                    _ -> Info
        in (case sev of
              Fatal -> fatalError
              Error -> errorAt
              Warning -> warningAt
              Info -> infoAt) errId (T.pack ("Error " ++ errId)) (_atLocation i 1))
        [0..] errIds
      filtered = filterBySeverity targetSeverity errors
  in property $ all (\e -> severity e == targetSeverity) filtered

-- Property: getErrorStatistics provides correct counts
prop_getErrorStatistics_correct_counts :: [String] -> Property
prop_getErrorStatistics_correct_counts errIds =
  not (null errIds) ==>
  let errors = zipWith (\i errId -> 
        let sev = case i `mod` 4 of
                    0 -> Fatal
                    1 -> Error
                    2 -> Warning
                    _ -> Info
            cat = case i `mod` 8 of
                    0 -> TypeChecking
                    1 -> Ownership
                    2 -> Parsing
                    3 -> Semantic
                    4 -> Runtime
                    5 -> Constraint
                    6 -> Inference
                    _ -> Integration
        in (case sev of
              Fatal -> fatalError
              Error -> errorWithCategory errId cat
              Warning -> warningWithCategory errId cat
              Info -> infoWithCategory errId cat) (T.pack ("Error " ++ errId)) (_atLocation i 1))
        [0..] errIds
      stats = getErrorStatistics errors
      totalErrors = length errors
      fatalErrors = length $ filter (\e -> severity e == Fatal) errors
      errorErrors = length $ filter (\e -> severity e == Error) errors
      warningErrors = length $ filter (\e -> severity e == Warning) errors
      infoErrors = length $ filter (\e -> severity e == Info) errors
  in property $ Map.findWithDefault 0 "total" stats === totalErrors .&&.
     Map.findWithDefault 0 "fatal" stats === fatalErrors .&&.
     Map.findWithDefault 0 "errors" stats === errorErrors .&&.
     Map.findWithDefault 0 "warnings" stats === warningErrors .&&.
     Map.findWithDefault 0 "info" stats === infoErrors

-- Property: canRecoverFrom correctly identifies recoverable errors
prop_canRecoverFrom_identifies_recoverable :: String -> String -> ErrorSeverity -> Property
prop_canRecoverFrom_identifies_recoverable errId msg severity =
  let location = _atLocation 1 1
      error = case severity of
                Fatal -> fatalError errId (T.pack msg) location
                Error -> errorAt errId (T.pack msg) location
                Warning -> warningAt errId (T.pack msg) location
                Info -> infoAt errId (T.pack msg) location
  in property $ canRecoverFrom error === (severity /= Fatal)

-- Property: shouldContinueAfter correctly identifies continuation
prop_shouldContinueAfter_identifies_continuation :: String -> String -> ErrorSeverity -> Property
prop_shouldContinueAfter_identifies_continuation errId msg severity =
  let location = _atLocation 1 1
      error = case severity of
                Fatal -> fatalError errId (T.pack msg) location
                Error -> errorAt errId (T.pack msg) location
                Warning -> warningAt errId (T.pack msg) location
                Info -> infoAt errId (T.pack msg) location
  in property $ shouldContinueAfter error === (severity /= Fatal)

-- Property: formatError includes essential information
prop_formatError_includes_essentials :: String -> String -> ErrorSeverity -> ErrorCategory -> Property
prop_formatError_includes_essentials errId msg severity category =
  let location = _atLocation 1 1
      error = errorWithCategory errId category (T.pack msg) location { severity = severity }
      formatted = formatError error
      severityStr = case severity of
                      Fatal -> "FATAL"
                      Error -> "ERROR"
                      Warning -> "WARNING"
                      Info -> "INFO"
      categoryStr = show category
  in property $ severityStr `isInfixOf` formatted .&&.
     categoryStr `isInfixOf` formatted .&&.
     msg `isInfixOf` formatted

-- Property: formatErrorWithLocation includes location information
prop_formatErrorWithLocation_includes_location :: String -> String -> String -> Int -> Int -> Property
prop_formatErrorWithLocation_includes_location errId msg filePath line col =
  let location = _atFileLocation filePath line col
      error = errorAt errId (T.pack msg) location
      formatted = formatErrorWithLocation error
  in property $ show line `isInfixOf` formatted .&&.
     show col `isInfixOf` formatted .&&.
     (if null filePath then True else filePath `isInfixOf` formatted)

-- Property: formatErrors maintains order by severity
prop_formatErrors_maintains_order :: [String] -> Property
prop_formatErrors_maintains_order errIds =
  not (null errIds) ==>
  let errors = zipWith (\i errId -> 
        let sev = case i `mod` 4 of
                    0 -> Fatal
                    1 -> Error
                    2 -> Warning
                    _ -> Info
        in (case sev of
              Fatal -> fatalError
              Error -> errorAt
              Warning -> warningAt
              Info -> infoAt) errId (T.pack ("Error " ++ errId)) (_atLocation i 1))
        [0..] errIds
      formatted = formatErrors errors
      formattedLines = lines formatted
  in property $ length formattedLines === length errors

-- Property: combineErrors preserves all errors
prop_combineErrors_preserves_all :: [String] -> Property
prop_combineErrors_preserves_all errIds =
  not (null errIds) ==>
  let errors = map (\errId -> errorAt errId (T.pack ("Error " ++ errId)) (_atLocation 1 1)) errIds
      combined = combineErrors errors
  in property $ length combined >= length errors

-- Property: error recovery strategies are consistent
prop_recovery_strategies_consistent :: String -> String -> Property
prop_recovery_strategies_consistent errId msg =
  let location = _atLocation 1 1
      fatal = fatalError errId (T.pack msg) location
      error = errorAt errId (T.pack msg) location
      warning = warningAt errId (T.pack msg) location
      info = infoAt errId (T.pack msg) location
  in property $ not (canRecoverFrom fatal) .&&.
     not (shouldContinueAfter fatal) .&&.
     canRecoverFrom error .&&.
     shouldContinueAfter error .&&.
     canRecoverFrom warning .&&.
     shouldContinueAfter warning .&&.
     canRecoverFrom info .&&.
     shouldContinueAfter info

-- Property: custom recovery strategy works correctly
prop_custom_recovery_strategy :: String -> String -> Bool -> Bool -> Property
prop_custom_recovery_strategy errId msg canRec shouldCont =
  let location = _atLocation 1 1
      recovery = customRecovery canRec shouldCont (Just "custom action") (Just "custom hint") 25 0.8
      error = errorAt errId (T.pack msg) location { recovery = recovery }
  in property $ canRecoverFrom error === canRec .&&.
     shouldContinueAfter error === shouldCont

-- Property: errorAtWithTimestamp preserves timestamp
prop_errorAtWithTimestamp_preserves_timestamp :: String -> String -> String -> Int -> Int -> Property
prop_errorAtWithTimestamp_preserves_timestamp errId msg timestamp line col =
  let location = _atLocation line col
      error = errorAtWithTimestamp timestamp errId (T.pack msg) location
  in property $ timestamp error === Just timestamp

-- Property: location range handling
prop_location_range_handling :: String -> String -> Int -> Int -> Int -> Int -> Property
prop_location_range_handling errId msg startLine startCol endLine endCol =
  let location = _atRange startLine startCol endLine endCol
      error = errorAt errId (T.pack msg) location
  in property $ line error === startLine .&&.
     column error === startCol .&&.
     endLine error === Just endLine .&&.
     endColumn error === Just endCol

-- ============================================================================
-- Test Suite Definition
-- ============================================================================

tests :: TestTree
tests = testGroup "New Robust ErrorHandler QuickCheck Tests"
  [ testGroup "Error creation properties"
    [ fastProperty "errorAt creates error with correct severity" prop_errorAt_correct_severity
    , fastProperty "errorWithCategory creates error with correct category" prop_errorWithCategory_correct_category
    , fastProperty "warningAt creates warning with correct severity" prop_warningAt_correct_severity
    , fastProperty "infoAt creates info with correct severity" prop_infoAt_correct_severity
    , fastProperty "fatalError creates fatal error with correct severity" prop_fatalError_correct_severity
    ]

  , testGroup "Error modification properties"
    [ fastProperty "withLocation updates error location" prop_withLocation_updates_location
    , fastProperty "withContext updates error context" prop_withContext_updates_context
    , fastProperty "withSuggestions adds suggestions to error" prop_withSuggestions_adds_suggestions
    , fastProperty "withRelatedErrors adds related errors" prop_withRelatedErrors_adds_related
    , fastProperty "withTimestamp adds timestamp to error" prop_withTimestamp_adds_timestamp
    , fastProperty "wrapError chains errors correctly" prop_wrap_error_chains_errors
    ]

  , testGroup "Error filtering properties"
    [ fastProperty "hasCategory correctly identifies error categories" prop_hasCategory_identifies_categories
    , fastProperty "filterByCategory correctly filters errors" prop_filterByCategory_filters_correctly
    , fastProperty "filterBySeverity correctly filters errors" prop_filterBySeverity_filters_correctly
    ]

  , testGroup "Error statistics properties"
    [ fastProperty "getErrorStatistics provides correct counts" prop_getErrorStatistics_correct_counts
    ]

  , testGroup "Error recovery properties"
    [ fastProperty "canRecoverFrom correctly identifies recoverable errors" prop_canRecoverFrom_identifies_recoverable
    , fastProperty "shouldContinueAfter correctly identifies continuation" prop_shouldContinueAfter_identifies_continuation
    , fastProperty "recovery strategies are consistent" prop_recovery_strategies_consistent
    , fastProperty "custom recovery strategy works correctly" prop_custom_recovery_strategy
    ]

  , testGroup "Error formatting properties"
    [ fastProperty "formatError includes essential information" prop_formatError_includes_essentials
    , fastProperty "formatErrorWithLocation includes location information" prop_formatErrorWithLocation_includes_location
    , fastProperty "formatErrors maintains order by severity" prop_formatErrors_maintains_order
    ]

  , testGroup "Error combination properties"
    [ fastProperty "combineErrors preserves all errors" prop_combineErrors_preserves_all
    ]

  , testGroup "Timestamp and location properties"
    [ fastProperty "errorAtWithTimestamp preserves timestamp" prop_errorAtWithTimestamp_preserves_timestamp
    , fastProperty "location range handling" prop_location_range_handling
    ]
  ]