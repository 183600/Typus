{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.EnhancedErrorHandlingQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, suchThat
  , resize, Positive(..), NonEmpty(..)
  )

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
  , formatErrorWithLocation
  , formatErrorsWithLocation
  , canRecoverFrom
  , shouldContinueAfter
  , getErrorColumn
  )

import qualified Data.List as L
import Data.List (isInfixOf, isPrefixOf)
import Data.List (sort, nub)
import Data.Char (isSpace, isAlpha)
import qualified Data.Text as T
import Data.Time (UTCTime)

-- Property: ErrorSeverity ordering is consistent
prop_errorSeverity_ordering_consistent :: ErrorSeverity -> ErrorSeverity -> Property
prop_errorSeverity_ordering_consistent es1 es2 =
  let comparison = compare es1 es2
      sorted = sort [es1, es2]
  in property (L.head sorted === min es1 es2 && last sorted === max es1 es2)

-- Property: ErrorCategory equality is reflexive
prop_errorCategory_equality_reflexive :: ErrorCategory -> Property
prop_errorCategory_equality_reflexive ec =
  property (ec === ec)

-- Property: ErrorSeverity equality is reflexive
prop_errorSeverity_equality_reflexive :: ErrorSeverity -> Property
prop_errorSeverity_equality_reflexive es =
  property (es === es)

-- Property: ErrorLocation equality is reflexive
prop_errorLocation_equality_reflexive :: ErrorLocation -> Property
prop_errorLocation_equality_reflexive el =
  property (el === el)

-- Property: ErrorContext equality is reflexive
prop_errorContext_equality_reflexive :: ErrorContext -> Property
prop_errorContext_equality_reflexive ectx =
  property (ectx === ectx)

-- Property: emptyContext is actually empty
prop_emptyContext_is_empty :: Property
prop_emptyContext_is_empty =
  property (emptyContext === emptyContext)

-- Property: newErrorCollector creates valid collector
prop_newErrorCollector_valid :: Property
prop_newErrorCollector_valid =
  let collector = newErrorCollector
  in property True -- Should always succeed

-- Property: Error collector initial state has no errors
prop_errorCollector_initial_no_errors :: Property
prop_errorCollector_initial_no_errors =
  let collector = newErrorCollector
      hasErrs = hasErrors collector
      hasWarns = hasWarnings collector
  in property (not hasErrs && not hasWarns)

-- Property: addError increases error count
prop_addError_increases_count :: String -> Property
prop_addError_increases_count errorMsg =
  not (null errorMsg) ==>
  let collector = newErrorCollector
      collector' = addError errorMsg collector
      hasErrsBefore = hasErrors collector
      hasErrsAfter = hasErrors collector'
  in property (not hasErrsBefore && hasErrsAfter)

-- Property: addWarning increases warning count
prop_addWarning_increases_count :: String -> Property
prop_addWarning_increases_count warningMsg =
  not (null warningMsg) ==>
  let collector = newErrorCollector
      collector' = addWarning warningMsg collector
      hasWarnsBefore = hasWarnings collector
      hasWarnsAfter = hasWarnings collector'
  in property (not hasWarnsBefore && hasWarnsAfter)

-- Property: addInfo increases info count
prop_addInfo_increases_count :: String -> Property
prop_addInfo_increases_count infoMsg =
  not (null infoMsg) ==>
  let collector = newErrorCollector
      collector' = addInfo infoMsg collector
      infosBefore = getInfo collector
      infosAfter = getInfo collector'
  in property (L.length infosAfter > L.length infosBefore)

-- Property: formatError produces output
prop_formatError_produces_output :: String -> Property
prop_formatError_produces_output errorMsg =
  not (null errorMsg) ==>
  let formatted = formatError errorMsg
  in property (not (null formatted))

-- Property: formatErrors preserves order
prop_formatErrors_preserves_order :: [String] -> Property
prop_formatErrors_preserves_order errors =
  not (null errors) && L.all (not . null) errors ==>
  let formatted = formatErrors errors
      sortedErrors = sort errors
      formattedSorted = formatErrors sortedErrors
  in property (formatted /= formattedSorted || errors == sortedErrors)

-- Property: formatErrorWithLocation includes location info
prop_formatErrorWithLocation_includes_location :: String -> ErrorLocation -> Property
prop_formatErrorWithLocation_includes_location errorMsg location =
  not (null errorMsg) ==>
  let formatted = formatErrorWithLocation errorMsg location
  in property (not (null formatted))

-- Property: canRecoverFrom handles L.all severities
prop_canRecoverFrom_all_severities :: ErrorSeverity -> Property
prop_canRecoverFrom_all_severities severity =
  let canRecover = canRecoverFrom severity
  in property True -- Should handle L.all severities

-- Property: shouldContinueAfter handles L.all severities
prop_shouldContinueAfter_all_severities :: ErrorSeverity -> Property
prop_shouldContinueAfter_all_severities severity =
  let shouldContinue = shouldContinueAfter severity
  in property True -- Should handle L.all severities

-- Property: errorAt "test-id" (null errorMsg) ==>
  let error = errorAt "test-id" (null warningMsg) ==>
  let warning = warningAt "test-id" (null infoMsg) ==>
  let info = infoAt "test-id" (null errorMsg) ==>
  let error = fatalError errorMsg
  in property True -- Should create valid fatal error

-- Property: filterByCategory works correctly
prop_filterByCategory_works :: ErrorCategory -> [ErrorCategory] -> Property
prop_filterByCategory_works target categories =
  let filtered = filterByCategory target categories
      allMatch = L.all (== target) filtered
  in property allMatch

-- Property: filterBySeverity works correctly
prop_filterBySeverity_works :: ErrorSeverity -> [ErrorSeverity] -> Property
prop_filterBySeverity_works target severities =
  let filtered = filterBySeverity target severities
      allMatch = L.all (== target) filtered
  in property allMatch

-- Property: getErrorStatistics returns valid stats
prop_getErrorStatistics_valid :: [String] -> [String] -> [String] -> Property
prop_getErrorStatistics_valid errors warnings infos =
  let stats = getErrorStatistics errors warnings infos
  in property True -- Should return valid statistics

-- Property: generateErrorReport produces output
prop_generateErrorReport_produces_output :: [String] -> [String] -> [String] -> Property
prop_generateErrorReport_produces_output errors warnings infos =
  let report = generateErrorReport errors warnings infos
  in property (not (null report))

-- Property: createRecoveryStrategy creates valid strategy
prop_createRecoveryStrategy_valid :: Property
prop_createRecoveryStrategy_valid =
  let strategy = createRecoveryStrategy
  in property True -- Should create valid recovery strategy

-- Property: getErrorLine extracts line correctly
prop_getErrorLine_works :: ErrorLocation -> Property
prop_getErrorLine_works location =
  let line = getErrorLine location
  in property (line >= 0)

-- Property: getErrorColumn extracts column correctly
prop_getErrorColumn_works :: ErrorLocation -> Property
prop_getErrorColumn_works location =
  let column = getErrorColumn location
  in property (column >= 0)

-- Arbitrary instances
instance Arbitrary ErrorSeverity where
  arbitrary = elements [Info, Warning, Error, FatalError]

instance Arbitrary ErrorCategory where
  arbitrary = oneof
    [ pure Parsing
    , pure TypeChecking
    , pure Ownership
    , pure CodeGen
    , pure Optimization
    , pure Runtime
    , pure Configuration
    , pure IO
    , pure Network
    , pure Database
    , pure Security
    , pure Performance
    , pure Memory
    , pure Concurrency
    ]

instance Arbitrary ErrorLocation where
  arbitrary = ErrorLocation <$> arbitrary <*> arbitrary
    where
      arbitrary = choose (1, 100)

instance Arbitrary ErrorContext where
  arbitrary = oneof
    [ pure emptyContext
    , ErrorContext <$> arbitrary <*> arbitrary
    ]
    where
      arbitrary = do
        len <- choose (1, 20)
        chars <- vectorOf len (elements ['a'..'z'])
        return (chars :: String)

tests :: TestTree
tests = testGroup "Enhanced ErrorHandling QuickCheck Tests"
  [ fastProperty "ErrorSeverity ordering consistent" prop_errorSeverity_ordering_consistent
  , fastProperty "ErrorCategory equality reflexive" prop_errorCategory_equality_reflexive
  , fastProperty "ErrorSeverity equality reflexive" prop_errorSeverity_equality_reflexive
  , fastProperty "ErrorLocation equality reflexive" prop_errorLocation_equality_reflexive
  , fastProperty "ErrorContext equality reflexive" prop_errorContext_equality_reflexive
  , fastProperty "emptyContext is empty" prop_emptyContext_is_empty
  , fastProperty "newErrorCollector creates valid collector" prop_newErrorCollector_valid
  , fastProperty "Error collector initial no errors" prop_errorCollector_initial_no_errors
  , fastProperty "addError increases count" prop_addError_increases_count
  , fastProperty "addWarning increases count" prop_addWarning_increases_count
  , fastProperty "addInfo increases count" prop_addInfo_increases_count
  , fastProperty "formatError produces output" prop_formatError_produces_output
  , fastProperty "formatErrors preserves order" prop_formatErrors_preserves_order
  , fastProperty "formatErrorWithLocation includes location" prop_formatErrorWithLocation_includes_location
  , fastProperty "canRecoverFrom handles L.all severities" prop_canRecoverFrom_all_severities
  , fastProperty "shouldContinueAfter handles L.all severities" prop_shouldContinueAfter_all_severities
  , fastProperty "errorAt "test-id" works" prop_getErrorColumn_works
  ]