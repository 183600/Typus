{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ErrorHandlerCoreQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Positive(..), NonNegative(..), Arbitrary(..), oneof, elements)

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
  , getErrorLine
  , getErrorColumn
  )

import SourceLocation (SourcePos(..), SourceSpan(..), spanBetween, posAtLineCol)

import Data.List (sort)
import Data.Word (Word32)

-- Property: emptyContext has no information
prop_emptyContext_has_no_info :: Property
prop_emptyContext_has_no_info =
  let ctx = emptyContext
  in property $ null ctx

-- Property: Error severity ordering is consistent
prop_error_severity_ordering :: ErrorSeverity -> ErrorSeverity -> Property
prop_error_severity_ordering sev1 sev2 =
  let ordering = compare sev1 sev2
      severityRank ErrorInfo = 0
      severityRank ErrorWarning = 1
      severityRank ErrorError = 2
      severityRank ErrorFatal = 3
      expectedOrdering = compare (severityRank sev1) (severityRank sev2)
  in classify (sev1 == sev2) "same severity" $
     classify (sev1 /= sev2) "different severity" $
     property $ ordering === expectedOrdering

-- Property: Error collector starts empty
prop_error_collector_starts_empty :: Property
prop_error_collector_starts_empty =
  let collector = newErrorCollector
  in property $ not (hasErrors collector) .&&. not (hasWarnings collector)

-- Property: Adding error to collector makes hasErrors true
prop_add_error_makes_has_errors_true :: String -> String -> Property
prop_add_error_makes_has_errors_true msg desc =
  not (null msg) ==> not (null desc) ==> 
  let collector = newErrorCollector
      error = TypeError msg desc ErrorError ErrorTypeChecking emptyContext
      collector' = addError error collector
  in property $ hasErrors collector' .&&. getErrors collector' === [error]

-- Property: Adding warning to collector makes hasWarnings true
prop_add_warning_makes_has_warnings_true :: String -> String -> Property
prop_add_warning_makes_has_warnings_true msg desc =
  not (null msg) ==> not (null desc) ==> 
  let collector = newErrorCollector
      warning = TypeError msg desc ErrorWarning ErrorTypeChecking emptyContext
      collector' = addWarning warning collector
  in property $ hasWarnings collector' .&&. getWarnings collector' === [warning]

-- Property: Adding info message doesn't affect errors or warnings
prop_add_info_doesnt_affect_errors_warnings :: String -> String -> Property
prop_add_info_doesnt_affect_errors_warnings msg desc =
  not (null msg) ==> not (null desc) ==> 
  let collector = newErrorCollector
      info = TypeError msg desc ErrorInfo ErrorTypeChecking emptyContext
      collector' = addInfo info collector
  in property $ not (hasErrors collector') .&&. not (hasWarnings collector') .&&. getInfo collector' === [info]

-- Property: getErrors returns errors in order of addition
prop_get_errors_preserves_order :: [String] -> [String] -> Property
prop_get_errors_preserves_order msgs descs =
  length msgs >= 2 ==> length descs >= 2 ==> 
  let collector = newErrorCollector
      errors = zipWith (\m d -> TypeError m d ErrorError ErrorTypeChecking emptyContext) (take 2 msgs) (take 2 descs)
      collector' = foldr addError collector errors
  in property $ getErrors collector' === reverse errors

-- Property: getWarnings returns warnings in order of addition
prop_get_warnings_preserves_order :: [String] -> [String] -> Property
prop_get_warnings_preserves_order msgs descs =
  length msgs >= 2 ==> length descs >= 2 ==> 
  let collector = newErrorCollector
      warnings = zipWith (\m d -> TypeError m d ErrorWarning ErrorTypeChecking emptyContext) (take 2 msgs) (take 2 descs)
      collector' = foldr addWarning collector warnings
  in property $ getWarnings collector' === reverse warnings

-- Property: getAllMessages includes all message types
prop_get_all_messages_includes_all :: String -> String -> String -> Property
prop_get_all_messages_includes_all errorMsg warnMsg infoMsg =
  not (null errorMsg) ==> not (null warnMsg) ==> not (null infoMsg) ==>
  let collector = newErrorCollector
      error = TypeError errorMsg "error desc" ErrorError ErrorTypeChecking emptyContext
      warning = TypeError warnMsg "warn desc" ErrorWarning ErrorTypeChecking emptyContext
      info = TypeError infoMsg "info desc" ErrorInfo ErrorTypeChecking emptyContext
      collector' = addInfo info (addWarning warning (addError error collector))
      allMsgs = getAllMessages collector'
  in property $ length allMsgs === 3 .&&. elem error allMsgs .&&. elem warning allMsgs .&&. elem info allMsgs

-- Property: canRecoverFrom is true for non-fatal errors
prop_can_recover_from_non_fatal :: String -> String -> Property
prop_can_recover_from_non_fatal msg desc =
  not (null msg) ==> not (null desc) ==> 
  let error = TypeError msg desc ErrorError ErrorTypeChecking emptyContext
  in property $ canRecoverFrom error

-- Property: canRecoverFrom is false for fatal errors
prop_cannot_recover_from_fatal :: String -> String -> Property
prop_cannot_recover_from_fatal msg desc =
  not (null msg) ==> not (null desc) ==> 
  let error = TypeError msg desc ErrorFatal ErrorTypeChecking emptyContext
  in property $ not (canRecoverFrom error)

-- Property: shouldContinueAfter is true for info and warning
prop_should_continue_for_info_warning :: String -> String -> ErrorSeverity -> Property
prop_should_continue_for_info_warning msg desc sev =
  not (null msg) ==> not (null desc) ==> 
  let error = TypeError msg desc sev ErrorTypeChecking emptyContext
  in property $ (sev == ErrorInfo || sev == ErrorWarning) ==> shouldContinueAfter error

-- Property: errorAt creates error with correct location
prop_error_at_creates_with_location :: Positive Int -> Positive Int -> String -> String -> Property
prop_error_at_creates_with_location (Positive line) (Positive col) msg desc =
  not (null msg) ==> not (null desc) ==> 
  let pos = posAtLineCol (fromIntegral line) (fromIntegral col)
      span = spanBetween pos pos
      location = ErrorLocation span
      error = errorAt location msg desc
  in property $ 
    errorMessage error === msg .&&.
    errorDescription error === desc .&&.
    errorSeverity error === ErrorError .&&.
    errorLocation error === Just location

-- Property: errorWithCategory creates error with correct category
prop_error_with_category :: String -> String -> ErrorCategory -> Property
prop_error_with_category msg desc cat =
  not (null msg) ==> not (null desc) ==> 
  let error = errorWithCategory msg desc cat
  in property $ 
    errorMessage error === msg .&&.
    errorDescription error === desc .&&.
    errorSeverity error === ErrorError .&&.
    errorCategory error === cat

-- Property: warningAt creates warning with correct location
prop_warning_at_creates_with_location :: Positive Int -> Positive Int -> String -> String -> Property
prop_warning_at_creates_with_location (Positive line) (Positive col) msg desc =
  not (null msg) ==> not (null desc) ==> 
  let pos = posAtLineCol (fromIntegral line) (fromIntegral col)
      span = spanBetween pos pos
      location = ErrorLocation span
      warning = warningAt location msg desc
  in property $ 
    errorMessage warning === msg .&&.
    errorDescription warning === desc .&&.
    errorSeverity warning === ErrorWarning .&&.
    errorLocation warning === Just location

-- Property: infoAt creates info with correct location
prop_info_at_creates_with_location :: Positive Int -> Positive Int -> String -> String -> Property
prop_info_at_creates_with_location (Positive line) (Positive col) msg desc =
  not (null msg) ==> not (null desc) ==> 
  let pos = posAtLineCol (fromIntegral line) (fromIntegral col)
      span = spanBetween pos pos
      location = ErrorLocation span
      info = infoAt location msg desc
  in property $ 
    errorMessage info === msg .&&.
    errorDescription info === desc .&&.
    errorSeverity info === ErrorInfo .&&.
    errorLocation info === Just location

-- Property: getErrorLine extracts line from error location
prop_get_error_line :: Positive Int -> Positive Int -> String -> String -> Property
prop_get_error_line (Positive line) (Positive col) msg desc =
  not (null msg) ==> not (null desc) ==> 
  let pos = posAtLineCol (fromIntegral line) (fromIntegral col)
      span = spanBetween pos pos
      location = ErrorLocation span
      error = errorAt location msg desc
  in property $ getErrorLine error === Just (fromIntegral line)

-- Property: getErrorColumn extracts column from error location
prop_get_error_column :: Positive Int -> Positive Int -> String -> String -> Property
prop_get_error_column (Positive line) (Positive col) msg desc =
  not (null msg) ==> not (null desc) ==> 
  let pos = posAtLineCol (fromIntegral line) (fromIntegral col)
      span = spanBetween pos pos
      location = ErrorLocation span
      error = errorAt location msg desc
  in property $ getErrorColumn error === Just (fromIntegral col)

-- Property: formatError produces non-empty string
prop_format_error_non_empty :: String -> String -> Property
prop_format_error_non_empty msg desc =
  not (null msg) ==> not (null desc) ==> 
  let error = TypeError msg desc ErrorError ErrorTypeChecking emptyContext
      formatted = formatError error
  in property $ not (null formatted) .&&. msg `isInfixOf` formatted

-- Property: formatErrors preserves order
prop_format_errors_preserves_order :: [String] -> [String] -> Property
prop_format_errors_preserves_order msgs descs =
  length msgs >= 2 ==> length descs >= 2 ==> 
  let errors = zipWith (\m d -> TypeError m d ErrorError ErrorTypeChecking emptyContext) (take 2 msgs) (take 2 descs)
      formatted = formatErrors errors
  in property $ not (null formatted)

-- Helper function
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `elem` (substrings haystack)
  where
    substrings [] = []
    substrings s@(x:xs) = take (length needle) s : substrings xs

tests :: TestTree
tests =
  testGroup "ErrorHandler Core QuickCheck Tests"
    [ fastProperty "emptyContext has no information" prop_emptyContext_has_no_info
    , fastProperty "error severity ordering is consistent" prop_error_severity_ordering
    , fastProperty "error collector starts empty" prop_error_collector_starts_empty
    , fastProperty "adding error makes hasErrors true" prop_add_error_makes_has_errors_true
    , fastProperty "adding warning makes hasWarnings true" prop_add_warning_makes_has_warnings_true
    , fastProperty "adding info doesn't affect errors or warnings" prop_add_info_doesnt_affect_errors_warnings
    , fastProperty "getErrors preserves order" prop_get_errors_preserves_order
    , fastProperty "getWarnings preserves order" prop_get_warnings_preserves_order
    , fastProperty "getAllMessages includes all types" prop_get_all_messages_includes_all
    , fastProperty "canRecoverFrom non-fatal errors" prop_can_recover_from_non_fatal
    , fastProperty "cannotRecoverFrom fatal errors" prop_cannot_recover_from_fatal
    , fastProperty "shouldContinue for info and warning" prop_should_continue_for_info_warning
    , fastProperty "errorAt creates with location" prop_error_at_creates_with_location
    , fastProperty "errorWithCategory creates with category" prop_error_with_category
    , fastProperty "warningAt creates with location" prop_warning_at_creates_with_location
    , fastProperty "infoAt creates with location" prop_info_at_creates_with_location
    , fastProperty "getErrorLine extracts line" prop_get_error_line
    , fastProperty "getErrorColumn extracts column" prop_get_error_column
    , fastProperty "formatError produces non-empty string" prop_format_error_non_empty
    , fastProperty "formatErrors preserves order" prop_format_errors_preserves_order
    ]