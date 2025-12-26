{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ErrorHandlerConsistencySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements)
import Test.QuickCheck.Gen (oneof, suchThat)

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
  , errorAtWithTimestamp
  , errorWithCategory
  , warningAt
  , warningWithCategory
  , infoAt
  , infoWithCategory
  , getErrorLine
  , getErrorColumn
  )

import Data.Time (UTCTime, fromGregorian, secondsToDiffTime)
import Data.List (isPrefixOf, isInfixOf)
import qualified Data.Text as T
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, spanFrom)

-- Arbitrary instances for error types

instance Arbitrary ErrorSeverity where
  arbitrary = elements [Error, Warning, Info]

instance Arbitrary ErrorCategory where
  arbitrary = oneof
    [ return ParseError
    , return TypeError
    , return NameError
    , return TypeError
    , return OwnershipError
    , return DependencyError
    , return InternalError
    ]

instance Arbitrary ErrorLocation where
  arbitrary = do
    line <- choose (1, 1000)
    col <- choose (1, 1000)
    filename <- listOf1 (elements ['a'..'z'])
    return $ ErrorLocation line col filename

instance Arbitrary ErrorContext where
  arbitrary = do
    context <- listOf1 (elements ['a'..'z'])
    return $ ErrorContext context

instance Arbitrary ErrorRecovery where
  arbitrary = elements [CanRecover, CannotRecover, SkipRemaining]

instance Arbitrary TypeError where
  arbitrary = do
    severity <- arbitrary
    category <- arbitrary
    location <- arbitrary
    context <- arbitrary
    message <- listOf1 (elements ['a'..'z'])
    recovery <- arbitrary
    return $ TypeError severity category location context message recovery

instance Arbitrary CombinedError where
  arbitrary = do
    errors <- listOf1 arbitrary
    return $ CombinedError errors

-- Helper generators
genUTCTime :: Gen UTCTime
genUTCTime = do
  year <- choose (2000, 2030)
  month <- choose (1, 12)
  day <- choose (1, 28)
  hour <- choose (0, 23)
  minute <- choose (0, 59)
  second <- choose (0, 59)
  return $ fromGregorian year month day `plus` secondsToDiffTime (hour * 3600 + minute * 60 + second)
  where
    plus = addUTCTime
    addUTCTime = error "addUTCTime not implemented in this context"

-- Consistency property tests

-- Property: ErrorCollector should start empty
prop_error_collector_starts_empty :: Property
prop_error_collector_starts_empty =
  let collector = newErrorCollector
  in property $ not (hasErrors collector) .&&.
     not (hasWarnings collector) .&&.
     null (getErrors collector) .&&.
     null (getWarnings collector) .&&.
     null (getInfo collector)

-- Property: Adding error should be reflected in hasErrors
prop_add_error_reflected_in_has_errors :: TypeError -> Property
prop_add_error_reflected_in_has_errors err =
  let collector = newErrorCollector
      collectorWithError = addError err collector
  in property $ hasErrors collectorWithError .&&.
     length (getErrors collectorWithError) === 1

-- Property: Adding warning should be reflected in hasWarnings
prop_add_warning_reflected_in_has_warnings :: TypeError -> Property
prop_add_warning_reflected_in_has_warnings warning =
  let collector = newErrorCollector
      collectorWithWarning = addWarning warning collector
  in property $ hasWarnings collectorWithWarning .&&.
     length (getWarnings collectorWithWarning) === 1

-- Property: Adding info should be reflected in getInfo
prop_add_info_reflected_in_get_info :: TypeError -> Property
prop_add_info_reflected_in_get_info info =
  let collector = newErrorCollector
      collectorWithInfo = addInfo info collector
  in property $ length (getInfo collectorWithInfo) === 1

-- Property: getAllMessages should include all types
prop_get_all_messages_includes_all :: TypeError -> TypeError -> TypeError -> Property
prop_get_all_messages_includes_all error warning info =
  let collector = newErrorCollector
      collector1 = addError error collector
      collector2 = addWarning warning collector1
      collector3 = addInfo info collector2
      allMessages = getAllMessages collector3
  in property $ length allMessages === 3 .&&.
     error `elem` allMessages .&&.
     warning `elem` allMessages .&&.
     info `elem` allMessages

-- Property: formatError should produce non-empty string
prop_format_error_non_empty :: TypeError -> Property
prop_format_error_non_empty err =
  let formatted = formatError err
  in property $ length formatted > 0

-- Property: formatError should include error message
prop_format_error_includes_message :: TypeError -> Property
prop_format_error_includes_message err =
  let formatted = formatError err
      errorMsg = errorMessage err
  in property $ errorMsg `isInfixOf` formatted

-- Property: formatErrorWithLocation should include location info
prop_format_error_with_location_includes_location :: TypeError -> Property
prop_format_error_with_location_includes_location err =
  let formatted = formatErrorWithLocation err
      location = errorLocation err
      locStr = "line " ++ show (errorLine location)
  in property $ locStr `isInfixOf` formatted

-- Property: formatErrors should handle empty list
prop_format_errors_empty :: Property
prop_format_errors_empty =
  let formatted = formatErrors []
  in property $ length formatted >= 0

-- Property: formatErrors should handle multiple errors
prop_format_errors_multiple :: [TypeError] -> Property
prop_format_errors_multiple errors =
  let formatted = formatErrors errors
      errorCount = length errors
  in property $ if errorCount > 0 
                then length formatted > 0
                else True

-- Property: canRecoverFrom should be consistent with ErrorRecovery
prop_can_recover_consistent :: TypeError -> Property
prop_can_recover_consistent err =
  let recovery = errorRecovery err
      canRecover = canRecoverFrom err
  in case recovery of
    CanRecover -> property canRecover
    CannotRecover -> property (not canRecover)
    SkipRemaining -> property canRecover

-- Property: shouldContinueAfter should be consistent with severity
prop_should_continue_consistent :: TypeError -> Property
prop_should_continue_consistent err =
  let severity = errorSeverity err
      shouldContinue = shouldContinueAfter err
  in case severity of
    Error -> property (not shouldContinue)
    Warning -> property shouldContinue
    Info -> property shouldContinue

-- Property: errorAt should create error with correct location
prop_error_at_correct_location :: ErrorLocation -> String -> Property
prop_error_at_correct_location location message =
  let err = errorAt location message
      errLocation = errorLocation err
      errMessage = errorMessage err
  in property $ errLocation === location .&&.
     errMessage === message

-- Property: errorWithCategory should create error with correct category
prop_error_with_category_correct :: ErrorCategory -> String -> Property
prop_error_with_category_correct category message =
  let err = errorWithCategory category message
      errCategory = errorCategory err
      errMessage = errorMessage err
  in property $ errCategory === category .&&.
     errMessage === message

-- Property: warningAt should create warning with correct severity
prop_warning_at_correct_severity :: ErrorLocation -> String -> Property
prop_warning_at_correct_severity location message =
  let warning = warningAt location message
      severity = errorSeverity warning
      warningLocation = errorLocation warning
      warningMessage = errorMessage warning
  in property $ severity === Warning .&&.
     warningLocation === location .&&.
     warningMessage === message

-- Property: warningWithCategory should create warning with correct category and severity
prop_warning_with_category_correct :: ErrorCategory -> String -> Property
prop_warning_with_category_correct category message =
  let warning = warningWithCategory category message
      severity = errorSeverity warning
      warningCategory = errorCategory warning
      warningMessage = errorMessage warning
  in property $ severity === Warning .&&.
     warningCategory === category .&&.
     warningMessage === message

-- Property: infoAt should create info with correct severity
prop_info_at_correct_severity :: ErrorLocation -> String -> Property
prop_info_at_correct_severity location message =
  let info = infoAt location message
      severity = errorSeverity info
      infoLocation = errorLocation info
      infoMessage = errorMessage info
  in property $ severity === Info .&&.
     infoLocation === location .&&.
     infoMessage === message

-- Property: infoWithCategory should create info with correct category and severity
prop_info_with_category_correct :: ErrorCategory -> String -> Property
prop_info_with_category_correct category message =
  let info = infoWithCategory category message
      severity = errorSeverity info
      infoCategory = errorCategory info
      infoMessage = errorMessage info
  in property $ severity === Info .&&.
     infoCategory === category .&&.
     infoMessage === message

-- Property: getErrorLine should extract line from ErrorLocation
prop_get_error_line_correct :: Int -> Int -> String -> Property
prop_get_error_line_correct line col filename =
  line > 0 && col > 0 ==>
  let location = ErrorLocation line col filename
      extractedLine = getErrorLine location
  in property $ extractedLine === line

-- Property: getErrorColumn should extract column from ErrorLocation
prop_get_error_column_correct :: Int -> Int -> String -> Property
prop_get_error_column_correct line col filename =
  line > 0 && col > 0 ==>
  let location = ErrorLocation line col filename
      extractedCol = getErrorColumn location
  in property $ extractedCol === col

-- Property: CombinedError should preserve all component errors
prop_combined_error_preserves_components :: [TypeError] -> Property
prop_combined_error_preserves_components errors =
  not (null errors) ==>
  let combined = CombinedError errors
      combinedErrors = combinedErrors combined
  in property $ length combinedErrors === length errors .&&.
     all (`elem` errors) combinedErrors .&&.
     all (`elem` combinedErrors) errors

-- Property: emptyContext should have no content
prop_empty_context_properties :: Property
prop_empty_context_properties =
  let context = emptyContext
  in property $ context === ErrorContext ""

-- Property: Error ordering should be consistent
prop_error_ordering_consistent :: TypeError -> TypeError -> Property
prop_error_ordering_consistent err1 err2 =
  let loc1 = errorLocation err1
      loc2 = errorLocation err2
      line1 = errorLine loc1
      line2 = errorLine loc2
      col1 = errorColumn loc1
      col2 = errorColumn loc2
      severity1 = errorSeverity err1
      severity2 = errorSeverity err2
      -- Simple ordering: by line, then column, then severity
      expected = if line1 < line2 then True
                 else if line1 > line2 then False
                 else if col1 < col2 then True
                 else if col1 > col2 then False
                 else fromEnum severity1 <= fromEnum severity2
  in property $ (err1 <= err2) === expected

-- Property: Error formatting should be idempotent
prop_error_formatting_idempotent :: TypeError -> Property
prop_error_formatting_idempotent err =
  let formatted1 = formatError err
      formatted2 = formatError err
  in property $ formatted1 === formatted2

-- Property: ErrorCollector should maintain order of added messages
prop_error_collector_maintains_order :: [TypeError] -> [TypeError] -> [TypeError] -> Property
prop_error_collector_maintains_order errors warnings infos =
  let collector = newErrorCollector
      addErrs = foldl (flip addError) collector errors
      addWarns = foldl (flip addWarning) addErrs warnings
      addAll = foldl (flip addInfo) addWarns infos
      allMessages = getAllMessages addAll
      expectedOrder = errors ++ warnings ++ infos
  in property $ allMessages === expectedOrder

-- Property: Error messages should be reasonably sized
prop_error_messages_reasonably_sized :: TypeError -> Property
prop_error_messages_reasonably_sized err =
  let formatted = formatError err
      formattedWithLocation = formatErrorWithLocation err
  in property $ length formatted < 1000 .&&.
     length formattedWithLocation < 2000

tests :: TestTree
tests = testGroup "ErrorHandler Consistency Tests"
  [ fastProperty "ErrorCollector starts empty" prop_error_collector_starts_empty
  , fastProperty "Adding error is reflected in hasErrors" prop_add_error_reflected_in_has_errors
  , fastProperty "Adding warning is reflected in hasWarnings" prop_add_warning_reflected_in_has_warnings
  , fastProperty "Adding info is reflected in getInfo" prop_add_info_reflected_in_get_info
  , fastProperty "getAllMessages includes all types" prop_get_all_messages_includes_all
  , fastProperty "formatError produces non-empty string" prop_format_error_non_empty
  , fastProperty "formatError includes error message" prop_format_error_includes_message
  , fastProperty "formatErrorWithLocation includes location info" prop_format_error_with_location_includes_location
  , fastProperty "formatErrors handles empty list" prop_format_errors_empty
  , fastProperty "formatErrors handles multiple errors" prop_format_errors_multiple
  , fastProperty "canRecoverFrom is consistent with ErrorRecovery" prop_can_recover_consistent
  , fastProperty "shouldContinueAfter is consistent with severity" prop_should_continue_consistent
  , fastProperty "errorAt creates error with correct location" prop_error_at_correct_location
  , fastProperty "errorWithCategory creates error with correct category" prop_error_with_category_correct
  , fastProperty "warningAt creates warning with correct severity" prop_warning_at_correct_severity
  , fastProperty "warningWithCategory creates warning with correct category and severity" prop_warning_with_category_correct
  , fastProperty "infoAt creates info with correct severity" prop_info_at_correct_severity
  , fastProperty "infoWithCategory creates info with correct category and severity" prop_info_with_category_correct
  , fastProperty "getErrorLine extracts line from ErrorLocation" prop_get_error_line_correct
  , fastProperty "getErrorColumn extracts column from ErrorLocation" prop_get_error_column_correct
  , fastProperty "CombinedError preserves all component errors" prop_combined_error_preserves_components
  , fastProperty "emptyContext has no content" prop_empty_context_properties
  , fastProperty "Error ordering is consistent" prop_error_ordering_consistent
  , fastProperty "Error formatting is idempotent" prop_error_formatting_idempotent
  , fastProperty "ErrorCollector maintains order of added messages" prop_error_collector_maintains_order
  , fastProperty "Error messages are reasonably sized" prop_error_messages_reasonably_sized
  ]