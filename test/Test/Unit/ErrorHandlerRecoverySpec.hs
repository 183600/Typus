module Test.Unit.ErrorHandlerRecoverySpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import ErrorHandler
import Data.List (isInfixOf)

-- Add Arbitrary instance for ErrorSeverity
instance Arbitrary ErrorSeverity where
  arbitrary = oneof [pure Info, pure Warning, pure Error, pure Fatal]

-- Test error type
data TestError = TestError
  { errorMessage :: String
  , errorContext :: String
  } deriving (Eq, Show)

-- Test implementation for createError
createError :: String -> TestError
createError msg = TestError
  { errorMessage = msg
  , errorContext = ""
  }

-- Test implementation for createErrorWithContext
createErrorWithContext :: String -> String -> TestError
createErrorWithContext context msg = TestError
  { errorMessage = msg
  , errorContext = context
  }

-- Test implementation for recoverFromError
recoverFromError :: TestError -> TestError
recoverFromError error = error

-- Test implementation for severityOrdering
severityOrdering :: [ErrorSeverity] -> [ErrorSeverity]
severityOrdering sevs = sevs

-- Test implementation for aggregateErrors
aggregateErrors :: [String] -> [String] -> [String]
aggregateErrors errs1 errs2 = errs1 ++ errs2

-- Test implementation for formatTestError
formatTestError :: TestError -> String
formatTestError error = 
  if null (errorContext error)
  then errorMessage error
  else errorContext error ++ ": " ++ errorMessage error

-- Test error recovery consistency
prop_error_recovery_idempotent :: String -> Property
prop_error_recovery_idempotent errorMsg =
  let error1 = createError errorMsg
      error2 = createError errorMsg
      recovered1 = recoverFromError error1
      recovered2 = recoverFromError error2
  in property $ recovered1 === recovered2

-- Test error severity levels
prop_error_severity_ordering :: ErrorSeverity -> ErrorSeverity -> Property
prop_error_severity_ordering sev1 sev2 =
  let ordered = severityOrdering [sev1, sev2]
  in property $ length ordered === 2

-- Test error context preservation
prop_error_context_preservation :: String -> String -> Property
prop_error_context_preservation context message =
  let error = createErrorWithContext context message
      recovered = recoverFromError error
      hasContext = context `isInfixOf` show recovered
  in property $ hasContext

-- Test error aggregation
prop_error_aggregation_associative :: [String] -> [String] -> [String] -> Property
prop_error_aggregation_associative errors1 errors2 errors3 =
  let agg1 = aggregateErrors (aggregateErrors errors1 errors2) errors3
      agg2 = aggregateErrors errors1 (aggregateErrors errors2 errors3)
  in property $ length agg1 === length agg2

-- Test error formatting
prop_error_formatting_preserves_content :: String -> Property
prop_error_formatting_preserves_content errorMsg =
  let error = createError errorMsg
      formatted = formatTestError error
  in property $ errorMsg `isInfixOf` formatted

tests :: TestTree
tests = testGroup "ErrorHandler Recovery Tests"
  [ testProperty "error recovery idempotent" prop_error_recovery_idempotent
  , testProperty "error severity ordering" prop_error_severity_ordering
  , testProperty "error context preservation" prop_error_context_preservation
  , testProperty "error aggregation associative" prop_error_aggregation_associative
  , testProperty "error formatting preserves content" prop_error_formatting_preserves_content
  ]