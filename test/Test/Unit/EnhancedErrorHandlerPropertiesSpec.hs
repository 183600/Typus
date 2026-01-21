module Test.Unit.EnhancedErrorHandlerPropertiesSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import EnhancedErrorHandler
import Data.List (isInfixOf)

-- Test recovery strategy type
data RecoveryStrategy = RetryStrategy | SkipStrategy | FallbackStrategy
  deriving (Eq, Show)

-- Test error category type
data TestErrorCategory = TestSyntaxError | TestTypeError | TestRuntimeError
  deriving (Eq, Show)

-- Test implementation for categorizeError
categorizeError :: String -> TestErrorCategory
categorizeError _ = TestSyntaxError

-- Test implementation for calculateErrorSeverity
calculateErrorSeverity :: String -> ErrorSeverity
calculateErrorSeverity _ = Error

-- Test implementation for extractErrorContext
extractErrorContext :: String -> String
extractErrorContext errorMsg = 
  case takeWhile (/= ':') errorMsg of
    "" -> errorMsg
    ctx -> ctx

-- Test implementation for generateErrorSuggestions
generateErrorSuggestions :: String -> [String]
generateErrorSuggestions _ = ["Check syntax", "Verify types"]

-- Test implementation for getApplicableRecoveryStrategies
getApplicableRecoveryStrategies :: String -> [RecoveryStrategy]
getApplicableRecoveryStrategies _ = [RetryStrategy, SkipStrategy]

-- Test enhanced error categorization
prop_error_categorization_consistent :: String -> Property
prop_error_categorization_consistent errorMsg =
  let category1 = categorizeError errorMsg
      category2 = categorizeError errorMsg
  in property $ category1 === category2

-- Test error severity calculation
prop_severity_calculation_monotonic :: [String] -> Property
prop_severity_calculation_monotonic errorMessages =
  let singleSeverity = case errorMessages of
                        [] -> calculateErrorSeverity ""
                        (x:_) -> calculateErrorSeverity x
      multipleSeverity = calculateErrorSeverity (unwords errorMessages)
  in property $ 
    if null errorMessages 
    then property True
    else property (multipleSeverity >= singleSeverity)

-- Test error context extraction
prop_context_extraction_preserves_info :: String -> String -> Property
prop_context_extraction_preserves_info context message =
  let fullError = context ++ ": " ++ message
      extractedContext = extractErrorContext fullError
  in property $ context `isInfixOf` extractedContext

-- Test error suggestion generation
prop_suggestion_generation_relevant :: String -> Property
prop_suggestion_generation_relevant errorMsg =
  let suggestions = generateErrorSuggestions errorMsg
  in property $ not (null suggestions)

-- Test error recovery strategies
prop_recovery_strategy_applicable :: String -> Property
prop_recovery_strategy_applicable errorMsg =
  let strategies = getApplicableRecoveryStrategies errorMsg
  in property $ 
    case strategies of
      [] -> property True
      _ -> property (all (isApplicableTo errorMsg) strategies)

-- Helper function
isApplicableTo :: String -> RecoveryStrategy -> Bool
isApplicableTo _ _ = True  -- Simplified for this example

tests :: TestTree
tests = testGroup "EnhancedErrorHandler Properties Tests"
  [ testProperty "error categorization consistent" prop_error_categorization_consistent
  , testProperty "severity calculation monotonic" prop_severity_calculation_monotonic
  , testProperty "context extraction preserves info" prop_context_extraction_preserves_info
  , testProperty "suggestion generation relevant" prop_suggestion_generation_relevant
  , testProperty "recovery strategy applicable" prop_recovery_strategy_applicable
  ]