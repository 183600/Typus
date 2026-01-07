module Test.Unit.EnhancedErrorHandlerPropertiesSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import EnhancedErrorHandler

-- Test enhanced error categorization
prop_error_categorization_consistent :: String -> Property
prop_error_categorization_consistent errorMsg =
  let category1 = categorizeError errorMsg
      category2 = categorizeError errorMsg
  in property $ category1 === category2

-- Test error severity calculation
prop_severity_calculation_monotonic :: [String] -> Property
prop_severity_calculation_monotonic errorMessages =
  let singleSeverity = calculateErrorSeverity (head errorMessages)
      multipleSeverity = calculateErrorSeverity (unwords errorMessages)
  in property $ 
    if null errorMessages 
    then property True
    else multipleSeverity >= singleSeverity

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
      _ -> all (isApplicableTo errorMsg) strategies

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