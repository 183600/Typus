module Test.Unit.ToolingErrorSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Tooling.Error

-- Test tooling error creation
prop_tooling_error_creation :: String -> Property
prop_tooling_error_creation errorMsg =
  let error1 = createToolingError errorMsg
      error2 = createToolingError errorMsg
  in property $ getErrorMessage error1 === getErrorMessage error2

-- Test error code assignment
prop_error_code_assignment :: String -> Int -> Property
prop_error_code_assignment errorMsg errorCode =
  let error = createToolingErrorWithCode errorMsg errorCode
      retrievedCode = getErrorCode error
  in property $ retrievedCode === errorCode

-- Test error context chaining
prop_error_context_chaining :: String -> String -> Property
prop_error_context_chaining context1 context2 =
  let baseError = createToolingError "base error"
      errorWithContext1 = addErrorContext baseError context1
      errorWithContext2 = addErrorContext errorWithContext1 context2
      fullContext = getErrorContext errorWithContext2
  in property $ context1 `isInfixOf` fullContext && context2 `isInfixOf` fullContext

-- Test error severity levels
prop_error_severity_levels :: ErrorSeverity -> Property
prop_error_severity_levels severity =
  let error = createToolingErrorWithSeverity "test error" severity
      retrievedSeverity = getErrorSeverity error
  in property $ retrievedSeverity === severity

-- Test error formatting
prop_error_formatting_includes_all_info :: String -> Int -> ErrorSeverity -> Property
prop_error_formatting_includes_all_info errorMsg errorCode severity =
  let error = createToolingErrorWithCodeAndSeverity errorMsg errorCode severity
      formatted = formatToolingError error
  in property $ 
    errorMsg `isInfixOf` formatted && 
    show errorCode `isInfixOf` formatted &&
    show severity `isInfixOf` formatted

tests :: TestTree
tests = testGroup "ToolingError Tests"
  [ testProperty "tooling error creation" prop_tooling_error_creation
  , testProperty "error code assignment" prop_error_code_assignment
  , testProperty "error context chaining" prop_error_context_chaining
  , testProperty "error severity levels" prop_error_severity_levels
  , testProperty "error formatting includes all info" prop_error_formatting_includes_all_info
  ]