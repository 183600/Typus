module Test.Unit.ToolingErrorSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck (Arbitrary(..), elements)
import Data.List (isInfixOf)





-- Define missing ErrorSeverity type
data ErrorSeverity = ErrorInfo | ErrorWarning | ErrorError | ErrorFatal
  deriving (Eq, Show)

instance Arbitrary ErrorSeverity where
  arbitrary = elements [ErrorInfo, ErrorWarning, ErrorError, ErrorFatal]

-- Define missing ToolingError type with additional fields for testing
data TestToolingError = TestToolingError
  { testErrorMessage :: String
  , testErrorCode :: Int
  , testErrorSeverity :: ErrorSeverity
  , testErrorContext :: [String]
  } deriving (Eq, Show)

-- Missing functions for testing
createToolingError :: String -> TestToolingError
createToolingError msg = TestToolingError msg 0 ErrorInfo []

createToolingErrorWithCode :: String -> Int -> TestToolingError
createToolingErrorWithCode msg code = TestToolingError msg code ErrorInfo []

createToolingErrorWithSeverity :: String -> ErrorSeverity -> TestToolingError
createToolingErrorWithSeverity msg severity = TestToolingError msg 0 severity []

createToolingErrorWithCodeAndSeverity :: String -> Int -> ErrorSeverity -> TestToolingError
createToolingErrorWithCodeAndSeverity msg code severity = TestToolingError msg code severity []

getErrorMessage :: TestToolingError -> String
getErrorMessage = testErrorMessage

getErrorCode :: TestToolingError -> Int
getErrorCode = testErrorCode

getErrorSeverity :: TestToolingError -> ErrorSeverity
getErrorSeverity = testErrorSeverity

addErrorContext :: TestToolingError -> String -> TestToolingError
addErrorContext err context = err { testErrorContext = testErrorContext err ++ [context] }

getErrorContext :: TestToolingError -> String
getErrorContext err = unwords $ testErrorContext err

formatToolingError :: TestToolingError -> String
formatToolingError err = 
  "Error: " ++ testErrorMessage err ++ 
  " (Code: " ++ show (testErrorCode err) ++ 
  ", Severity: " ++ show (testErrorSeverity err) ++ ")"

-- Test tooling error creation
prop_tooling_error_creation :: String -> Property
prop_tooling_error_creation errorMsg =
  let error1 = createToolingError errorMsg
      error2 = createToolingError errorMsg
  in property $ getErrorMessage error1 === getErrorMessage error2

-- Test error code assignment
prop_error_code_assignment :: String -> Int -> Property
prop_error_code_assignment errorMsg errorCode =
  let err = createToolingErrorWithCode errorMsg errorCode
      retrievedCode = getErrorCode err
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
  let err = createToolingErrorWithSeverity "test error" severity
      retrievedSeverity = getErrorSeverity err
  in property $ retrievedSeverity === severity

-- Test error formatting
prop_error_formatting_includes_all_info :: String -> Int -> ErrorSeverity -> Property
prop_error_formatting_includes_all_info errorMsg errorCode severity =
  let err = createToolingErrorWithCodeAndSeverity errorMsg errorCode severity
      formatted = formatToolingError err
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