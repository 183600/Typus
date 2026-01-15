module ErrorHandler (
  -- Re-export everything from Compiler.Errors.Core
  module Compiler.Errors.Core,
  -- Additional exports for tests
  ErrorHandler,
  ErrorMessage,
  handleError,
  handleErrors,
  createError,
  createWarning,
  createInfo,
  errorCount,
  warningCount,
  infoCount,
  hasInfos,
  getInfos,
  clearErrors,
  clearWarnings,
  clearInfos,
  mergeHandlers,
  filterBySeverityForTests,
  sortBySeverity,
  renderErrors
) where

import Compiler.Errors.Core
import Compiler.Errors.Core hiding (ErrorHandler)
import qualified Data.Text as T
import qualified Data.List as List
import qualified Data.Ord as Ord

-- Type aliases for tests
type ErrorHandler = [TypeError]
type ErrorMessage = TypeError

-- Functions for tests
handleError :: ErrorHandler -> TypeError -> ErrorHandler
handleError errs err = err : errs

handleErrors :: ErrorHandler -> [TypeError] -> ErrorHandler
handleErrors errs newErrs = newErrs ++ errs

createError :: String -> T.Text -> ErrorLocation -> TypeError
createError errId msg loc = errorAt errId Error msg loc

createWarning :: String -> T.Text -> ErrorLocation -> TypeError
createWarning = warningAt

createInfo :: String -> T.Text -> ErrorLocation -> TypeError
createInfo = infoAt

errorCount :: ErrorHandler -> Int
errorCount = length . Compiler.Errors.Core.getErrors

warningCount :: ErrorHandler -> Int
warningCount = length . Compiler.Errors.Core.getWarnings

infoCount :: ErrorHandler -> Int
infoCount = length . Compiler.Errors.Core.getInfo

hasErrors :: ErrorHandler -> Bool
hasErrors = not . null . Compiler.Errors.Core.getErrors

hasWarnings :: ErrorHandler -> Bool
hasWarnings = not . null . Compiler.Errors.Core.getWarnings

hasInfos :: ErrorHandler -> Bool
hasInfos = not . null . Compiler.Errors.Core.getInfo

getErrors :: ErrorHandler -> [TypeError]
getErrors errs = Compiler.Errors.Core.getErrors errs

getWarnings :: ErrorHandler -> [TypeError]
getWarnings errs = Compiler.Errors.Core.getWarnings errs

getInfos :: ErrorHandler -> [TypeError]
getInfos errs = Compiler.Errors.Core.getInfo errs

clearErrors :: ErrorHandler -> ErrorHandler
clearErrors = filter (not . isError)
  where
    isError err = errorSeverity err == Error

clearWarnings :: ErrorHandler -> ErrorHandler
clearWarnings = filter (not . isWarning)
  where
    isWarning err = errorSeverity err == Warning

clearInfos :: ErrorHandler -> ErrorHandler
clearInfos = filter (not . isInfo)
  where
    isInfo err = errorSeverity err == Info

mergeHandlers :: ErrorHandler -> ErrorHandler -> ErrorHandler
mergeHandlers h1 h2 = h1 ++ h2

filterBySeverityForTests :: ErrorSeverity -> ErrorHandler -> ErrorHandler
filterBySeverityForTests severity = filter ((== severity) . errorSeverity)

sortBySeverity :: ErrorHandler -> ErrorHandler
sortBySeverity = List.sortBy compareSeverity
  where
    compareSeverity e1 e2 = Ord.compare (errorSeverity e1) (errorSeverity e2)

renderErrors :: ErrorHandler -> String
renderErrors = unlines . map (T.unpack . errorMessage) . Compiler.Errors.Core.getErrors
