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
errorCount = length . filter (\e -> errorSeverity e == Error)

warningCount :: ErrorHandler -> Int
warningCount = length . filter (\e -> errorSeverity e == Warning)

infoCount :: ErrorHandler -> Int
infoCount = length . filter (\e -> errorSeverity e == Info)

hasInfos :: ErrorHandler -> Bool
hasInfos = not . null . Compiler.Errors.Core.getInfo

getInfos :: ErrorHandler -> ErrorHandler
getInfos = Compiler.Errors.Core.getInfo

clearErrors :: ErrorHandler -> ErrorHandler
clearErrors = filter (\e -> errorSeverity e /= Error)

clearWarnings :: ErrorHandler -> ErrorHandler
clearWarnings = filter (\e -> errorSeverity e /= Warning)

clearInfos :: ErrorHandler -> ErrorHandler
clearInfos = filter (\e -> errorSeverity e /= Info)

mergeHandlers :: ErrorHandler -> ErrorHandler -> ErrorHandler
mergeHandlers h1 h2 = h1 ++ h2

filterBySeverityForTests :: ErrorSeverity -> ErrorHandler -> ErrorHandler
filterBySeverityForTests sev = filter (\e -> errorSeverity e == sev)

sortBySeverity :: ErrorHandler -> ErrorHandler
sortBySeverity = List.sortBy (\e1 e2 -> Ord.compare (errorSeverity e1) (errorSeverity e2))

renderErrors :: ErrorHandler -> String
renderErrors = unlines . map (T.unpack . errorMessage) . Compiler.Errors.Core.getErrors
