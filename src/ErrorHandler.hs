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
  renderErrors
) where

import Compiler.Errors.Core
import qualified Data.Text as T

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
errorCount = length . filter (\e -> severity e == Error)

warningCount :: ErrorHandler -> Int
warningCount = length . filter (\e -> severity e == Warning)

infoCount :: ErrorHandler -> Int
infoCount = length . filter (\e -> severity e == Info)

hasInfos :: ErrorHandler -> Bool
hasInfos = not . null . filter (\e -> severity e == Info)

getInfos :: ErrorHandler -> ErrorHandler
getInfos = filter (\e -> severity e == Info)

clearErrors :: ErrorHandler -> ErrorHandler
clearErrors = filter (\e -> severity e /= Error)

clearWarnings :: ErrorHandler -> ErrorHandler
clearWarnings = filter (\e -> severity e /= Warning)

clearInfos :: ErrorHandler -> ErrorHandler
clearInfos = filter (\e -> severity e /= Info)

mergeHandlers :: ErrorHandler -> ErrorHandler -> ErrorHandler
mergeHandlers h1 h2 = h1 ++ h2

filterBySeverityForTests :: ErrorSeverity -> ErrorHandler -> ErrorHandler
filterBySeverityForTests sev = filter (\e -> severity e == sev)

-- Use sortBySeverity from Compiler.Errors.Core

renderErrors :: ErrorHandler -> String
renderErrors = unlines . map (T.unpack . errorMessage) . Compiler.Errors.Core.getErrors
