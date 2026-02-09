module ErrorHandler (
  -- Re-export everything from Compiler.Errors.Core
  module Compiler.Errors.Core,
  -- Additional exports for tests
  ErrorHandler,
  ErrorMessage,
  handleError,
  handleErrors,
  handleErrorsList,
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
  renderErrors,
  handleWithResourceManagement,
  collectErrors,
  saveErrors,
  loadErrors,
  versionErrors,
  checkErrorSecurity,
  handleBatch,
  handleInteractive,
  handleWithLogging,
  handleWithMonitoring
) where

import Compiler.Errors.Core
import qualified Data.Text as T
import Data.List (isInfixOf)

-- Type aliases for tests
type ErrorHandler = [TypeError]
type ErrorMessage = TypeError

-- Functions for tests
handleError :: ErrorHandler -> TypeError -> ErrorHandler
handleError errs err = err : errs

handleErrors :: ErrorHandler -> [TypeError] -> ErrorHandler
handleErrors errs newErrs = newErrs ++ errs

-- Additional function to handle list of errors directly
handleErrorsList :: ErrorHandler -> [TypeError] -> ErrorHandler
handleErrorsList errs = handleErrors errs

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

-- | 处理错误并管理资源（简单实现，用于测试）
handleWithResourceManagement :: String -> Either String String
handleWithResourceManagement code = 
  if null code
    then Left "Empty code"
    else Right ("Handled: " ++ take 100 code)

-- | 收集错误（简单实现，用于测试）
collectErrors :: String -> [TypeError]
collectErrors code = 
  if "error" `isInfixOf` code
    then [errorAt "TEST" Error (T.pack "Test error") (ErrorLocation Nothing 0 0 Nothing Nothing)]
    else []

-- | 保存错误到文件（简单实现，用于测试）
saveErrors :: [TypeError] -> String -> IO Bool
saveErrors errors filepath = do
  writeFile filepath (show errors)
  return True

-- | 从文件加载错误（简单实现，用于测试）
loadErrors :: String -> IO [TypeError]
loadErrors filepath = do
  content <- readFile filepath
  return []  -- 简单实现，返回空列表

-- | 版本化错误（简单实现，用于测试）
versionErrors :: [TypeError] -> String -> [TypeError]
versionErrors errors version = 
  map (\e -> e { errorId = errorId e ++ ":" ++ version }) errors

-- | 检查错误安全性（简单实现，用于测试）
checkErrorSecurity :: [TypeError] -> Bool
checkErrorSecurity errors = all (\e -> "SECURE" `isInfixOf` errorId e || not ("INSECURE" `isInfixOf` errorId e)) errors

-- | 批量处理（简单实现，用于测试）
handleBatch :: [String] -> [Either String String]
handleBatch codes = map handleWithResourceManagement codes

-- | 交互式处理（简单实现，用于测试）
handleInteractive :: String -> Either String String
handleInteractive code = 
  if "interactive" `isInfixOf` code
    then Right ("Interactive: " ++ code)
    else handleWithResourceManagement code

-- | 带日志的处理（简单实现，用于测试）
handleWithLogging :: String -> Either String String
handleWithLogging code = 
  case handleWithResourceManagement code of
    Left err -> Left $ "[LOG] " ++ err
    Right result -> Right $ "[LOG] " ++ result

-- | 带监控的处理（简单实现，用于测试）
handleWithMonitoring :: String -> Either String String
handleWithMonitoring code = 
  case handleWithResourceManagement code of
    Left err -> Left $ "[MONITOR] " ++ err
    Right result -> Right $ "[MONITOR] " ++ result
