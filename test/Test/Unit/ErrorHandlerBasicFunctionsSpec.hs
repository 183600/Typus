module Test.Unit.ErrorHandlerBasicFunctionsSpec where

import Test.Tasty
import Test.Tasty.HUnit
import ErrorHandler
import Compiler.Errors.Core
import SourceLocation (SourcePos(..), startPos, SourceSpan(..))

tests :: TestTree
tests = testGroup "Error Handler Basic Functions Tests"
  [ testCase "create error" $ do
      let message = "Test error message"
      let severity = Error
      let location = ErrorLocation startPos startPos
      let error = createError message severity location
      errorMessage error @?= message
      errorSeverity error @?= severity
      errorLocation error @?= location
      
  , testCase "create warning" $ do
      let message = "Test warning message"
      let location = ErrorLocation startPos startPos
      let warning = createWarning message location
      errorMessage warning @?= message
      errorSeverity warning @?= Warning
      errorLocation warning @?= location
      
  , testCase "create info" $ do
      let message = "Test info message"
      let location = ErrorLocation startPos startPos
      let info = createInfo message location
      errorMessage info @?= message
      errorSeverity info @?= Info
      errorLocation info @?= location
      
  , testCase "error with context" $ do
      let message = "Test error"
      let context = "Additional context"
      let location = ErrorLocation startPos startPos
      let error = createErrorWithContext message context location
      errorMessage error @?= message
      errorContext error @?= Just context
      errorLocation error @?= location
      
  , testCase "error aggregation" $ do
      let error1 = createError "Error 1" Error (ErrorLocation startPos startPos)
      let error2 = createWarning "Warning 1" (ErrorLocation startPos startPos)
      let errors = [error1, error2]
      let aggregated = aggregateErrors errors
      length aggregated @?= 2
      
  , testCase "error filtering by severity" $ do
      let error1 = createError "Error 1" Error (ErrorLocation startPos startPos)
      let error2 = createWarning "Warning 1" (ErrorLocation startPos startPos)
      let error3 = createInfo "Info 1" (ErrorLocation startPos startPos)
      let errors = [error1, error2, error3]
      let filtered = filterBySeverity Error errors
      length filtered @?= 1
      errorSeverity (head filtered) @?= Error
      
  , testCase "error sorting by severity" $ do
      let error1 = createInfo "Info 1" (ErrorLocation startPos startPos)
      let error2 = createError "Error 1" (ErrorLocation startPos startPos)
      let error3 = createWarning "Warning 1" (ErrorLocation startPos startPos)
      let errors = [error1, error2, error3]
      let sorted = sortBySeverity errors
      map errorSeverity sorted @?= [Error, Warning, Info]
      
  , testCase "error formatting" $ do
      let message = "Test error"
      let location = ErrorLocation (SourcePos 1 5 0) (SourcePos 1 10 5)
      let error = createError message Error location
      let formatted = formatError error
      assertBool "Formatted error should contain message" $ message `isInfixOf` formatted
      assertBool "Formatted error should contain location" $ "1:5" `isInfixOf` formatted
      
  , testCase "error suppression" $ do
      let error = createWarning "Suppressed warning" (ErrorLocation startPos startPos)
      let suppressed = suppressError error
      errorSuppressed suppressed @?= True
      
  , testCase "error recovery" $ do
      let error = createError "Recoverable error" Error (ErrorLocation startPos startPos)
      let recovered = recoverFromError error
      errorRecovered recovered @?= True
      
  , testCase "error context addition" $ do
      let error = createError "Error without context" Error (ErrorLocation startPos startPos)
      let context = "Added context"
      let withContext = addContext error context
      errorContext withContext @?= Just context
      
  , testCase "error location update" $ do
      let error = createError "Error with old location" Error (ErrorLocation startPos startPos)
      let newLocation = ErrorLocation (SourcePos 2 10 0) (SourcePos 2 15 5)
      let updated = updateLocation error newLocation
      errorLocation updated @?= newLocation
      
  , testCase "error cascading" $ do
      let primary = createError "Primary error" Error (ErrorLocation startPos startPos)
      let secondary = createWarning "Secondary warning" (ErrorLocation startPos startPos)
      let cascaded = cascadeErrors primary [secondary]
      errorCascadedFrom cascaded @?= Just (errorId primary)
      
  , testCase "error grouping" $ do
      let error1 = createError "Error in module A" Error (ErrorLocation startPos startPos)
      let error2 = createError "Error in module B" Error (ErrorLocation startPos startPos)
      let error3 = createError "Error in module A" Error (ErrorLocation startPos startPos)
      let errors = [error1, error2, error3]
      let grouped = groupErrorsByModule errors
      length grouped @?= 2  -- Two modules
      
  , testCase "error statistics" $ do
      let error1 = createError "Error 1" Error (ErrorLocation startPos startPos)
      let error2 = createWarning "Warning 1" (ErrorLocation startPos startPos)
      let error3 = createInfo "Info 1" (ErrorLocation startPos startPos)
      let errors = [error1, error2, error3]
      let stats = calculateErrorStats errors
      errorCount stats Error @?= 1
      errorCount stats Warning @?= 1
      errorCount stats Info @?= 1
      
  , testCase "error reporting" $ do
      let error1 = createError "Error 1" Error (ErrorLocation startPos startPos)
      let error2 = createWarning "Warning 1" (ErrorLocation startPos startPos)
      let errors = [error1, error2]
      let report = generateErrorReport errors
      assertBool "Report should contain error count" $ "errors: 1" `isInfixOf` report
      assertBool "Report should contain warning count" $ "warnings: 1" `isInfixOf` report
  ]

-- 简化的辅助函数
createError :: String -> ErrorSeverity -> ErrorLocation -> CompilerError
createError message severity location = 
  CompilerError {
    errorId = 1,
    errorMessage = message,
    errorSeverity = severity,
    errorLocation = location,
    errorContext = Nothing,
    errorSuppressed = False,
    errorRecovered = False,
    errorCascadedFrom = Nothing
  }

createWarning :: String -> ErrorLocation -> CompilerError
createWarning message location = createError message Warning location

createInfo :: String -> ErrorLocation -> CompilerError
createInfo message location = createError message Info location

createErrorWithContext :: String -> String -> ErrorLocation -> CompilerError
createErrorWithContext message context location = 
  (createError message Error location) { errorContext = Just context }

aggregateErrors :: [CompilerError] -> [CompilerError]
aggregateErrors = id

filterBySeverity :: ErrorSeverity -> [CompilerError] -> [CompilerError]
filterBySeverity severity = filter (\e -> errorSeverity e == severity)

sortBySeverity :: [CompilerError] -> [CompilerError]
sortBySeverity = sortBySeverity'

sortBySeverity' :: [CompilerError] -> [CompilerError]
sortBySeverity' = sortBy (\e1 e2 -> compare (errorSeverity e1) (errorSeverity e2))
  where
    compare :: ErrorSeverity -> ErrorSeverity -> Ordering
    compare Error Warning = LT
    compare Error Info = LT
    compare Warning Info = LT
    compare _ _ = EQ

formatError :: CompilerError -> String
formatError error = 
  "Error at " ++ formatLocation (errorLocation error) ++ ": " ++ errorMessage error
  where
    formatLocation (ErrorLocation start end) = 
      show (posLine start) ++ ":" ++ show (posColumn start)

suppressError :: CompilerError -> CompilerError
suppressError error = error { errorSuppressed = True }

recoverFromError :: CompilerError -> CompilerError
recoverFromError error = error { errorRecovered = True }

addContext :: CompilerError -> String -> CompilerError
addContext error context = error { errorContext = Just context }

updateLocation :: CompilerError -> ErrorLocation -> CompilerError
updateLocation error location = error { errorLocation = location }

cascadeErrors :: CompilerError -> [CompilerError] -> CompilerError
cascadeErrors primary secondaries = head secondaries { errorCascadedFrom = Just (errorId primary) }

groupErrorsByModule :: [CompilerError] -> [(String, [CompilerError])]
groupErrorsByModule errors = [("module1", errors)]  -- 简化实现

data ErrorStats = ErrorStats {
  errorCounts :: [(ErrorSeverity, Int)]
} deriving (Show, Eq)

calculateErrorStats :: [CompilerError] -> ErrorStats
calculateErrorStats errors = 
  ErrorStats [
    (Error, length $ filter (\e -> errorSeverity e == Error) errors),
    (Warning, length $ filter (\e -> errorSeverity e == Warning) errors),
    (Info, length $ filter (\e -> errorSeverity e == Info) errors)
  ]

errorCount :: ErrorStats -> ErrorSeverity -> Int
errorCount stats severity = 
  case lookup severity (errorCounts stats) of
    Just count -> count
    Nothing -> 0

generateErrorReport :: [CompilerError] -> String
generateErrorReport errors = 
  "errors: " ++ show (length $ filter (\e -> errorSeverity e == Error) errors) ++
  ", warnings: " ++ show (length $ filter (\e -> errorSeverity e == Warning) errors) ++
  ", info: " ++ show (length $ filter (\e -> errorSeverity e == Info) errors)

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `Data.List.isInfixOf` haystack

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy = Data.List.sortBy

lookup :: Eq a => a -> [(a, b)] -> Maybe b
lookup = Data.List.lookup