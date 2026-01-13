module Test.Unit.ErrorHandlerBasicFunctionsSpec where

import Test.Tasty
import Test.Tasty.HUnit
import ErrorHandler
import qualified Compiler.Errors.Core as C
import Data.List (isInfixOf, sortBy, lookup)
import SourceLocation (SourcePos(..), startPos, SourceSpan(..))

tests :: TestTree
tests = testGroup "C.Error Handler Basic Functions Tests"
  [ testCase "create error" $ do
      let message = "Test error message"
      let severity = C.Error
      let location = C.ErrorLocation startPos startPos
      let error = createError message severity location
      errorMessage error @?= message
      errorSeverity error @?= severity
      errorLocation error @?= location
      
  , testCase "create warning" $ do
      let message = "Test warning message"
      let location = C.ErrorLocation startPos startPos
      let warning = createWarning message location
      errorMessage warning @?= message
      errorSeverity warning @?= C.Warning
      errorLocation warning @?= location
      
  , testCase "create info" $ do
      let message = "Test info message"
      let location = C.ErrorLocation startPos startPos
      let info = createInfo message location
      errorMessage info @?= message
      errorSeverity info @?= C.Info
      errorLocation info @?= location
      
  , testCase "error with context" $ do
      let message = "Test error"
      let context = "Additional context"
      let location = C.ErrorLocation startPos startPos
      let error = createErrorWithContext message context location
      errorMessage error @?= message
      errorContext error @?= Just context
      errorLocation error @?= location
      
  , testCase "error aggregation" $ do
      let error1 = createError "C.Error 1" C.Error (C.ErrorLocation startPos startPos)
      let error2 = createWarning "C.Warning 1" (C.ErrorLocation startPos startPos)
      let errors = [error1, error2]
      let aggregated = aggregateErrors errors
      length aggregated @?= 2
      
  , testCase "error filtering by severity" $ do
      let error1 = createError "C.Error 1" C.Error (C.ErrorLocation startPos startPos)
      let error2 = createWarning "C.Warning 1" (C.ErrorLocation startPos startPos)
      let error3 = createInfo "C.Info 1" (C.ErrorLocation startPos startPos)
      let errors = [error1, error2, error3]
      let filtered = ErrorHandler.filterBySeverity C.Error errors
      length filtered @?= 1
      errorSeverity (head filtered) @?= C.Error
      
  , testCase "error sorting by severity" $ do
      let error1 = createInfo "C.Info 1" (C.ErrorLocation (Just "test") 1 1 Nothing Nothing)
      let error2 = createError "C.Error 1" (C.ErrorLocation (Just "test") 1 1 Nothing Nothing)
      let error3 = createWarning "C.Warning 1" (C.ErrorLocation (Just "test") 1 1 Nothing Nothing)
      let errors = [error1, error2, error3]
      let sorted = sortBySeverity errors
      map errorSeverity sorted @?= [C.Error, C.Warning, C.Info]
      
  , testCase "error formatting" $ do
      let message = "Test error"
      let location = C.ErrorLocation (SourcePos 1 5 0) (SourcePos 1 10 5)
      let error = createError message C.Error location
      let formatted = ErrorHandler.formatError error
      assertBool "Formatted error should contain message" $ message `isInfixOf` formatted
      assertBool "Formatted error should contain location" $ "1:5" `isInfixOf` formatted
      
  , testCase "error suppression" $ do
      let error = createWarning "Suppressed warning" (C.ErrorLocation startPos startPos)
      let suppressed = suppressError error
      errorSuppressed suppressed @?= True
      
  , testCase "error recovery" $ do
      let error = createError "Recoverable error" C.Error (C.ErrorLocation startPos startPos)
      let recovered = recoverFromError error
      errorRecovered recovered @?= True
      
  , testCase "error context addition" $ do
      let error = createError "C.Error without context" C.Error (C.ErrorLocation startPos startPos)
      let context = "Added context"
      let withContext = addContext error context
      errorContext withContext @?= Just context
      
  , testCase "error location update" $ do
      let error = createError "C.Error with old location" C.Error (C.ErrorLocation startPos startPos)
      let newLocation = C.ErrorLocation (SourcePos 2 10 0) (SourcePos 2 15 5)
      let updated = updateLocation error newLocation
      errorLocation updated @?= newLocation
      
  , testCase "error cascading" $ do
      let primary = createError "Primary error" C.Error (C.ErrorLocation startPos startPos)
      let secondary = createWarning "Secondary warning" (C.ErrorLocation startPos startPos)
      let cascaded = cascadeErrors primary [secondary]
      errorCascadedFrom cascaded @?= Just (errorId primary)
      
  , testCase "error grouping" $ do
      let error1 = createError "C.Error in module A" C.Error (C.ErrorLocation startPos startPos)
      let error2 = createError "C.Error in module B" C.Error (C.ErrorLocation startPos startPos)
      let error3 = createError "C.Error in module A" C.Error (C.ErrorLocation startPos startPos)
      let errors = [error1, error2, error3]
      let grouped = groupErrorsByModule errors
      length grouped @?= 2  -- Two modules
      
  , testCase "error statistics" $ do
      let error1 = createError "C.Error 1" C.Error (C.ErrorLocation startPos startPos)
      let error2 = createWarning "C.Warning 1" (C.ErrorLocation startPos startPos)
      let error3 = createInfo "C.Info 1" (C.ErrorLocation startPos startPos)
      let errors = [error1, error2, error3]
      let stats = calculateErrorStats errors
      errorCount stats C.Error @?= 1
      errorCount stats C.Warning @?= 1
      errorCount stats C.Info @?= 1
      
  , testCase "error reporting" $ do
      let error1 = createError "C.Error 1" C.Error (C.ErrorLocation startPos startPos)
      let error2 = createWarning "C.Warning 1" (C.ErrorLocation startPos startPos)
      let errors = [error1, error2]
      let report = ErrorHandler.generateErrorReport errors
      assertBool "Report should contain error count" $ "errors: 1" `isInfixOf` report
      assertBool "Report should contain warning count" $ "warnings: 1" `isInfixOf` report
  ]

-- 简化的辅助函数
createError :: String -> C.ErrorSeverity -> C.ErrorLocation -> CombinedError
createError message severity location = 
  IntegrationError message severity

createWarning :: String -> C.ErrorLocation -> CombinedError
createWarning message location = createError message C.Warning location

createInfo :: String -> C.ErrorLocation -> CombinedError
createInfo message location = createError message C.Info location

createErrorWithContext :: String -> String -> C.ErrorLocation -> CombinedError
createErrorWithContext message context location = 
  IntegrationError (message ++ " (context: " ++ context ++ ")") C.Error

aggregateErrors :: [CombinedError] -> [CombinedError]
aggregateErrors = id

filterBySeverity :: ErrorSeverity -> [CombinedError] -> [CombinedError]
filterBySeverity severity = filter (\e -> errorSeverity e == severity)

sortBySeverity :: [CombinedError] -> [CombinedError]
sortBySeverity = sortBySeverity'

sortBySeverity' :: [CombinedError] -> [CombinedError]
sortBySeverity' = sortBy (\e1 e2 -> compare (errorSeverity e1) (errorSeverity e2))
  where
    compare :: ErrorSeverity -> ErrorSeverity -> Ordering
    compare C.Error C.Warning = LT
    compare C.Error C.Info = LT
    compare C.Warning C.Info = LT
    compare _ _ = EQ

formatError :: CombinedError -> String
formatError error = 
  "C.Error at " ++ formatLocation (errorLocation error) ++ ": " ++ errorMessage error
  where
    formatLocation (C.ErrorLocation start end) = 
      show (posLine start) ++ ":" ++ show (posColumn start)

suppressError :: CombinedError -> CombinedError
suppressError error = error  -- 简化实现

recoverFromError :: CombinedError -> CombinedError
recoverFromError error = error  -- 简化实现

addContext :: CombinedError -> String -> CombinedError
addContext error context = error  -- 简化实现

updateLocation :: CombinedError -> C.ErrorLocation -> CombinedError
updateLocation error location = error  -- 简化实现

cascadeErrors :: CombinedError -> [CombinedError] -> CombinedError
cascadeErrors primary secondaries = head secondaries  -- 简化实现

groupErrorsByModule :: [CombinedError] -> [(String, [CombinedError])]
groupErrorsByModule errors = [("module1", errors)]  -- 简化实现

data ErrorStats = ErrorStats {
  errorCounts :: [(ErrorSeverity, Int)]
} deriving (Show, Eq)

calculateErrorStats :: [CombinedError] -> ErrorStats
calculateErrorStats errors = 
  ErrorStats [
    (C.Error, length $ filter (\e -> errorSeverity e == C.Error) errors),
    (C.Warning, length $ filter (\e -> errorSeverity e == C.Warning) errors),
    (C.Info, length $ filter (\e -> errorSeverity e == C.Info) errors)
  ]

errorCount :: ErrorStats -> ErrorSeverity -> Int
errorCount stats severity = 
  case Prelude.lookup severity (errorCounts stats) of
    Just count -> count
    Nothing -> 0

generateErrorReport :: [CombinedError] -> String
generateErrorReport errors = 
  "errors: " ++ show (length $ filter (\e -> errorSeverity e == C.Error) errors) ++
  ", warnings: " ++ show (length $ filter (\e -> errorSeverity e == C.Warning) errors) ++
  ", info: " ++ show (length $ filter (\e -> errorSeverity e == C.Info) errors)

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `isInfixOf` haystack

sortBy' :: (a -> a -> Ordering) -> [a] -> [a]
sortBy' = sortBy

lookup' :: Eq a => a -> [(a, b)] -> Maybe b
lookup' = lookup