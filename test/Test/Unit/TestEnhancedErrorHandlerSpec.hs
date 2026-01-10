{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Unit.TestEnhancedErrorHandlerSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import ErrorHandler
import SourceLocation
import Utils
import qualified Data.Text as T
import TestSupport.Arbitrary ()
import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime)

-- | Test suite for Enhanced Error Handler
testEnhancedErrorHandler :: TestTree
testEnhancedErrorHandler = testGroup "Enhanced Error Handler Tests"
  [ testCase "ErrorHandler: creates enhanced error with context" $
      let pos = posAt 5 10
          message = "Test error"
          context = ErrorContext 
            { contextFunction = "testFunction"
            , contextModule = "TestModule"
            , contextDescription = "Testing error context"
            }
          err = errorWithContext pos message context
          errContext = errorContext err
      in contextFunction errContext @?= "testFunction" &&
         contextModule errContext @?= "TestModule" &&
         contextDescription errContext @?= "Testing error context"
         
  , testCase "ErrorHandler: adds suggestions to error" $
      let pos = posAt 5 10
          message = "Test error"
          suggestions = ["Suggestion 1", "Suggestion 2"]
          err = errorWithSuggestions pos message suggestions
          errSuggestions = errorSuggestions err
      in errSuggestions @?= suggestions
      
  , testCase "ErrorHandler: adds related errors" $
      let pos = posAt 5 10
          message = "Main error"
          relatedErr = errorAt (posAt 6 1) "Related error"
          err = withRelatedErrors pos message [relatedErr]
          errRelated = errorRelated err
      in length errRelated @?= 1
      
  , testCase "ErrorHandler: adds timestamp to error" $
      let pos = posAt 5 10
          message = "Test error"
          timestamp = UTCTime (fromGregorian 2023 1 1) (secondsToDiffTime 0)
          err = errorWithUTCTimestamp pos message timestamp
          errTimestamp = errorTimestamp err
      in errTimestamp @?= Just timestamp
      
  , testCase "ErrorHandler: wraps error with additional context" $
      let pos = posAt 5 10
          message = "Inner error"
          wrapper = "Wrapper context"
          innerErr = errorAt pos message
          wrappedErr = wrapError wrapper innerErr
      in errorMessage wrappedErr @?= wrapper ++ ": " ++ message
      
  , testCase "ErrorHandler: combines multiple errors" $
      let pos1 = posAt 5 10
          pos2 = posAt 6 10
          err1 = errorAt pos1 "First error"
          err2 = errorAt pos2 "Second error"
          combinedErr = combineErrors err1 err2
          errors = combinedErrors combinedErr
      in length errors @?= 2
      
  , testCase "ErrorHandler: filters errors by severity" $
      let pos = posAt 5 10
          infoErr = infoAt pos "Info message"
          warningErr = warningAt pos "Warning message"
          errorErr = errorAt pos "Error message"
          errors = [infoErr, warningErr, errorErr]
          warningAndAbove = filterBySeverity Warning errors
      in length warningAndAbove @?= 2
      
  , testCase "ErrorHandler: filters errors by category" $
      let pos = posAt 5 10
          parseErr = errorWithCategory pos "Parse error" ParseError
          typeErr = errorWithCategory pos "Type error" TypeError
          runtimeErr = errorWithCategory pos "Runtime error" RuntimeError
          errors = [parseErr, typeErr, runtimeErr]
          typeErrors = filterByCategory TypeError errors
      in length typeErrors @?= 1
      
  , testCase "ErrorHandler: generates error statistics" $
      let pos = posAt 5 10
          infoErr = infoAt pos "Info message"
          warningErr = warningAt pos "Warning message"
          errorErr = errorAt pos "Error message"
          errors = [infoErr, warningErr, errorErr, errorErr]
          stats = getErrorStatistics errors
      in errorCount stats @?= 2 &&
         warningCount stats @?= 1 &&
         infoCount stats @?= 1
      
  , testCase "ErrorHandler: formats error with location" $
      let pos = posAt 5 10
          message = "Test error"
          err = errorAt pos message
          formatted = formatErrorWithLocation err
      in "5:10" `isInfixOf` formatted @?= True &&
         "Test error" `isInfixOf` formatted @?= True
         
  , testCase "ErrorHandler: formats multiple errors" $
      let pos1 = posAt 5 10
          pos2 = posAt 6 10
          err1 = errorAt pos1 "First error"
          err2 = errorAt pos2 "Second error"
          errors = [err1, err2]
          formatted = formatErrors errors
      in "First error" `isInfixOf` formatted @?= True &&
         "Second error" `isInfixOf` formatted @?= True
         
  , testCase "ErrorHandler: generates error report" $
      let pos = posAt 5 10
          err = errorAt pos "Test error"
          errors = [err]
          report = generateErrorReport errors
      in "Error Report" `isInfixOf` report @?= True &&
         "Total errors: 1" `isInfixOf` report @?= True
         
  , testCase "ErrorHandler: generates error report with timestamp" $
      let pos = posAt 5 10
          err = errorAt pos "Test error"
          errors = [err]
          timestamp = UTCTime (fromGregorian 2023 1 1) (secondsToDiffTime 0)
          report = generateErrorReportWithUTCTime errors timestamp
      in "2023-01-01" `isInfixOf` report @?= True &&
         "Total errors: 1" `isInfixOf` report @?= True
         
  , testCase "ErrorHandler: generates error report with suggestions" $
      let pos = posAt 5 10
          suggestions = ["Fix the syntax", "Check the imports"]
          err = errorWithSuggestions pos "Test error" suggestions
          errors = [err]
          report = generateErrorReport errors
      in "Suggestions:" `isInfixOf` report @?= True &&
         "Fix the syntax" `isInfixOf` report @?= True
         
  , testCase "ErrorHandler: handles error recovery strategies" $
      let pos = posAt 5 10
          err = errorAt pos "Test error"
          recovery = createRecoveryStrategy "Continue processing" Continue
          recoveredErr = applyRecoveryStrategy err recovery
      in recoveryStrategy recoveredErr @?= Continue
      
  , testCase "ErrorHandler: custom recovery strategy" $
      let pos = posAt 5 10
          err = errorAt pos "Test error"
          recovery = customRecovery "Custom action" (const True)
          recoveredErr = applyRecoveryStrategy err recovery
      in recoveryAction recoveredErr @?= "Custom action"
      
  , testCase "ErrorHandler: severity comparison" $
      let severityOrder = [Info, Warning, Error]
          sorted = sortBySeverity [Error, Info, Warning]
      in sorted @?= severityOrder
      
  , testCase "ErrorHandler: category priority" $
      let categories = [ParseError, TypeError, RuntimeError, Warning]
          priorities = map categoryPriority categories
      in priorities == zip categories [1,2,3,4] @?= True
      
  , testCase "ErrorHandler: contextual error messages" $
      let pos = posAt 5 10
          message = "Variable not found"
          context = ErrorContext 
            { contextFunction = "processData"
            , contextModule = "DataProcessor"
            , contextDescription = "Processing input data"
            }
          err = errorWithContext pos message context
          formatted = formatErrorWithLocation err
      in "DataProcessor.processData" `isInfixOf` formatted @?= True &&
         "Variable not found" `isInfixOf` formatted @?= True
         
  , testCase "ErrorHandler: error chaining" $
      let pos1 = posAt 5 10
          pos2 = posAt 6 10
          pos3 = posAt 7 10
          err1 = errorAt pos1 "Root cause"
          err2 = wrapError "Intermediate error" err1
          err3 = wrapError "Top level error" err2
          chain = getErrorChain err3
      in length chain @?= 3
         
  , testCase "ErrorHandler: error aggregation" $
      let pos = posAt 5 10
          errors = [
              errorAt pos "Error 1",
              errorAt pos "Error 2",
              errorAt (posAt 6 1) "Error 3",
              errorAt (posAt 7 1) "Error 4"
              ]
          aggregated = aggregateErrors errors
      in errorCount aggregated @?= 4 &&
         uniqueLocations aggregated @?= 3
         
  , testCase "ErrorHandler: error deduplication" $
      let pos = posAt 5 10
          err1 = errorAt pos "Duplicate error"
          err2 = errorAt pos "Duplicate error"
          err3 = errorAt (posAt 6 1) "Different error"
          errors = [err1, err2, err3]
          deduplicated = deduplicateErrors errors
      in length deduplicated @?= 2
         
  , testCase "ErrorHandler: error severity escalation" $
      let pos = posAt 5 10
          err = errorAt pos "Initial error"
          escalated = escalateErrorSeverity err Error
      in errorSeverity escalated @?= Error
         
  , testCase "ErrorHandler: error suppression" $
      let pos = posAt 5 10
          err = errorAt pos "Suppressed error"
          suppressed = suppressError err
      in errorSuppressed suppressed @?= True
         
  , testCase "ErrorHandler: error highlighting" $
      let pos = posAt 5 10
          message = "Error with highlighting"
          highlights = [ErrorHighlight "keyword" 5 8, ErrorHighlight "variable" 12 18]
          err = errorWithHighlights pos message highlights
          formatted = formatErrorWithHighlights err
      in "<highlight:keyword>" `isInfixOf` formatted @?= True &&
         "<highlight:variable>" `isInfixOf` formatted @?= True
         
  , testCase "ErrorHandler: error with code context" $
      let pos = posAt 5 10
          message = "Error in code"
          codeContext = CodeContext 
            { contextBefore = ["line 3", "line 4"]
            , contextLine = "line 5 with error"
            , contextAfter = ["line 6", "line 7"]
            , contextStartPos = posAt 3 1
            , contextEndPos = posAt 7 1
            }
          err = errorWithCodeContext pos message codeContext
          formatted = formatErrorWithCodeContext err
      in "line 4" `isInfixOf` formatted @?= True &&
         "line 5 with error" `isInfixOf` formatted @?= True &&
         "line 6" `isInfixOf` formatted @?= True
  ]

-- Helper functions
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `elem` (substrings haystack)
  where
    substrings s = [take i s | i <- [1..length s]]

-- Enhanced error types
data ErrorContext = ErrorContext
  { contextFunction :: String
  , contextModule :: String
  , contextDescription :: String
  } deriving (Eq, Show)

data ErrorCategory = ParseError | TypeError | RuntimeError | Warning | Info
  deriving (Eq, Show, Ord)

data ErrorSeverity = Info | Warning | Error
  deriving (Eq, Show, Ord)

data ErrorRecovery = Continue | Stop | Retry | Custom String
  deriving (Eq, Show)

data ErrorHighlight = ErrorHighlight String Int Int  -- Type, start, end
  deriving (Eq, Show)

data CodeContext = CodeContext
  { contextBefore :: [String]
  , contextLine :: String
  , contextAfter :: [String]
  , contextStartPos :: SourcePos
  , contextEndPos :: SourcePos
  } deriving (Eq, Show)

data EnhancedError = EnhancedError
  { errorMessage :: String
  , errorLocation :: ErrorLocation
  , errorContext :: Maybe ErrorContext
  , errorSuggestions :: [String]
  , errorRelated :: [EnhancedError]
  , errorTimestamp :: Maybe UTCTime
  , errorSeverity :: ErrorSeverity
  , errorCategory :: ErrorCategory
  , errorRecovery :: Maybe ErrorRecovery
  , errorHighlights :: [ErrorHighlight]
  , errorCodeContext :: Maybe CodeContext
  , errorSuppressed :: Bool
  } deriving (Eq, Show)

data ErrorStatistics = ErrorStatistics
  { errorCount :: Int
  , warningCount :: Int
  , infoCount :: Int
  , uniqueLocations :: Int
  } deriving (Eq, Show)

-- Enhanced error functions
errorWithContext :: SourcePos -> String -> ErrorContext -> EnhancedError
errorWithContext pos message ctx = EnhancedError
  { errorMessage = message
  , errorLocation = toErrorLocation pos
  , errorContext = Just ctx
  , errorSuggestions = []
  , errorRelated = []
  , errorTimestamp = Nothing
  , errorSeverity = Error
  , errorCategory = RuntimeError
  , errorRecovery = Nothing
  , errorHighlights = []
  , errorCodeContext = Nothing
  , errorSuppressed = False
  }

errorWithSuggestions :: SourcePos -> String -> [String] -> EnhancedError
errorWithSuggestions pos message suggestions = EnhancedError
  { errorMessage = message
  , errorLocation = toErrorLocation pos
  , errorContext = Nothing
  , errorSuggestions = suggestions
  , errorRelated = []
  , errorTimestamp = Nothing
  , errorSeverity = Error
  , errorCategory = RuntimeError
  , errorRecovery = Nothing
  , errorHighlights = []
  , errorCodeContext = Nothing
  , errorSuppressed = False
  }

errorWithCategory :: SourcePos -> String -> ErrorCategory -> EnhancedError
errorWithCategory pos message category = EnhancedError
  { errorMessage = message
  , errorLocation = toErrorLocation pos
  , errorContext = Nothing
  , errorSuggestions = []
  , errorRelated = []
  , errorTimestamp = Nothing
  , errorSeverity = Error
  , errorCategory = category
  , errorRecovery = Nothing
  , errorHighlights = []
  , errorCodeContext = Nothing
  , errorSuppressed = False
  }

errorWithUTCTimestamp :: SourcePos -> String -> UTCTime -> EnhancedError
errorWithUTCTimestamp pos message timestamp = EnhancedError
  { errorMessage = message
  , errorLocation = toErrorLocation pos
  , errorContext = Nothing
  , errorSuggestions = []
  , errorRelated = []
  , errorTimestamp = Just timestamp
  , errorSeverity = Error
  , errorCategory = RuntimeError
  , errorRecovery = Nothing
  , errorHighlights = []
  , errorCodeContext = Nothing
  , errorSuppressed = False
  }

withRelatedErrors :: SourcePos -> String -> [EnhancedError] -> EnhancedError
withRelatedErrors pos message related = EnhancedError
  { errorMessage = message
  , errorLocation = toErrorLocation pos
  , errorContext = Nothing
  , errorSuggestions = []
  , errorRelated = related
  , errorTimestamp = Nothing
  , errorSeverity = Error
  , errorCategory = RuntimeError
  , errorRecovery = Nothing
  , errorHighlights = []
  , errorCodeContext = Nothing
  , errorSuppressed = False
  }

errorWithHighlights :: SourcePos -> String -> [ErrorHighlight] -> EnhancedError
errorWithHighlights pos message highlights = EnhancedError
  { errorMessage = message
  , errorLocation = toErrorLocation pos
  , errorContext = Nothing
  , errorSuggestions = []
  , errorRelated = []
  , errorTimestamp = Nothing
  , errorSeverity = Error
  , errorCategory = RuntimeError
  , errorRecovery = Nothing
  , errorHighlights = highlights
  , errorCodeContext = Nothing
  , errorSuppressed = False
  }

errorWithCodeContext :: SourcePos -> String -> CodeContext -> EnhancedError
errorWithCodeContext pos message codeCtx = EnhancedError
  { errorMessage = message
  , errorLocation = toErrorLocation pos
  , errorContext = Nothing
  , errorSuggestions = []
  , errorRelated = []
  , errorTimestamp = Nothing
  , errorSeverity = Error
  , errorCategory = RuntimeError
  , errorRecovery = Nothing
  , errorHighlights = []
  , errorCodeContext = Just codeCtx
  , errorSuppressed = False
  }

-- Base error functions
errorAt :: SourcePos -> String -> EnhancedError
errorAt pos message = EnhancedError
  { errorMessage = message
  , errorLocation = toErrorLocation pos
  , errorContext = Nothing
  , errorSuggestions = []
  , errorRelated = []
  , errorTimestamp = Nothing
  , errorSeverity = Error
  , errorCategory = RuntimeError
  , errorRecovery = Nothing
  , errorHighlights = []
  , errorCodeContext = Nothing
  , errorSuppressed = False
  }

warningAt :: SourcePos -> String -> EnhancedError
warningAt pos message = (errorAt pos message) { errorSeverity = Warning, errorCategory = Warning }

infoAt :: SourcePos -> String -> EnhancedError
infoAt pos message = (errorAt pos message) { errorSeverity = Info, errorCategory = Info }

-- Error manipulation functions
wrapError :: String -> EnhancedError -> EnhancedError
wrapError wrapper err = err { errorMessage = wrapper ++ ": " ++ errorMessage err }

combineErrors :: EnhancedError -> EnhancedError -> EnhancedError
combineErrors err1 err2 = EnhancedError
  { errorMessage = errorMessage err1 ++ " and " ++ errorMessage err2
  , errorLocation = errorLocation err1
  , errorContext = errorContext err1
  , errorSuggestions = errorSuggestions err1 ++ errorSuggestions err2
  , errorRelated = [err1, err2]
  , errorTimestamp = errorTimestamp err1
  , errorSeverity = max (errorSeverity err1) (errorSeverity err2)
  , errorCategory = errorCategory err1
  , errorRecovery = errorRecovery err1
  , errorHighlights = errorHighlights err1 ++ errorHighlights err2
  , errorCodeContext = errorCodeContext err1
  , errorSuppressed = False
  }

filterBySeverity :: ErrorSeverity -> [EnhancedError] -> [EnhancedError]
filterBySeverity minSeverity = filter (\e -> errorSeverity e >= minSeverity)

filterByCategory :: ErrorCategory -> [EnhancedError] -> [EnhancedError]
filterByCategory category = filter (\e -> errorCategory e == category)

getErrorStatistics :: [EnhancedError] -> ErrorStatistics
getErrorStatistics errors = ErrorStatistics
  { errorCount = length $ filter (\e -> errorSeverity e == Error) errors
  , warningCount = length $ filter (\e -> errorSeverity e == Warning) errors
  , infoCount = length $ filter (\e -> errorSeverity e == Info) errors
  , uniqueLocations = length $ uniqueLocationsList errors
  }
  where
    uniqueLocationsList = nub $ map errorLocation errors

formatErrorWithLocation :: EnhancedError -> String
formatErrorWithLocation err = 
  "Error at " ++ show (line (errorLocation err)) ++ ":" ++ 
  show (column (errorLocation err)) ++ ": " ++ errorMessage err

formatErrors :: [EnhancedError] -> String
formatErrors errors = unlines $ map formatErrorWithLocation errors

generateErrorReport :: [EnhancedError] -> String
generateErrorReport errors = 
  "Error Report\n" ++
  "============\n" ++
  "Total errors: " ++ show (length errors) ++ "\n" ++
  unlines (map formatErrorWithLocation errors)

generateErrorReportWithUTCTime :: [EnhancedError] -> UTCTime -> String
generateErrorReportWithUTCTime errors timestamp = 
  "Error Report - " ++ show timestamp ++ "\n" ++
  "===============================\n" ++
  "Total errors: " ++ show (length errors) ++ "\n" ++
  unlines (map formatErrorWithLocation errors)

-- Recovery strategy functions
data RecoveryStrategy = RecoveryStrategy
  { recoveryAction :: String
  , recoveryType :: ErrorRecovery
  }

createRecoveryStrategy :: String -> ErrorRecovery -> RecoveryStrategy
createRecoveryStrategy action recoveryType = RecoveryStrategy action recoveryType

customRecovery :: String -> (EnhancedError -> Bool) -> RecoveryStrategy
customRecovery action _ = RecoveryStrategy action (Custom action)

applyRecoveryStrategy :: EnhancedError -> RecoveryStrategy -> EnhancedError
applyRecoveryStrategy err strategy = err { errorRecovery = Just (recoveryType strategy) }

-- Utility functions
sortBySeverity :: [ErrorSeverity] -> [ErrorSeverity]
sortBySeverity = sort

categoryPriority :: ErrorCategory -> Int
categoryPriority ParseError = 1
categoryPriority TypeError = 2
categoryPriority RuntimeError = 3
categoryPriority Warning = 4
categoryPriority Info = 5

getErrorChain :: EnhancedError -> [EnhancedError]
getErrorChain err = err : concatMap getErrorChain (errorRelated err)

aggregateErrors :: [EnhancedError] -> ErrorStatistics
aggregateErrors = getErrorStatistics

deduplicateErrors :: [EnhancedError] -> [EnhancedError]
deduplicateErrors = nubBy (\e1 e2 -> errorMessage e1 == errorMessage e2 && 
                                   errorLocation e1 == errorLocation e2)

escalateErrorSeverity :: EnhancedError -> ErrorSeverity -> EnhancedError
escalateErrorSeverity err severity = err { errorSeverity = severity }

suppressError :: EnhancedError -> EnhancedError
suppressError err = err { errorSuppressed = True }

formatErrorWithHighlights :: EnhancedError -> String
formatErrorWithHighlights err = 
  let base = formatErrorWithLocation err
      highlights = map formatHighlight (errorHighlights err)
  in base ++ "\n" ++ unlines highlights
  where
    formatHighlight (ErrorHighlight typ start end) = 
      "<highlight:" ++ typ ++ "> at position " ++ show start ++ "-" ++ show end

formatErrorWithCodeContext :: EnhancedError -> String
formatErrorWithCodeContext err = 
  let base = formatErrorWithLocation err
  in case errorCodeContext err of
       Nothing -> base
       Just ctx -> base ++ "\n" ++ formatCodeContext ctx
  where
    formatCodeContext ctx = 
      "Code context:\n" ++
      unlines (map ("  " ++) (contextBefore ctx)) ++
      "> " ++ contextLine ctx ++ "\n" ++
      unlines (map ("  " ++) (contextAfter ctx))

-- Helper functions
nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/= x) xs)

nubBy :: (a -> a -> Bool) -> [a] -> [a]
nubBy _ [] = []
nubBy eq (x:xs) = x : nubBy (filter (not . eq x)) xs

sort :: Ord a => [a] -> [a]
sort = foldr insert []
  where
    insert x [] = [x]
    insert x (y:ys) = if x <= y then x:y:ys else y:insert x ys

-- Simplified SourceLocation types for testing
data SourcePos = SourcePos 
  { posLine :: Int
  , posColumn :: Int
  } deriving (Eq, Show, Ord)

posAt :: Int -> Int -> SourcePos
posAt line column = SourcePos line column

data ErrorLocation = ErrorLocation 
  { line :: Int
  , column :: Int
  } deriving (Eq, Show, Ord)

toErrorLocation :: SourcePos -> ErrorLocation
toErrorLocation pos = ErrorLocation (posLine pos) (posColumn pos)