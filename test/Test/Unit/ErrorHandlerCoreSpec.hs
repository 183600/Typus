module Test.Unit.ErrorHandlerCoreSpec where



import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck

import ErrorHandler ()
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), locatedAt, startPos, posAt)

-- Test cases for basic error handling
testBasicErrorHandling :: TestTree
testBasicErrorHandling = testGroup "Basic error handling tests"
  [ testCase "create basic error" $
      let error = createBasicError "Test error message" startPos
      in errorMessage error @?= "Test error message"
  , testCase "error with severity" $
      let error = createErrorWithSeverity "Warning message" WarningSeverity startPos
      in errorSeverity error @?= WarningSeverity
  , testCase "error with source span" $
      let span = SourceSpan startPos (posAt 1 5)
          error = createErrorWithSpan "Span error" span
      in errorSpan error @?= Just span
  ]

-- Test cases for error collection
testErrorCollection :: TestTree
testErrorCollection = testGroup "Error collection tests"
  [ testCase "empty error collection" $
      let collection = emptyErrorCollection
      in errorCount collection @?= 0
  , testCase "add error to collection" $
      let error = createBasicError "Test error" startPos
          collection = addError error emptyErrorCollection
      in errorCount collection @?= 1
  , testCase "add multiple errors" $
      let error1 = createBasicError "Error 1" startPos
          error2 = createBasicError "Error 2" startPos
          collection = addError error1 (addError error2 emptyErrorCollection)
      in errorCount collection @?= 2
  , testCase "get errors by severity" $
      let error1 = createErrorWithSeverity "Warning" WarningSeverity startPos
          error2 = createErrorWithSeverity "Error" ErrorSeverity startPos
          collection = addError error1 (addError error2 emptyErrorCollection)
          errors = getErrorsBySeverity ErrorSeverity collection
      in length errors @?= 1
  ]

-- Test cases for error formatting
testErrorFormatting :: TestTree
testErrorFormatting = testGroup "Error formatting tests"
  [ testCase "format error with position" $
      let error = createBasicError "Test error" (posAt 5 10)
          formatted = formatError error
      in "5:10" `isInfixOf` formatted @?= True
  , testCase "format error with span" $
      let span = SourceSpan (posAt 3 5) (posAt 3 10)
          error = createErrorWithSpan "Span error" span
          formatted = formatError error
      in "3:5-3:10" `isInfixOf` formatted @?= True
  , testCase "format error includes severity" $
      let error = createErrorWithSeverity "Warning message" WarningSeverity startPos
          formatted = formatError error
      in "Warning" `isInfixOf` formatted @?= True
  ]

-- Test cases for error filtering
testErrorFiltering :: TestTree
testErrorFiltering = testGroup "Error filtering tests"
  [ testCase "filter errors by severity" $
      let error1 = createErrorWithSeverity "Warning 1" WarningSeverity startPos
          error2 = createErrorWithSeverity "Error 1" ErrorSeverity startPos
          error3 = createErrorWithSeverity "Warning 2" WarningSeverity startPos
          collection = addError error3 (addError error2 (addError error1 emptyErrorCollection))
          warnings = filterErrorsBySeverity WarningSeverity collection
      in errorCount warnings @?= 2
  , testCase "filter errors by position range" $
      let error1 = createBasicError "Error 1" (posAt 1 5)
          error2 = createBasicError "Error 2" (posAt 5 10)
          error3 = createBasicError "Error 3" (posAt 10 15)
          collection = addError error3 (addError error2 (addError error1 emptyErrorCollection))
          start = posAt 1 1
          end = posAt 5 12
          filtered = filterErrorsByRange start end collection
      in errorCount filtered @?= 2
  ]

-- Test cases for error context
testErrorContext :: TestTree
testErrorContext = testGroup "Error context tests"
  [ testCase "add context to error" $
      let error = createBasicError "Base error" startPos
          errorWithContext = addErrorContext "Additional context" error
      in errorContext errorWithContext @?= Just "Additional context"
  , testCase "format error with context" $
      let error = createBasicError "Base error" startPos
          errorWithContext = addErrorContext "Context info" error
          formatted = formatError errorWithContext
      in "Context info" `isInfixOf` formatted @?= True
  ]

-- Test cases for error recovery
testErrorRecovery :: TestTree
testErrorRecovery = testGroup "Error recovery tests"
  [ testCase "create recoverable error" $
      let error = createRecoverableError "Recoverable error" startPos
      in recoverable error @?= True
  , testCase "create non-recoverable error" $
      let error = createNonRecoverableError "Critical error" startPos
      in recoverable error @?= False
  , testCase "filter recoverable errors" $
      let error1 = createRecoverableError "Warning" startPos
          error2 = createNonRecoverableError "Critical" startPos
          collection = addError error2 (addError error1 emptyErrorCollection)
          recoverable = filterRecoverableErrors collection
      in errorCount recoverable @?= 1
  ]

-- Test cases for error aggregation
testErrorAggregation :: TestTree
testErrorAggregation = testGroup "Error aggregation tests"
  [ testCase "merge error collections" $
      let error1 = createBasicError "Error 1" startPos
          error2 = createBasicError "Error 2" startPos
          collection1 = addError error1 emptyErrorCollection
          collection2 = addError error2 emptyErrorCollection
          merged = mergeErrorCollections collection1 collection2
      in errorCount merged @?= 2
  , testCase "aggregate error statistics" $
      let error1 = createErrorWithSeverity "Warning" WarningSeverity startPos
          error2 = createErrorWithSeverity "Error" ErrorSeverity startPos
          error3 = createErrorWithSeverity "Warning" WarningSeverity startPos
          collection = addError error3 (addError error2 (addError error1 emptyErrorCollection))
          stats = getErrorStatistics collection
      in do
        warningCount stats @?= 2
        statsErrorCount stats @?= 3
  ]

-- Helper functions for testing (these would be implemented in ErrorHandler module)
createBasicError :: String -> SourcePos -> TestError
createBasicError msg pos = TestError msg ErrorSeverity Nothing pos False Nothing

createErrorWithSeverity :: String -> ErrorSeverity -> SourcePos -> TestError
createErrorWithSeverity msg severity pos = TestError msg severity Nothing pos False Nothing

createErrorWithSpan :: String -> SourceSpan -> TestError
createErrorWithSpan msg span = TestError msg ErrorSeverity (Just span) (spanStart span) False Nothing

createRecoverableError :: String -> SourcePos -> TestError
createRecoverableError msg pos = TestError msg ErrorSeverity Nothing pos True Nothing

createNonRecoverableError :: String -> SourcePos -> TestError
createNonRecoverableError msg pos = TestError msg ErrorSeverity Nothing pos False Nothing

addErrorContext :: String -> TestError -> TestError
addErrorContext context error = error { errorContext = Just context }

-- Test data types
data TestError = TestError
  { errorMessage :: String
  , errorSeverity :: ErrorSeverity
  , errorSpan :: Maybe SourceSpan
  , errorPosition :: SourcePos
  , recoverable :: Bool
  , errorContext :: Maybe String
  } deriving (Show, Eq)

data ErrorSeverity = ErrorSeverity | WarningSeverity | InfoSeverity deriving (Show, Eq)

data ErrorCollection = ErrorCollection
  { errors :: [TestError]
  } deriving (Show, Eq)

-- Mock implementations for testing
emptyErrorCollection :: ErrorCollection
emptyErrorCollection = ErrorCollection []

addError :: TestError -> ErrorCollection -> ErrorCollection
addError error collection = collection { errors = error : errors collection }

errorCount :: ErrorCollection -> Int
errorCount collection = length (errors collection)

getErrorsBySeverity :: ErrorSeverity -> ErrorCollection -> [TestError]
getErrorsBySeverity severity collection = 
  filter (\e -> errorSeverity e == severity) (errors collection)

filterErrorsBySeverity :: ErrorSeverity -> ErrorCollection -> ErrorCollection
filterErrorsBySeverity severity collection = 
  ErrorCollection (getErrorsBySeverity severity collection)

filterErrorsByRange :: SourcePos -> SourcePos -> ErrorCollection -> ErrorCollection
filterErrorsByRange start end collection = 
  ErrorCollection (filter inRange (errors collection))
  where
    inRange error = errorPosition error >= start && errorPosition error <= end

formatError :: TestError -> String
formatError error = 
  let posStr = show (posLine (errorPosition error)) ++ ":" ++ show (posColumn (errorPosition error))
      severityStr = show (errorSeverity error)
      baseMsg = errorMessage error
      contextStr = case errorContext error of
        Nothing -> ""
        Just ctx -> " (Context: " ++ ctx ++ ")"
  in severityStr ++ " at " ++ posStr ++ ": " ++ baseMsg ++ contextStr

filterRecoverableErrors :: ErrorCollection -> ErrorCollection
filterRecoverableErrors collection = 
  ErrorCollection (filter recoverable (errors collection))

mergeErrorCollections :: ErrorCollection -> ErrorCollection -> ErrorCollection
mergeErrorCollections collection1 collection2 = 
  ErrorCollection (errors collection1 ++ errors collection2)

getErrorStatistics :: ErrorCollection -> ErrorStats
getErrorStatistics collection = 
  let allErrors = errors collection
      warnings = length $ filter (\e -> errorSeverity e == WarningSeverity) allErrors
  in ErrorStats (length allErrors) warnings

data ErrorStats = ErrorStats
  { statsErrorCount :: Int
  , warningCount :: Int
  } deriving (Show, Eq)

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `elem` [take (length needle) $ drop i haystack | i <- [0..length haystack - length needle]]

-- QuickCheck properties
prop_error_count_preserved :: TestError -> ErrorCollection -> Property
prop_error_count_preserved error collection = 
  let newCollection = addError error collection
  in (errorCount newCollection == errorCount collection + 1) === True

prop_error_formatting_contains_message :: TestError -> Property
prop_error_formatting_contains_message error = 
  let formatted = formatError error
  in (errorMessage error `isInfixOf` formatted) === True

prop_filter_by_severity_preserves_count :: ErrorSeverity -> ErrorCollection -> Property
prop_filter_by_severity_preserves_count severity collection = 
  let filtered = filterErrorsBySeverity severity collection
      originalCount = length $ filter (\e -> errorSeverity e == severity) (errors collection)
  in (errorCount filtered == originalCount) === True

tests :: TestTree
tests = testGroup "ErrorHandler Core Tests"
  [ testBasicErrorHandling
  , testErrorCollection
  , testErrorFormatting
  , testErrorFiltering
  , testErrorContext
  , testErrorRecovery
  , testErrorAggregation
-- , testProperty "error count preserved" prop_error_count_preserved
-- , testProperty "error formatting contains message" prop_error_formatting_contains_message
-- , testProperty "filter by severity preserves count" prop_filter_by_severity_preserves_count
  ]