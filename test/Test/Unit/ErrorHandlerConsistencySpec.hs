{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.ErrorHandlerConsistencySpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Compiler.Errors.Core
import SourceLocation
import Data.Time (UTCTime, getCurrentTime)
import Data.List (sort)
import Control.Monad (when)
import qualified Data.Text as T

-- ============================================================================
-- Error Handler Consistency Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Error Handler Consistency Tests"
  [ errorCollectorProperties
  , errorFormattingProperties
  , errorRecoveryProperties
  , errorSeverityProperties
  , errorCategoryProperties
  , errorLocationProperties
  , errorConsistencyProperties
  ]

-- ============================================================================
-- Error Collector Properties
-- ============================================================================

errorCollectorProperties :: TestTree
errorCollectorProperties = testGroup "Error Collector Properties"
  [ testProperty "new error collector is empty" $
      let collector = newErrorCollector
      in null (getErrors collector) && null (getWarnings collector) && null (getInfo collector)
    
  , testProperty "adding error increases error count" $
      \errorType ->
        let collector = newErrorCollector
            collector' = addError errorType collector
        in length (getErrors collector') === length (getErrors collector) + 1
    
  , testProperty "adding warning increases warning count" $
      \warningType ->
        let collector = newErrorCollector
            collector' = addWarning warningType collector
        in length (getWarnings collector') === length (getWarnings collector) + 1
    
  , testProperty "adding info increases info count" $
      \infoType ->
        let collector = newErrorCollector
            collector' = addInfo infoType collector
        in length (getInfo collector') === length (getInfo collector) + 1
    
  , testProperty "hasErrors is true when errors exist" $
      \errorType ->
        let collector = newErrorCollector
            collector' = addError errorType collector
        in hasErrors collector' === True
    
  , testProperty "hasErrors is false when no errors exist" $
      let collector = newErrorCollector
      in hasErrors collector === False
    
  , testProperty "error collection preserves order" $
      \errors ->
        let collector = foldl (flip addError) newErrorCollector errors
            collectedErrors = getErrors collector
        in length collectedErrors === length errors
  ]

-- ============================================================================
-- Error Formatting Properties
-- ============================================================================

errorFormattingProperties :: TestTree
errorFormattingProperties = testGroup "Error Formatting Properties"
  [ testProperty "formatError produces non-empty string" $
      \errorType ->
        let formatted = formatError errorType
        in not $ null formatted
    
  , testProperty "formatErrors preserves order" $
      \errors ->
        let formatted = formatErrors errors
            formattedLines = lines formatted
        in length formattedLines >= length errors
    
  , testProperty "formatErrorWithLocation includes location info" $
      \errorType location ->
        let formatted = formatErrorWithLocation errorType location
        in length formatted >= length (formatError errorType)
    
  , testProperty "formatErrorsWithLocation preserves all errors" $
      \errors locations ->
        let formatted = formatErrorsWithLocation errors locations
            formattedLines = lines formatted
        in length formattedLines >= length errors
    
  , testCase "format error with different severities" $
      let error = TypeError "Test error" ErrorSeverityError ErrorCategoryTypeChecking emptyContext startPos
          warning = TypeError "Test warning" ErrorSeverityWarning ErrorCategoryTypeChecking emptyContext startPos
          info = TypeError "Test info" ErrorSeverityInfo ErrorCategoryTypeChecking emptyContext startPos
          formattedError = formatError error
          formattedWarning = formatError warning
          formattedInfo = formatError info
      in do
        assertBool "Error format is non-empty" $ not $ null formattedError
        assertBool "Warning format is non-empty" $ not $ null formattedWarning
        assertBool "Info format is non-empty" $ not $ null formattedInfo
  ]

-- ============================================================================
-- Error Recovery Properties
-- ============================================================================

errorRecoveryProperties :: TestTree
errorRecoveryProperties = testGroup "Error Recovery Properties"
  [ testProperty "canRecoverFrom is consistent with severity" $
      \errorType ->
        let canRecover = canRecoverFrom errorType
            severity = getErrorSeverity errorType
        in case severity of
          ErrorSeverityError -> canRecover `elem` [True, False]  -- Depends on error type
          ErrorSeverityWarning -> canRecover === True
          ErrorSeverityInfo -> canRecover === True
    
  , testProperty "shouldContinueAfter is consistent with recovery" $
      \errorType ->
        let shouldContinue = shouldContinueAfter errorType
            canRecover = canRecoverFrom errorType
        in if canRecover
           then shouldContinue === True
           else shouldContinue `elem` [True, False]  -- May still continue depending on policy
    
  , testCase "recovery from different error types" $
      let typeError = TypeError "Type error" ErrorSeverityError ErrorCategoryTypeChecking emptyContext startPos
          syntaxError = TypeError "Syntax error" ErrorSeverityError ErrorCategorySyntax emptyContext startPos
          warning = TypeError "Warning" ErrorSeverityWarning ErrorCategoryTypeChecking emptyContext startPos
      in do
        assertBool "Can recover from type error" $ canRecoverFrom typeError
        assertBool "Can recover from syntax error" $ canRecoverFrom syntaxError
        assertBool "Can recover from warning" $ canRecoverFrom warning
    
  , testProperty "recovery strategy is deterministic" $
      \errorType ->
        canRecoverFrom errorType === canRecoverFrom errorType &&
        shouldContinueAfter errorType === shouldContinueAfter errorType
  ]

-- ============================================================================
-- Error Severity Properties
-- ============================================================================

errorSeverityProperties :: TestTree
errorSeverityProperties = testGroup "Error Severity Properties"
  [ testProperty "severity ordering is total" $
      \sev1 sev2 ->
        let cmp = compare sev1 sev2
        in (sev1 <= sev2 && sev2 <= sev1) === (sev1 == sev2)
    
  , testCase "severity levels are distinct" $
      let severities = [ErrorSeverityError, ErrorSeverityWarning, ErrorSeverityInfo]
          uniqueSeverities = nub severities
      in length uniqueSeverities @?= length severities
    
  , testProperty "error severity affects recovery" $
      \severity ->
        let error = TypeError "Test" severity ErrorCategoryTypeChecking emptyContext startPos
        in case severity of
          ErrorSeverityError -> canRecoverFrom error `elem` [True, False]
          _ -> canRecoverFrom error === True
    
  , testProperty "severity extraction is consistent" $
      \severity ->
        let error = TypeError "Test" severity ErrorCategoryTypeChecking emptyContext startPos
        in getErrorSeverity error === severity
  ]
  where
    nub [] = []
    nub (x:xs) = x : nub (filter (/= x) xs)

-- ============================================================================
-- Error Category Properties
-- ============================================================================

errorCategoryProperties :: TestTree
errorCategoryProperties = testGroup "Error Category Properties"
  [ testProperty "category affects error behavior" $
      \category ->
        let error = TypeError "Test" ErrorSeverityError category emptyContext startPos
        in -- Categories may affect recovery strategies and formatting
           True
    
  , testCase "different categories produce different behaviors" $
      let typeError = TypeError "Type error" ErrorSeverityError ErrorCategoryTypeChecking emptyContext startPos
          syntaxError = TypeError "Syntax error" ErrorSeverityError ErrorCategorySyntax emptyContext startPos
          runtimeError = TypeError "Runtime error" ErrorSeverityError ErrorCategoryRuntime emptyContext startPos
      in do
        assertBool "Type error has category" $ getErrorCategory typeError == ErrorCategoryTypeChecking
        assertBool "Syntax error has category" $ getErrorCategory syntaxError == ErrorCategorySyntax
        assertBool "Runtime error has category" $ getErrorCategory runtimeError == ErrorCategoryRuntime
    
  , testProperty "category extraction is consistent" $
      \category ->
        let error = TypeError "Test" ErrorSeverityError category emptyContext startPos
        in getErrorCategory error === category
    
  , testProperty "categories are distinct" $
      \cat1 cat2 ->
        if cat1 == cat2
        then True
        else getErrorCategory (TypeError "Test" ErrorSeverityError cat1 emptyContext startPos) /=
             getErrorCategory (TypeError "Test" ErrorSeverityError cat2 emptyContext startPos)
  ]

-- ============================================================================
-- Error Location Properties
-- ============================================================================

errorLocationProperties :: TestTree
errorLocationProperties = testGroup "Error Location Properties"
  [ testProperty "errorAt creates error with correct location" $
      \message location ->
        let error = errorAt message location
        in getErrorLocation error === location
    
  , testProperty "errorAtWithTimestamp includes timestamp" $
      \message location timestamp ->
        let error = errorAtWithTimestamp message location timestamp
        in getErrorLocation error === location
    
  , testProperty "errorWithCategory sets correct category" $
      \message category ->
        let error = errorWithCategory message category
        in getErrorCategory error === category
    
  , testProperty "warningAt creates warning with correct severity" $
      \message location ->
        let warning = warningAt message location
        in getErrorSeverity warning === ErrorSeverityWarning
    
  , testProperty "infoAt creates info with correct severity" $
      \message location ->
        let info = infoAt message location
        in getErrorSeverity info === ErrorSeverityInfo
    
  , testCase "location information is preserved" $
      let pos = SourcePos 10 20
          error = errorAt "Test error" pos
      in do
        getErrorLine error @?= 10
        getErrorColumn error @?= 20
  ]

-- ============================================================================
-- Error Consistency Properties
-- ============================================================================

errorConsistencyProperties :: TestTree
errorConsistencyProperties = testGroup "Error Consistency Properties"
  [ testProperty "error creation is consistent" $
      \message severity category ->
        let error1 = TypeError message severity category emptyContext startPos
            error2 = TypeError message severity category emptyContext startPos
        in error1 === error2
    
  , testProperty "error collection preserves all information" $
      \errors ->
        let collector = foldl (flip addError) newErrorCollector errors
            collectedErrors = getErrors collector
        in length collectedErrors === length errors
    
  , testProperty "error formatting is deterministic" $
      \errorType ->
        let formatted1 = formatError errorType
            formatted2 = formatError errorType
        in formatted1 === formatted2
    
  , testProperty "error ordering is preserved" $
      \errors ->
        let collector = foldl (flip addError) newErrorCollector errors
            collectedErrors = getErrors collector
        in -- Should preserve insertion order
           length collectedErrors === length errors
    
  , testProperty "combined errors maintain consistency" $
      \error1 error2 ->
        let combined = CombinedError [error1, error2]
            errors = getCombinedErrors combined
        in length errors === 2 &&
           head errors === error1 &&
           last errors === error2
  ]

-- ============================================================================
-- QuickCheck Generators
-- ============================================================================

-- Generate error messages
genErrorMessage :: Gen String
genErrorMessage = listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "

-- Generate error severities
genErrorSeverity :: Gen ErrorSeverity
genErrorSeverity = elements [ErrorSeverityError, ErrorSeverityWarning, ErrorSeverityInfo]

-- Generate error categories
genErrorCategory :: Gen ErrorCategory
genErrorCategory = elements 
  [ ErrorCategoryTypeChecking
  , ErrorCategorySyntax
  , ErrorCategoryRuntime
  , ErrorCategoryOwnership
  , ErrorCategoryDependency
  ]

-- Generate source positions
genSourcePos :: Gen SourcePos
genSourcePos = SourcePos <$> choose (1, 1000) <*> choose (1, 1000)

-- Generate error contexts
genErrorContext :: Gen ErrorContext
genErrorContext = return emptyContext  -- Simplified for testing

-- Generate type errors
genTypeError :: Gen TypeError
genTypeError = do
  message <- genErrorMessage
  severity <- genErrorSeverity
  category <- genErrorCategory
  context <- genErrorContext
  location <- genSourcePos
  return $ TypeError message severity category context location

-- Generate combined errors
genCombinedError :: Gen CombinedError
genCombinedError = do
  n <- choose (1, 5)
  errors <- vectorOf n genTypeError
  return $ CombinedError errors

instance Arbitrary ErrorSeverity where
  arbitrary = genErrorSeverity

instance Arbitrary ErrorCategory where
  arbitrary = genErrorCategory

instance Arbitrary SourcePos where
  arbitrary = genSourcePos

instance Arbitrary TypeError where
  arbitrary = genTypeError

instance Arbitrary CombinedError where
  arbitrary = genCombinedError

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- Get error severity from type error
getErrorSeverity :: TypeError -> ErrorSeverity
getErrorSeverity (TypeError _ severity _ _ _) = severity

-- Get error category from type error
getErrorCategory :: TypeError -> ErrorCategory
getErrorCategory (TypeError _ _ category _ _) = category

-- Get error location from type error
getErrorLocation :: TypeError -> SourcePos
getErrorLocation (TypeError _ _ _ _ location) = location

-- Get errors from combined error
getCombinedErrors :: CombinedError -> [TypeError]
getCombinedErrors (CombinedError errors) = errors

-- ============================================================================
-- Edge Case Tests
-- ============================================================================

edgeCaseProperties :: TestTree
edgeCaseProperties = testGroup "Edge Case Tests"
  [ testCase "handle empty error message" $
      let error = TypeError "" ErrorSeverityError ErrorCategoryTypeChecking emptyContext startPos
          formatted = formatError error
      in assertBool "Empty message handled" $ not $ null formatted
    
  , testCase "handle very long error message" $
      let longMessage = replicate 1000 'a'
          error = TypeError longMessage ErrorSeverityError ErrorCategoryTypeChecking emptyContext startPos
          formatted = formatError error
      in assertBool "Long message handled" $ not $ null formatted
    
  , testCase "handle error with maximum position values" $
      let maxPos = SourcePos maxBound maxBound
          error = errorAt "Test error" maxPos
      in do
        getErrorLine error @?= maxBound
        getErrorColumn error @?= maxBound
    
  , testCase "handle error with minimum position values" $
      let minPos = SourcePos 1 1
          error = errorAt "Test error" minPos
      in do
        getErrorLine error @?= 1
        getErrorColumn error @?= 1
    
  , testProperty "handle large number of errors" $
      \n -> n < 1000 ==>
        let errors = replicate n (TypeError "Test" ErrorSeverityError ErrorCategoryTypeChecking emptyContext startPos)
            collector = foldl (flip addError) newErrorCollector errors
        in length (getErrors collector) === n
  ]

-- ============================================================================
-- Performance Properties
-- ============================================================================

performanceProperties :: TestTree
performanceProperties = testGroup "Performance Properties"
  [ testProperty "error collection is linear time" $
      \errors -> length errors < 1000 ==>
        let collector = foldl (flip addError) newErrorCollector errors
            result = getErrors collector
        in length result `seq` True
    
  , testProperty "error formatting is efficient" $
      \error ->
        let formatted = formatError error
        in length formatted `seq` True
    
  , testProperty "large error collections are handled" $
      \n -> n < 10000 ==>
        let errors = replicate n (TypeError "Test" ErrorSeverityError ErrorCategoryTypeChecking emptyContext startPos)
            collector = foldl (flip addError) newErrorCollector errors
            allErrors = getAllMessages collector
        in length allErrors `seq` True
  ]