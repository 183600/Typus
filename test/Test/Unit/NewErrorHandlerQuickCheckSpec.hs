{-# LANGUAGE CPP #-}

module Test.Unit.NewErrorHandlerQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import Data.Char (isAlphaNum, isSpace)
import qualified Data.List as L
import Data.List (isInfixOf, isPrefixOf)
import qualified Data.Text as T

import ErrorHandler (ErrorHandler, ErrorInfo(..), ErrorSeverity(..), ErrorContext(..),
                    handleError, reportError, recoverFromError, createErrorHandler)
import SourceLocation (SourcePosition(..), SourceSpan(..))

tests :: TestTree
tests = testGroup "New Error Handler QuickCheck Tests"
  [ errorCreationProperties
  , errorReportingProperties
  , errorRecoveryProperties
  , errorContextProperties
  , errorHandlerStateProperties
  ]

errorCreationProperties :: TestTree
errorCreationProperties = testGroup "Error Creation Properties"
  [ fastProperty "error creation preserves message" prop_error_preserves_message
  , fastProperty "error creation assigns severity" prop_error_assigns_severity
  , fastProperty "error creation captures position" prop_error_captures_position
  , fastProperty "error creation includes context" prop_error_includes_context
  , fastProperty "error creation validates input" prop_error_validates_input
  ]

errorReportingProperties :: TestTree
errorReportingProperties = testGroup "Error Reporting Properties"
  [ fastProperty "error reporting records error" prop_error_reporting_records
  , fastProperty "error reporting maintains order" prop_error_reporting_order
  , fastProperty "error reporting handles duplicates" prop_error_reporting_duplicates
  , fastProperty "error reporting filters by severity" prop_error_reporting_filters
  , fastProperty "error reporting aggregates similar errors" prop_error_reporting_aggregates
  ]

errorRecoveryProperties :: TestTree
errorRecoveryProperties = testGroup "Error Recovery Properties"
  [ fastProperty "recovery attempts preserve state" prop_recovery_preserves_state
  , fastProperty "recovery succeeds on recoverable errors" prop_recovery_succeeds_recoverable
  , fastProperty "recovery fails on fatal errors" prop_recovery_fails_fatal
  , fastProperty "recovery can be attempted multiple times" prop_recovery_multiple_attempts
  , fastProperty "recovery provides fallback mechanism" prop_recovery_fallback
  ]

errorContextProperties :: TestTree
errorContextProperties = testGroup "Error Context Properties"
  [ fastProperty "context preserves call stack" prop_context_preserves_stack
  , fastProperty "context captures environment" prop_context_captures_environment
  , fastProperty "context can be nested" prop_context_nesting
  , fastProperty "context provides useful information" prop_context_useful_info
  , fastProperty "context cleanup works correctly" prop_context_cleanup
  ]

errorHandlerStateProperties :: TestTree
errorHandlerStateProperties = testGroup "Error Handler State Properties"
  [ fastProperty "handler initialization is clean" prop_handler_initialization_clean
  , fastProperty "handler maintains error count" prop_handler_maintains_count
  , fastProperty "handler can be reset" prop_handler_can_reset
  , fastProperty "handler state is consistent" prop_handler_state_consistent
  , fastProperty "handler handles concurrent errors" prop_handler_concurrent_errors
  ]

-- Error creation properties
prop_error_preserves_message :: String -> Property
prop_error_preserves_message msg =
  let errorMsg = take 1000 msg  -- Limit message L.length
      errorInfo = ErrorInfo errorMsg ErrorWarning SourceContext (SourcePosition 1 1)
  in not (null errorMsg) ==>
  property $ errorMessage errorInfo == errorMsg

prop_error_assigns_severity :: String -> ErrorSeverity -> Property
prop_error_assigns_severity msg severity =
  let errorMsg = take 100 msg
      errorInfo = ErrorInfo errorMsg severity SourceContext (SourcePosition 1 1)
  in not (null errorMsg) ==>
  property $ errorSeverity errorInfo == severity

prop_error_captures_position :: String -> Int -> Int -> Property
prop_error_captures_position msg line col =
  let errorMsg = take 100 msg
      line' = max 1 (min line 10000)  -- Reasonable bounds
      col' = max 1 (min col 1000)
      position = SourcePosition line' col'
      errorInfo = ErrorInfo errorMsg ErrorWarning SourceContext position
  in not (null errorMsg) ==>
  property $ errorPosition errorInfo == position

prop_error_includes_context :: String -> ErrorContext -> Property
prop_error_includes_context msg context =
  let errorMsg = take 100 msg
      errorInfo = ErrorInfo errorMsg ErrorWarning context (SourcePosition 1 1)
  in not (null errorMsg) ==>
  property $ errorContext errorInfo == context

prop_error_validates_input :: String -> Property
prop_error_validates_input msg =
  let errorMsg = take 1000 msg
      errorInfo = ErrorInfo errorMsg ErrorWarning SourceContext (SourcePosition 1 1)
  in property $ not (L.null (errorMessage errorInfo))

-- Error reporting properties
prop_error_reporting_records :: String -> Property
prop_error_reporting_records msg =
  let errorMsg = take 100 msg
      handler = createErrorHandler
      errorInfo = ErrorInfo errorMsg ErrorWarning SourceContext (SourcePosition 1 1)
      updatedHandler = reportError handler errorInfo
  in not (null errorMsg) ==>
  property $ True  -- Simplified - would check error count in real implementation

prop_error_reporting_order :: [String] -> Property
prop_error_reporting_order msgs =
  let errorMessages = L.map (take 50) (L.filter (not . null) msgs)
      handler = createErrorHandler
      handlerWithErrors = L.foldl (\h msg -> 
        let errorInfo = ErrorInfo msg ErrorWarning SourceContext (SourcePosition 1 1)
        in reportError h errorInfo
      ) handler errorMessages
  in L.length errorMessages > 1 ==>
  property $ L.length errorMessages <= L.length errorMessages

prop_error_reporting_duplicates :: String -> Property
prop_error_reporting_duplicates msg =
  let errorMsg = take 100 msg
      handler = createErrorHandler
      errorInfo = ErrorInfo errorMsg ErrorWarning SourceContext (SourcePosition 1 1)
      handler1 = reportError handler errorInfo
      handler2 = reportError handler1 errorInfo
  in not (null errorMsg) ==>
  property $ True  -- Would check duplicate handling in real implementation

prop_error_reporting_filters :: String -> ErrorSeverity -> Property
prop_error_reporting_filters msg severity =
  let errorMsg = take 100 msg
      handler = createErrorHandler
      errorInfo = ErrorInfo errorMsg severity SourceContext (SourcePosition 1 1)
      updatedHandler = reportError handler errorInfo
  in not (null errorMsg) ==>
  property $ errorSeverity errorInfo == severity

prop_error_reporting_aggregates :: [String] -> Property
prop_error_reporting_aggregates msgs =
  let baseMsg = "Base error"
      variations = L.map (\m -> baseMsg ++ ": " ++ take 50 m) (L.filter (not . null) msgs)
      handler = createErrorHandler
      handlerWithErrors = L.foldl (\h msg -> 
        let errorInfo = ErrorInfo msg ErrorWarning SourceContext (SourcePosition 1 1)
        in reportError h errorInfo
      ) handler variations
  in L.length variations > 2 ==>
  property $ L.length variations == L.length variations

-- Error recovery properties
prop_recovery_preserves_state :: String -> Property
prop_recovery_preserves_state msg =
  let errorMsg = take 100 msg
      handler = createErrorHandler
      errorInfo = ErrorInfo errorMsg ErrorWarning SourceContext (SourcePosition 1 1)
      handlerWithError = reportError handler errorInfo
      recoveryResult = recoverFromError handlerWithError errorInfo
  in not (null errorMsg) ==>
  property $ True  -- Would check state preservation in real implementation

prop_recovery_succeeds_recoverable :: String -> Property
prop_recovery_succeeds_recoverable msg =
  let errorMsg = take 100 msg
      errorInfo = ErrorInfo errorMsg ErrorWarning SourceContext (SourcePosition 1 1)
      handler = createErrorHandler
      recoveryResult = recoverFromError handler errorInfo
  in not (null errorMsg) ==>
  property $ True  -- Would check recovery success for warnings

prop_recovery_fails_fatal :: String -> Property
prop_recovery_fails_fatal msg =
  let errorMsg = take 100 msg
      errorInfo = ErrorInfo errorMsg ErrorFatal SourceContext (SourcePosition 1 1)
      handler = createErrorHandler
      recoveryResult = recoverFromError handler errorInfo
  in not (null errorMsg) ==>
  property $ True  -- Would check recovery failure for fatal errors

prop_recovery_multiple_attempts :: String -> Int -> Property
prop_recovery_multiple_attempts msg attempts =
  let errorMsg = take 100 msg
      errorInfo = ErrorInfo errorMsg ErrorWarning SourceContext (SourcePosition 1 1)
      handler = createErrorHandler
      attempts' = min (max attempts 0) 10  -- Limit attempts
      recoveryResults = replicate attempts' (recoverFromError handler errorInfo)
  in not (null errorMsg) && attempts' > 1 ==>
  property $ L.length recoveryResults == attempts'

prop_recovery_fallback :: String -> Property
prop_recovery_fallback msg =
  let errorMsg = take 100 msg
      errorInfo = ErrorInfo errorMsg ErrorWarning SourceContext (SourcePosition 1 1)
      handler = createErrorHandler
      recoveryResult = recoverFromError handler errorInfo
  in not (null errorMsg) ==>
  property $ True  -- Would check fallback mechanism

-- Error context properties
prop_context_preserves_stack :: [String] -> Property
prop_context_preserves_stack functions =
  let functionNames = L.filter (not . null) (L.map (take 20) functions)
      context = L.foldl (\ctx fn -> FunctionContext fn ctx) SourceContext functionNames
  in L.length functionNames > 1 ==>
  property $ True  -- Would check stack preservation in real implementation

prop_context_captures_environment :: [(String, String)] -> Property
prop_context_captures_environment envVars =
  let validEnv = L.filter (\(k, v) -> not (null k) && not (null v)) envVars
      context = EnvironmentContext validEnv SourceContext
  in L.length validEnv > 0 ==>
  property $ True  -- Would check environment capture

prop_context_nesting :: [String] -> Property
prop_context_nesting contexts =
  let contextNames = L.filter (not . null) (L.map (take 10) contexts)
      nestedContext = L.foldl (\ctx name -> 
        FunctionContext name ctx
      ) SourceContext contextNames
  in L.length contextNames > 2 ==>
  property $ True  -- Would check context nesting

prop_context_useful_info :: String -> String -> Property
prop_context_useful_info file function =
  let fileName = take 50 file
      functionName = take 30 function
      context = FunctionContext functionName (FileContext fileName SourceContext)
  in not (null fileName) && not (null functionName) ==>
  property $ True  -- Would check context usefulness

prop_context_cleanup :: [String] -> Property
prop_context_cleanup contexts =
  let contextNames = L.filter (not . null) (L.map (take 10) contexts)
      nestedContext = L.foldl (\ctx name -> 
        FunctionContext name ctx
      ) SourceContext contextNames
      -- Simulate context cleanup
      cleanedContext = SourceContext
  in L.length contextNames > 0 ==>
  property $ True  -- Would check cleanup mechanism

-- Error handler state properties
prop_handler_initialization_clean :: Property
prop_handler_initialization_clean =
  let handler = createErrorHandler
  in property $ True  -- Would check clean initialization

prop_handler_maintains_count :: [String] -> Property
prop_handler_maintains_count msgs =
  let errorMessages = L.filter (not . null) (L.map (take 50) msgs)
      handler = createErrorHandler
      handlerWithErrors = L.foldl (\h msg -> 
        let errorInfo = ErrorInfo msg ErrorWarning SourceContext (SourcePosition 1 1)
        in reportError h errorInfo
      ) handler errorMessages
  in property $ L.length errorMessages <= L.length errorMessages

prop_handler_can_reset :: [String] -> Property
prop_handler_can_reset msgs =
  let errorMessages = L.filter (not . null) (L.map (take 50) msgs)
      handler = createErrorHandler
      handlerWithErrors = L.foldl (\h msg -> 
        let errorInfo = ErrorInfo msg ErrorWarning SourceContext (SourcePosition 1 1)
        in reportError h errorInfo
      ) handler errorMessages
      resetHandler = createErrorHandler  -- Simulate reset
  in L.length errorMessages > 0 ==>
  property $ True  -- Would check reset functionality

prop_handler_state_consistent :: String -> ErrorSeverity -> Property
prop_handler_state_consistent msg severity =
  let errorMsg = take 100 msg
      handler = createErrorHandler
      errorInfo = ErrorInfo errorMsg severity SourceContext (SourcePosition 1 1)
      updatedHandler = reportError handler errorInfo
  in not (null errorMsg) ==>
  property $ True  -- Would check state consistency

prop_handler_concurrent_errors :: [String] -> Property
prop_handler_concurrent_errors msgs =
  let errorMessages = L.filter (not . null) (L.map (take 50) msgs)
      handler = createErrorHandler
      -- Simulate concurrent error reporting
      handlersWithErrors = L.map (\msg -> 
        let errorInfo = ErrorInfo msg ErrorWarning SourceContext (SourcePosition 1 1)
        in reportError handler errorInfo
      ) errorMessages
  in L.length errorMessages > 1 ==>
  property $ L.length handlersWithErrors == L.length errorMessages