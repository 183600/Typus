{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Test.Unit.NewErrorHandlingSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Compiler.Errors.Core
  ( TypeError(..)
  , ErrorSeverity(..)
  , ErrorCategory(..)
  , ErrorLocation(..)
  , ErrorContext(..)
  , ErrorRecovery(..)
  , emptyContext
  , errorAt
  , errorAtWithUTCTime
  , errorWithCategory
  , warningAt
  , infoAt
  , fatalError
  , errorWithSuggestions
  , withLocation
  , withContext
  , withSuggestions
  , withRelatedErrors
  , withUTCTimestamp
  , wrapError
  , combineErrors
  , formatError
  , formatErrorWithLocation
  , formatErrors
  , formatErrorsWithLocation
  , hasCategory
  , filterByCategory
  , filterBySeverity
  , getErrorStatistics
  , generateErrorReport
  , generateErrorReportIO
  , canRecoverFrom
  , shouldContinueAfter
  , fatalRecovery
  , errorRecovery
  , warningRecovery
  , infoRecovery
  , customRecovery
  , getErrorLine
  , getErrorColumn
  , _atLocation
  , _atFileLocation
  , _atRange
  )

tests :: TestTree
tests =
  testGroup "New Error Handling"
    [ testCase "creates basic error with correct properties" $ do
        let loc = _atLocation 10 5
            err = errorAt "TEST001" "Test error message" loc
        
        errorId err @?= "TEST001"
        message err @?= "Test error message"
        location err @?= loc
        severity err @?= Error
        category err @?= Unknown
        context err @?= emptyContext
        recovery err @?= errorRecovery
        suggestions err @?= []
        relatedErrors err @?= []
        errorChain err @?= []

    , testCase "creates error with category" $ do
        let loc = _atLocation 1 1
            err = errorWithCategory "TYPE001" TypeChecking "Type mismatch error" loc
        
        errorId err @?= "TYPE001"
        category err @?= TypeChecking
        severity err @?= Error

    , testCase "creates warning with correct severity" $ do
        let loc = _atLocation 5 10
            warning = warningAt "WARN001" "This is a warning" loc
        
        severity warning @?= Warning
        errorId warning @?= "WARN001"
        message warning @?= "This is a warning"

    , testCase "creates info message with correct severity" $ do
        let loc = _atLocation 3 7
            info = infoAt "INFO001" "This is info" loc
        
        severity info @?= Info
        errorId info @?= "INFO001"
        message info @?= "This is info"

    , testCase "creates fatal error with non-recoverable recovery" $ do
        let loc = _atLocation 20 15
            fatal = fatalError "FATAL001" "Critical system failure" loc
        
        severity fatal @?= Fatal
        recovery fatal @?= fatalRecovery
        canRecoverFrom fatal @?= False
        shouldContinueAfter fatal @?= False

    , testCase "adds suggestions to error" $ do
        let loc = _atLocation 8 12
            suggestions = ["Try using a different type", "Check variable scope"]
            err = errorWithSuggestions "SUGG001" "Type inference failed" suggestions loc
        
        suggestions err @?= map T.pack suggestions

    , testCase "adds context to error" $ do
        let loc = _atLocation 15 8
            ctx = ErrorContext (Just "let x = 5") (Just "main") (Just "x") (Just "Int") []
            err = (errorAt "CTX001" "Variable error" loc) `withContext` ctx
        
        context err @?= ctx
        contextFunction (context err) @?= Just "main"
        contextVariable (context err) @?= Just "x"
        contextType (context err) @?= Just "Int"

    , testCase "wraps error with additional message" $ do
        let loc = _atLocation 10 10
            innerErr = errorAt "INNER001" "Inner error" loc
            wrappedErr = wrapError "Wrapper message" innerErr
        
        message wrappedErr @?= "Wrapper message: Inner error"
        errorChain wrappedErr @?= [innerErr]

    , testCase "combines related errors" $ do
        let loc1 = _atLocation 5 5
            loc2 = _atLocation 10 10
            err1 = errorAt "ERR001" "First error" loc1
            err2 = errorAt "ERR002" "Second error" loc2
            combinedErr = err1 `withRelatedErrors` [err2]
        
        relatedErrors combinedErr @?= [err2]

    , testCase "formats error without location" $ do
        let loc = _atLocation 1 1
            err = errorWithCategory "PARSE001" Parsing "Unexpected token" loc
            formatted = formatError err
        
        assertBool "format contains severity" ("[ERROR]" `isInfixOf` formatted)
        assertBool "format contains category" ("[Parsing]" `isInfixOf` formatted)
        assertBool "format contains message" ("Unexpected token" `isInfixOf` formatted)

    , testCase "formats error with location" $ do
        let loc = _atFileLocation "test.typus" 10 5
            err = errorAt "LOC001" "Location test" loc
            formatted = formatErrorWithLocation err
        
        assertBool "format contains file location" ("test.typus:10:5" `isInfixOf` formatted)
        assertBool "format contains message" ("Location test" `isInfixOf` formatted)

    , testCase "formats multiple errors sorted by severity" $ do
        let loc = _atLocation 1 1
            info = infoAt "INFO001" "Info message" loc
            warning = warningAt "WARN001" "Warning message" loc
            error = errorAt "ERR001" "Error message" loc
            fatal = fatalError "FATAL001" "Fatal message" loc
            errors = [info, warning, error, fatal]
            formatted = formatErrors errors
        
        let lines' = lines formatted
        assertBool "fatal comes first" ("[FATAL]" `isInfixOf` head lines')
        assertBool "info comes last" ("[INFO]" `isInfixOf` last lines')

    , testCase "filters errors by category" $ do
        let loc = _atLocation 1 1
            typeError = errorWithCategory "TYPE001" TypeChecking "Type error" loc
            ownershipError = errorWithCategory "OWN001" Ownership "Ownership error" loc
            parseError = errorWithCategory "PARSE001" Parsing "Parse error" loc
            errors = [typeError, ownershipError, parseError]
            typeErrors = filterByCategory TypeChecking errors
        
        length typeErrors @?= 1
        errorId (head typeErrors) @?= "TYPE001"

    , testCase "filters errors by severity" $ do
        let loc = _atLocation 1 1
            info = infoAt "INFO001" "Info" loc
            warning = warningAt "WARN001" "Warning" loc
            error = errorAt "ERR001" "Error" loc
            errors = [info, warning, error]
            warningsOnly = filterBySeverity Warning errors
        
        length warningsOnly @?= 1
        severity (head warningsOnly) @?= Warning

    , testCase "generates error statistics" $ do
        let loc = _atLocation 1 1
            info = infoAt "INFO001" "Info" loc
            warning = warningAt "WARN001" "Warning" loc
            error = errorAt "ERR001" "Error" loc
            typeError = errorWithCategory "TYPE002" TypeChecking "Type error" loc
            errors = [info, warning, error, typeError]
            stats = getErrorStatistics errors
        
        Map.lookup "total" stats @?= Just 4
        Map.lookup "errors" stats @?= Just 1
        Map.lookup "warnings" stats @?= Just 1
        Map.lookup "info" stats @?= Just 1
        Map.lookup "typeChecking" stats @?= Just 1

    , testCase "generates comprehensive error report" $ do
        let loc = _atLocation 5 10
            err = errorWithSuggestions "REPORT001" TypeChecking "Type mismatch" 
                                      ["Check types", "Add annotation"] loc
            report = generateErrorReport [err]
        
        assertBool "report contains header" ("Error Report" `isInfixOf` report)
        assertBool "report contains statistics" ("Statistics:" `isInfixOf` report)
        assertBool "report contains error details" ("Detailed Errors:" `isInfixOf` report)
        assertBool "report contains suggestions" ("Suggestions:" `isInfixOf` report)

    , testCase "handles error location with range" $ do
        let loc = _atRange 5 1 5 10
            err = errorAt "RANGE001" "Range error" loc
            formatted = formatErrorWithLocation err
        
        assertBool "format contains range" ("5:1-5:10" `isInfixOf` formatted)

    , testCase "creates error with timestamp" $ do
        let loc = _atLocation 1 1
            err = errorAt "TIME001" "Timed error" loc
            -- Test with a fake timestamp
            timestamped = withTimestamp "2023-12-25 10:30:45.123" err
            formatted = formatErrorWithLocation timestamped
        
        assertBool "format contains timestamp" ("[2023-12-25 10:30:45.123]" `isInfixOf` formatted)

    , testCase "creates custom recovery strategy" $ do
        let loc = _atLocation 1 1
            customRec = customRecovery True True (Just "Retry") (Just "Check connection") 30 0.8
            err = (errorAt "CUSTOM001" "Custom recovery" loc) { recovery = customRec }
        
        canRecoverFrom err @?= True
        shouldContinueAfter err @?= True
        recoveryAction (recovery err) @?= Just "Retry"
        recoveryHint (recovery err) @?= Just "Check connection"
        recoveryCost (recovery err) @?= 30
        recoveryConfidence (recovery err) @?= 0.8

    , testCase "error recovery strategies behave correctly" $ do
        let loc = _atLocation 1 1
            fatalErr = fatalError "FATAL001" "Fatal error" loc
            regularErr = errorAt "ERR001" "Regular error" loc
            warning = warningAt "WARN001" "Warning" loc
            info = infoAt "INFO001" "Info" loc
        
        canRecoverFrom fatalErr @?= False
        shouldContinueAfter fatalErr @?= False
        
        canRecoverFrom regularErr @?= True
        shouldContinueAfter regularErr @?= True
        
        canRecoverFrom warning @?= True
        shouldContinueAfter warning @?= True
        
        canRecoverFrom info @?= True
        shouldContinueAfter info @?= True

    , testCase "helper functions access location fields" $ do
        let loc = _atLocation 42 17
            line = getErrorLine loc
            column = getErrorColumn loc
        
        line @?= 42
        column @?= 17

    , testCase "error chain preserves hierarchy" $ do
        let loc = _atLocation 1 1
            baseErr = errorAt "BASE001" "Base error" loc
            wrapped1 = wrapError "First wrapper" baseErr
            wrapped2 = wrapError "Second wrapper" wrapped1
        
        errorChain wrapped2 @?= [wrapped1, baseErr]
        assertBool "contains both wrapper messages" ("First wrapper" `isInfixOf` formatError wrapped2)
        assertBool "contains both wrapper messages" ("Second wrapper" `isInfixOf` formatError wrapped2)
    ]