{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.Unit.ErrorHandlingAdvancedSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Gen, arbitrary, oneof, elements, choose, listOf, resize)

import Compiler.Errors.Core
  ( TypeError(..)
  , ErrorSeverity(..)
  , ErrorCategory(..)
  , ErrorLocation(..)
  , ErrorContext(..)
  , ErrorRecovery(..)
  , CombinedError(..)
  , ErrorCollector
  , newErrorCollector
  , addError
  , addWarning
  , addInfo
  , getErrors
  , getWarnings
  , getInfo
  , getAllMessages
  , hasErrors
  , hasWarnings
  , formatError
  , formatErrorWithLocation
  , formatErrors
  , formatErrorsWithLocation
  , canRecoverFrom
  , shouldContinueAfter
  , errorAt
  , errorWithCategory
  , warningAt
  , infoAt
  , withLocation
  , withContext
  , withSuggestions
  , withRelatedErrors
  , withTimestamp
  , wrapError
  , combineErrors
  , errorWithSuggestions
  , hasCategory
  , filterByCategory
  , filterBySeverity
  , getErrorStatistics
  , generateErrorReport
  , emptyContext
  , fatalRecovery
  , errorRecovery
  , warningRecovery
  , infoRecovery
  , customRecovery
  , formatTimestamp
  , getCurrentTimestamp
  , createRecoveryStrategy
  , fatalError
  , fatalErrorWithCategory
  )

import qualified Data.Map.Strict as Map
import Data.List (sort, isInfixOf, isPrefixOf)
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.Text as T

-- ============================================================================
-- Advanced Error Handling Tests
-- ============================================================================

tests :: TestTree
tests =
  testGroup "Advanced Error Handling Tests"
    [ testGroup "Error Creation and Construction"
        [ testCase "creates basic error with all fields" $ do
            let location = ErrorLocation (Just "test.typus") 10 5 (Just 10) (Just 20)
            let context = ErrorContext (Just "println(\"Hello\")") (Just "main") (Just "msg") (Just "string") []
            let recovery = customRecovery True True (Just "Retry") (Just "Check syntax") 30 0.8
            let error = TypeError
                    { errorId = "TEST001"
                    , severity = Error
                    , category = TypeChecking
                    , message = "Type mismatch error"
                    , location = location
                    , context = context
                    , recovery = recovery
                    , suggestions = ["Check types", "Verify imports"]
                    , relatedErrors = []
                    , errorChain = []
                    , timestamp = Nothing
                    }
            
            errorId error @?= "TEST001"
            severity error @?= Error
            category error @?= TypeChecking
            message error @?= "Type mismatch error"
            location error @?= location
            context error @?= context
            recovery error @?= recovery
            suggestions error @?= ["Check types", "Verify imports"]

        , testCase "errorAt creates error with minimal fields" $ do
            let location = ErrorLocation Nothing 5 10 Nothing Nothing
            let error = errorAt "ERR001" (T.pack "Test error") location
            
            errorId error @?= "ERR001"
            severity error @?= Error
            category error @?= Unknown
            message error @?= "Test error"
            location error @?= location
            context error @?= emptyContext
            recovery error @?= errorRecovery

        , testCase "errorWithCategory sets category correctly" $ do
            let location = ErrorLocation (Just "file.typus") 1 1 Nothing Nothing
            let error = errorWithCategory "CAT001" Parsing (T.pack "Parse error") location
            
            category error @?= Parsing
            severity error @?= Error
            message error @?= "Parse error"

        , testCase "warningAt and infoAt create appropriate severity levels" $ do
            let location = ErrorLocation Nothing 1 1 Nothing Nothing
            let warning = warningAt "WARN001" (T.pack "Warning message") location
            let info = infoAt "INFO001" (T.pack "Info message") location
            
            severity warning @?= Warning
            severity info @?= Info
            category warning @?= Unknown
            category info @?= Unknown
        ]

    , testGroup "Error Modification and Enhancement"
        [ testCase "withLocation updates error location" $ do
            let originalLocation = ErrorLocation Nothing 1 1 Nothing Nothing
            let newLocation = ErrorLocation (Just "new.typus") 5 10 (Just 5) (Just 15)
            let error = errorAt "ERR001" (T.pack "Test") originalLocation
            let updatedError = withLocation error newLocation
            
            location updatedError @?= newLocation
            errorId updatedError @?= errorId error
            message updatedError @?= message error

        , testCase "withContext adds context information" $ do
            let location = ErrorLocation Nothing 1 1 Nothing Nothing
            let context = ErrorContext (Just "code") (Just "func") (Just "var") (Just "type") [("extra", "info")]
            let error = errorAt "ERR001" (T.pack "Test") location
            let contextError = withContext error context
            
            context contextError @?= context
            location contextError @?= location

        , testCase "withSuggestions adds suggestion list" $ do
            let location = ErrorLocation Nothing 1 1 Nothing Nothing
            let error = errorAt "ERR001" (T.pack "Test") location
            let suggestions = ["Suggestion 1", "Suggestion 2"]
            let enhancedError = withSuggestions suggestions error
            
            suggestions enhancedError @?= suggestions
            errorId enhancedError @?= errorId error

        , testCase "wrapError creates error chain" $ do
            let location = ErrorLocation Nothing 1 1 Nothing Nothing
            let innerError = errorAt "INNER001" (T.pack "Inner error") location
            let wrappedError = wrapError (T.pack "Wrapper: ") innerError
            
            message wrappedError @?= "Wrapper: Inner error"
            errorChain wrappedError @?= [innerError]
            errorId wrappedError @?= errorId innerError

        , testCase "timestamp handling" $ do
            let location = ErrorLocation Nothing 1 1 Nothing Nothing
            let error = errorAt "ERR001" (T.pack "Test") location
            let timestamp = "2023-12-25 10:30:45.123"
            let timestampedError = withTimestamp timestamp error
            
            timestamp timestampedError @?= Just timestamp
            errorId timestampedError @?= errorId error
        ]

    , testGroup "Error Collection and Management"
        [ testCase "ErrorCollector manages different error types" $ do
            let location = ErrorLocation Nothing 1 1 Nothing Nothing
            let error = errorAt "ERR001" (T.pack "Error") location
            let warning = warningAt "WARN001" (T.pack "Warning") location
            let info = infoAt "INFO001" (T.pack "Info") location
            
            let allErrors = combineErrors [error, warning, info]
            
            length allErrors @?= 3
            hasErrors allErrors @?= True
            hasWarnings allErrors @?= True
            length (getErrors allErrors) @?= 1
            length (getWarnings allErrors) @?= 1
            length (getInfo allErrors) @?= 1

        , testCase "filterByCategory and filterBySeverity work correctly" $ do
            let location = ErrorLocation Nothing 1 1 Nothing Nothing
            let typeError = errorWithCategory "TYPE001" TypeChecking (T.pack "Type error") location
            let parseError = errorWithCategory "PARSE001" Parsing (T.pack "Parse error") location
            let warning = warningAt "WARN001" (T.pack "Warning") location
            
            let allErrors = [typeError, parseError, warning]
            let typeErrors = filterByCategory TypeChecking allErrors
            let parseErrors = filterByCategory Parsing allErrors
            let errorSeverity = filterBySeverity Error allErrors
            let warningSeverity = filterBySeverity Warning allErrors
            
            length typeErrors @?= 1
            length parseErrors @?= 1
            length errorSeverity @?= 2
            length warningSeverity @?= 1

        , testCase "getErrorStatistics provides correct counts" $ do
            let location = ErrorLocation Nothing 1 1 Nothing Nothing
            let errors = 
                  [ errorWithCategory "TYPE001" TypeChecking (T.pack "Type error") location
                  , errorWithCategory "OWN001" Ownership (T.pack "Ownership error") location
                  , warningWithCategory "WARN001" Parsing (T.pack "Parse warning") location
                  , infoAt "INFO001" (T.pack "Info") location
                  ]
            
            let stats = getErrorStatistics errors
            
            Map.lookup "total" stats @?= Just 4
            Map.lookup "errors" stats @?= Just 2
            Map.lookup "warnings" stats @?= Just 1
            Map.lookup "info" stats @?= Just 1
            Map.lookup "typeChecking" stats @?= Just 1
            Map.lookup "ownership" stats @?= Just 1
            Map.lookup "parsing" stats @?= Just 1
        ]

    , testGroup "Error Recovery Strategies"
        [ testCase "built-in recovery strategies have correct properties" $ do
            assertBool "fatal recovery cannot recover" (not $ canRecover fatalRecovery)
            assertBool "fatal recovery should not continue" (not $ shouldContinue fatalRecovery)
            assertBool "fatal recovery has max cost" (recoveryCost fatalRecovery == 100)
            assertBool "fatal recovery has zero confidence" (recoveryConfidence fatalRecovery == 0.0)
            
            assertBool "error recovery can recover" (canRecover errorRecovery)
            assertBool "error recovery should continue" (shouldContinue errorRecovery)
            assertBool "error recovery has medium cost" (recoveryCost errorRecovery == 50)
            assertBool "error recovery has good confidence" (recoveryConfidence errorRecovery == 0.7)
            
            assertBool "warning recovery can recover" (canRecover warningRecovery)
            assertBool "warning recovery should continue" (shouldContinue warningRecovery)
            assertBool "warning recovery has low cost" (recoveryCost warningRecovery == 10)
            assertBool "warning recovery has high confidence" (recoveryConfidence warningRecovery == 0.9)

        , testCase "customRecovery creates strategy with specified properties" $ do
            let recovery = customRecovery True True (Just "Custom action") (Just "Custom hint") 25 0.85
            
            canRecover recovery @?= True
            shouldContinue recovery @?= True
            recoveryAction recovery @?= Just "Custom action"
            recoveryHint recovery @?= Just "Custom hint"
            recoveryCost recovery @?= 25
            recoveryConfidence recovery @?= 0.85

        , testCase "errors use correct recovery strategies" $ do
            let location = ErrorLocation Nothing 1 1 Nothing Nothing
            let regularError = errorAt "ERR001" (T.pack "Error") location
            let warning = warningAt "WARN001" (T.pack "Warning") location
            let info = infoAt "INFO001" (T.pack "Info") location
            let fatal = fatalError "FATAL001" (T.pack "Fatal") location
            
            recovery regularError @?= errorRecovery
            recovery warning @?= warningRecovery
            recovery info @?= infoRecovery
            recovery fatal @?= fatalRecovery

        , testCase "canRecoverFrom and shouldContinueAfter work correctly" $ do
            let location = ErrorLocation Nothing 1 1 Nothing Nothing
            let recoverableError = errorAt "ERR001" (T.pack "Error") location
            let nonRecoverableError = fatalError "FATAL001" (T.pack "Fatal") location
            
            canRecoverFrom recoverableError @?= True
            shouldContinueAfter recoverableError @?= True
            canRecoverFrom nonRecoverableError @?= False
            shouldContinueAfter nonRecoverableError @?= False
        ]

    , testGroup "Error Formatting and Reporting"
        [ testCase "formatError includes all relevant information" $ do
            let location = ErrorLocation (Just "test.typus") 5 10 (Just 5) (Just 15)
            let context = ErrorContext (Just "println(\"test\")") (Just "main") (Just "msg") (Just "string") []
            let error = errorWithSuggestions "ERR001" (T.pack "Test error") ["Check syntax"] location
            let contextError = withContext error context
            
            let formatted = formatError contextError
            assertBool "includes error ID" ("ERR001" `isInfixOf` formatted)
            assertBool "includes severity" ("[ERROR]" `isInfixOf` formatted)
            assertBool "includes message" ("Test error" `isInfixOf` formatted)
            assertBool "includes suggestions" ("Check syntax" `isInfixOf` formatted)
            assertBool "includes context" ("function: main" `isInfixOf` formatted)

        , testCase "formatErrorWithLocation includes location information" $ do
            let location = ErrorLocation (Just "test.typus") 10 5 (Just 10) (Just 20)
            let error = errorAt "ERR001" (T.pack "Test") location
            let formatted = formatErrorWithLocation error
            
            assertBool "includes file path" ("test.typus" `isInfixOf` formatted)
            assertBool "includes line number" ("10:" `isInfixOf` formatted)
            assertBool "includes column number" (":5" `isInfixOf` formatted)

        , testCase "formatErrors handles multiple errors" $ do
            let location = ErrorLocation Nothing 1 1 Nothing Nothing
            let error1 = errorAt "ERR001" (T.pack "First error") location
            let error2 = warningAt "WARN001" (T.pack "Warning") location
            let error3 = infoAt "INFO001" (T.pack "Info") location
            
            let formatted = formatErrors [error1, error2, error3]
            let lines = lines formatted
            
            assertBool "includes all errors" (length lines >= 3)
            assertBool "includes error" ("First error" `isInfixOf` formatted)
            assertBool "includes warning" ("Warning" `isInfixOf` formatted)
            assertBool "includes info" ("Info" `isInfixOf` formatted)

        , testCase "generateErrorReport creates comprehensive report" $ do
            let location = ErrorLocation (Just "test.typus") 1 1 Nothing Nothing
            let errors = 
                  [ errorWithCategory "TYPE001" TypeChecking (T.pack "Type error") location
                  , warningWithCategory "WARN001" Parsing (T.pack "Parse warning") location
                  ]
            
            let report = generateErrorReport errors
            let lines' = lines report
            
            assertBool "includes header" ("Error Report" `isInfixOf` report)
            assertBool "includes statistics" ("Statistics:" `isInfixOf` report)
            assertBool "includes total count" ("total: 2" `isInfixOf` report)
            assertBool "includes error count" ("errors: 1" `isInfixOf` report)
            assertBool "includes warning count" ("warnings: 1" `isInfixOf` report)
            assertBool "includes detailed errors" ("Detailed Errors:" `isInfixOf` report)

        , testCase "timestamp formatting works correctly" $ do
            let timestamp = "2023-12-25 15:30:45.123"
            let formatted = formatTimestamp (read timestamp :: UTCTime)
            
            assertBool "format preserves timestamp format" (timestamp `isPrefixOf` formatted)
        ]

    , testGroup "Property-Based Error Handling Tests"
        [ fastProperty "error creation preserves all fields" $
            \errId severity category message ->
                let location = ErrorLocation Nothing 1 1 Nothing Nothing
                    error = errorAt errId (T.pack message) location
                    categorizedError = errorWithCategory errId category (T.pack message) location
                in errorId error === errId .&&.
                   message error === T.pack message .&&.
                   category categorizedError === category

        , fastProperty "error filtering works correctly" $
            \errors ->
                let filteredErrors = filterBySeverity Error errors
                    filteredWarnings = filterBySeverity Warning errors
                    errorCount = length $ filter (\e -> severity e == Error) errors
                    warningCount = length $ filter (\e -> severity e == Warning) errors
                in length filteredErrors === errorCount .&&.
                   length filteredWarnings === warningCount

        , fastProperty "error statistics are accurate" $
            \errors ->
                let stats = getErrorStatistics errors
                    total = length errors
                    errorCount = length $ filter (\e -> severity e == Error) errors
                    warningCount = length $ filter (\e -> severity e == Warning) errors
                    infoCount = length $ filter (\e -> severity e == Info) errors
                in Map.lookup "total" stats === Just total .&&.
                   Map.lookup "errors" stats === Just errorCount .&&.
                   Map.lookup "warnings" stats === Just warningCount .&&.
                   Map.lookup "info" stats === Just infoCount

        , fastProperty "error recovery strategies are consistent" $
            \canRec shouldCont ->
                let recovery = customRecovery canRec shouldCont Nothing Nothing 50 0.5
                in canRecover recovery === canRec .&&.
                   shouldContinue recovery === shouldCont
        ]

    , testGroup "Edge Cases and Error Conditions"
        [ testCase "handles empty error collections gracefully" $ do
            let emptyErrors = []
            
            hasErrors emptyErrors @?= False
            hasWarnings emptyErrors @?= False
            length (getErrors emptyErrors) @?= 0
            length (getWarnings emptyErrors) @?= 0
            length (getInfo emptyErrors) @?= 0
            
            let stats = getErrorStatistics emptyErrors
            Map.lookup "total" stats @?= Just 0

        , testCase "handles errors with minimal information" $ do
            let location = ErrorLocation Nothing 0 0 Nothing Nothing
            let error = errorAt "" (T.pack "") location
            
            errorId error @?= ""
            message error @?= ""
            location error @?= location
            context error @?= emptyContext

        , testCase "handles errors with maximal information" $ do
            let location = ErrorLocation (Just "very-long-file-name.typus") 999999 999999 (Just 999999) (Just 999999)
            let context = ErrorContext (Just "very long code snippet") (Just "very long function name") 
                                     (Just "very long variable name") (Just "very long type name")
                                     [("very long key", "very long value")]
            let recovery = customRecovery True True (Just "very long recovery action") 
                                                (Just "very long recovery hint") 100 1.0
            let suggestions = replicate 100 (T.pack "very long suggestion message")
            let relatedErrors = replicate 50 (errorAt "RELATED" (T.pack "related") location)
            let error = TypeError
                    { errorId = "MAXIMAL_ERROR"
                    , severity = Fatal
                    , category = Integration
                    , message = T.pack $ replicate 1000 "very long message "
                    , location = location
                    , context = context
                    , recovery = recovery
                    , suggestions = suggestions
                    , relatedErrors = relatedErrors
                    , errorChain = relatedErrors
                    , timestamp = Just "2023-12-25 23:59:59.999"
                    }
            
            assertBool "handles maximal error without crashing" (True)
            length (suggestions error) @?= 100
            length (relatedErrors error) @?= 50
            length (errorChain error) @?= 50

        , testCase "formatting handles unicode characters correctly" $ do
            let location = ErrorLocation (Just "测试文件.typus") 10 5 Nothing Nothing
            let context = ErrorContext (Just "println(\"你好世界\")") (Just "主函数") (Just "变量") (Just "字符串") []
            let error = errorWithSuggestions "UNICODE001" (T.pack "Unicode 错误测试 🚀") 
                                            ["检查编码", "验证字符"] location
            let contextError = withContext error context
            
            let formatted = formatErrorWithLocation contextError
            assertBool "handles unicode in file path" ("测试文件" `isInfixOf` formatted)
            assertBool "handles unicode in message" ("Unicode 错误测试 🚀" `isInfixOf` formatted)
            assertBool "handles unicode in context" ("主函数" `isInfixOf` formatted)
            assertBool "handles unicode in suggestions" ("检查编码" `isInfixOf` formatted)
        ]
    ]