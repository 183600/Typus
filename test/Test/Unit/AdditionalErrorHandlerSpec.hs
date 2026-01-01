module Test.Unit.AdditionalErrorHandlerSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import qualified Data.Text as T (pack, unpack)
import Control.Monad.State (execState)

import Compiler.Errors.Core
  ( TypeError(..)
  , CombinedError(..)
  , ErrorSeverity(..)
  , ErrorCategory(..)
  , ErrorLocation(..)
  , ErrorContext(..)
  , emptyContext
  , ErrorRecovery(..)
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
  , formatErrors
  , errorAt
  , warningAt
  , infoAt
  , errorWithCategory
  , warningWithCategory
  , fatalError
  , withContext
  , withSuggestions
  , canRecoverFrom
  , shouldContinueAfter
  , combineErrors
  , combinedErrorSeverity
  , filterBySeverity
  , filterByCategory
  , hasCategory
  )

import SourceLocation (SourcePos(..), SourceSpan(..))
import Data.Time (UTCTime, fromGregorian, secondsToDiffTime)

-- | Additional unit tests for ErrorHandler module
tests :: TestTree
tests =
  testGroup "Additional ErrorHandler tests"
    [ testGroup "Error creation L.and basic properties"
        [ testCase "errorAt creates error with correct properties" $ do
            let errLoc = ErrorLocation (Just "test.typus") 1 5 Nothing Nothing
                error = errorAt "test-id" (T.pack "Test error message") errLoc
            T.unpack (message error) @?= "Test error message"
            severity error @?= Error
            category error @?= Unknown
            location error @?= errLoc
            assertBool "Context should be empty" (L.null $ contextAdditional $ context error)

        , testCase "warningAt creates warning with correct properties" $ do
            let location = ErrorLocation (Just "test.typus") 2 3 Nothing Nothing
                warning = warningAt "test-id" (T.pack "Test warning") location
            T.unpack (message warning) @?= "Test warning"
            severity warning @?= Warning
            category warning @?= Unknown

        , testCase "infoAt creates info with correct properties" $ do
            let location = ErrorLocation (Just "test.typus") 3 1 Nothing Nothing
                info = infoAt "test-id" (T.pack "Test info") location
            T.unpack (message info) @?= "Test info"
            severity info @?= Info
            category info @?= Unknown

        , testCase "fatalError creates fatal error" $ do
            let location = ErrorLocation (Just "test.typus") 1 1 Nothing Nothing
                fatal = fatalError "test-id" (T.pack "Fatal error message") location
            T.unpack (message fatal) @?= "Fatal error message"
            severity fatal @?= Fatal
            category fatal @?= Unknown
        ]

    , testGroup "Error categorization"
        [ testCase "errorWithCategory sets correct category" $ do
            let location = ErrorLocation (Just "test.typus") 1 1 Nothing Nothing
                error = errorWithCategory "test-id" TypeChecking (T.pack "Type mismatch error") location
            case error of
                TypeError { message = msg, severity = sev, category = cat } -> do
                    msg @?= T.pack "Type mismatch error"
                    sev @?= Error
                    cat @?= TypeChecking

        , testCase "warningWithCategory sets correct category" $ do
            let location = ErrorLocation (Just "test.typus") 1 1 Nothing Nothing
                warning = warningWithCategory "test-id" Unknown (T.pack "Deprecated feature warning") location
            case warning of
                TypeError { message = msg, severity = sev, category = cat } -> do
                    msg @?= T.pack "Deprecated feature warning"
                    sev @?= Warning
                    cat @?= Unknown
        ]

    , testGroup "Error collector operations"
        [ testCase "newErrorCollector creates empty collector" $ do
            let errors = execState newErrorCollector []
            hasErrors errors @?= False
            hasWarnings errors @?= False
            L.length (getAllMessages errors) @?= 0

        , testCase "addError adds error to collector" $ do
            let location = ErrorLocation (Just "test.typus") 1 1 Nothing Nothing
                error = errorAt "test-id" (T.pack "Test error") location
                updatedCollector = execState (addError error) []
            hasErrors updatedCollector @?= True
            L.length (getErrors updatedCollector) @?= 1

        , testCase "addWarning adds warning to collector" $ do
            let location = ErrorLocation (Just "test.typus") 1 1 Nothing Nothing
                warning = warningAt "test-id" (T.pack "Test warning") location
                updatedCollector = execState (addWarning warning) []
            hasWarnings updatedCollector @?= True
            L.length (getWarnings updatedCollector) @?= 1

        , testCase "addInfo adds info message to collector" $ do
            let location = ErrorLocation (Just "test.typus") 1 1 Nothing Nothing
                info = infoAt "test-id" (T.pack "Test info") location
                updatedCollector = execState (addInfo info) []
            L.length (getInfo updatedCollector) @?= 1
        ]

    , testGroup "Error filtering L.and analysis"
        [ testCase "filterBySeverity filters correctly" $ do
            let location = ErrorLocation (Just "test.typus") 1 1 Nothing Nothing
                error = errorAt "test-id" (T.pack "Error") location
                warning = warningAt "test-id" (T.pack "Warning") location
                info = infoAt "test-id" (T.pack "Info") location
                allMessages = [error, warning, info]
                errorsOnly = filterBySeverity Error allMessages
            L.length errorsOnly @?= 1

        , testCase "filterByCategory filters correctly" $ do
            let location = ErrorLocation (Just "test.typus") 1 1 Nothing Nothing
                typeError = errorWithCategory "test-id" TypeChecking (T.pack "Type error") location
                syntaxError = errorWithCategory "test-id" Parsing (T.pack "Syntax error") location
                allMessages = [typeError, syntaxError]
                typeErrorsOnly = filterByCategory TypeChecking allMessages
            L.length typeErrorsOnly @?= 1

        , testCase "hasCategory checks category correctly" $ do
            let location = ErrorLocation (Just "test.typus") 1 1 Nothing Nothing
                typeError = errorWithCategory "test-id" TypeChecking (T.pack "Type error") location
            hasCategory TypeChecking typeError @?= True
            hasCategory Parsing typeError @?= False
        ]

    , testGroup "Error recovery strategies"
        [ testCase "canRecoverFrom determines recoverability" $ do
            let location = ErrorLocation (Just "test.typus") 1 1 Nothing Nothing
                recoverableError = errorAt "test-id" (T.pack "Recoverable error") location
                fatalErr = fatalError "test-id" (T.pack "Fatal error") location
            canRecoverFrom recoverableError @?= True
            canRecoverFrom fatalErr @?= False

        , testCase "shouldContinueAfter determines continuation" $ do
            let location = ErrorLocation (Just "test.typus") 1 1 Nothing Nothing
                warning = warningAt "test-id" (T.pack "Warning") location
                error = errorAt "test-id" (T.pack "Error") location
                fatalErr = fatalError "test-id" (T.pack "Fatal error") location
            shouldContinueAfter warning @?= True
            shouldContinueAfter error @?= True
            shouldContinueAfter fatalErr @?= False
        ]

    , testGroup "Error combination"
        [ testCase "combineErrors combines errors correctly" $ do
            let location = ErrorLocation (Just "test.typus") 1 1 Nothing Nothing
                error1 = errorAt "test-id" (T.pack "First error") location
                error2 = errorAt "test-id" (T.pack "Second error") location
                combined = combineErrors [error1, error2]
            L.length combined @?= 2

        , testCase "combinedErrorSeverity determines highest severity" $ do
            let location = ErrorLocation (Just "test.typus") 1 1 Nothing Nothing
                warning = warningAt "test-id" (T.pack "Warning") location
                error = errorAt "test-id" (T.pack "Error") location
                fatal = fatalError "test-id" (T.pack "Fatal error") location
                combined = combineErrors [warning, error, fatal]
            -- Find the highest severity in the combined list
            let severities = map severity combined
            L.maximum severities @?= Fatal
        ]

    , testGroup "Error formatting"
        [ testCase "formatError produces non-empty string" $ do
            let location = ErrorLocation (Just "test.typus") 1 1 Nothing Nothing
                error = errorAt "test-id" (T.pack "Test error") location
                formatted = formatError error
            assertBool "Formatted error should not be empty" (not $ null formatted)

        , testCase "formatErrors formats multiple errors" $ do
            let location = ErrorLocation (Just "test.typus") 1 1 Nothing Nothing
                error1 = errorAt "test-id" (T.pack "First error") location
                error2 = errorAt "test-id" (T.pack "Second error") location
                formatted = formatErrors [error1, error2]
            assertBool "Formatted errors should not be empty" (not $ null formatted)
        ]

    , testGroup "Error context L.and suggestions"
        [ testCase "withContext adds context to error" $ do
            let location = ErrorLocation (Just "test.typus") 1 1 Nothing Nothing
                baseError = errorAt "test-id" (T.pack "Base error") location
                customContext = emptyContext { contextAdditional = [("Context line 1", "value1"), ("Context line 2", "value2")] }
                contextError = withContext baseError customContext
            case contextError of
                TypeError errId msg sev cat loc ctx rec sug rel ts chain -> do
                    L.length (contextAdditional ctx) @?= 2

        , testCase "withSuggestions adds suggestions to error" $ do
            let location = ErrorLocation (Just "test.typus") 1 1 Nothing Nothing
                baseError = errorAt "test-id" (T.pack "Base error") location
                suggestionError = withSuggestions [T.pack "Suggestion 1", T.pack "Suggestion 2"] baseError
            case suggestionError of
                TypeError { suggestions = sug } -> do
                    L.length sug @?= 2
        ]
    ]