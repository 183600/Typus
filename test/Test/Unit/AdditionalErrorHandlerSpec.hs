module Test.Unit.AdditionalErrorHandlerSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

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
  , fatalError
  , errorWithCategory
  , warningWithCategory
  , canRecoverFrom
  , shouldContinueAfter
  , filterBySeverity
  , filterByCategory
  , hasCategory
  , getErrorStatistics
  , withLocation
  , withContext
  , withSuggestions
  , combineErrors
  , combinedErrorSeverity
  )

import SourceLocation (SourcePos(..), SourceSpan(..))
import Data.Time (UTCTime, fromGregorian, secondsToDiffTime)

-- | Additional unit tests for ErrorHandler module
tests :: TestTree
tests =
  testGroup "Additional ErrorHandler tests"
    [ testGroup "Error creation and basic properties"
        [ testCase "errorAt creates error with location" $ do
            let pos = SourcePos 1 5 10
                span = SourceSpan pos pos
                location = ErrorLocation span "test.typus"
                error = errorAt location "Test error message"
            case error of
                TypeError msg sev cat loc ctx rec sug rel ts -> do
                    msg @?= "Test error message"
                    sev @?= Error
                    cat @?= General
                    loc @?= location
                    assertBool "Context should be empty" (null ctx)

        , testCase "warningAt creates warning with correct severity" $ do
            let pos = SourcePos 2 3 15
                span = SourceSpan pos pos
                location = ErrorLocation span "test.typus"
                warning = warningAt location "Test warning"
            case warning of
                TypeError msg sev cat loc ctx rec sug rel ts -> do
                    msg @?= "Test warning"
                    sev @?= Warning
                    cat @?= General

        , testCase "infoAt creates info message with correct severity" $ do
            let pos = SourcePos 3 1 20
                span = SourceSpan pos pos
                location = ErrorLocation span "test.typus"
                info = infoAt location "Test info"
            case info of
                TypeError msg sev cat loc ctx rec rec sug rel ts -> do
                    msg @?= "Test info"
                    sev @?= Info
                    cat @?= General

        , testCase "fatalError creates fatal error" $ do
            let fatal = fatalError "Fatal error message"
            case fatal of
                TypeError msg sev cat loc ctx rec sug rel ts -> do
                    msg @?= "Fatal error message"
                    sev @?= Fatal
                    cat @?= General
        ]

    , testGroup "Error categorization"
        [ testCase "errorWithCategory sets correct category" $ do
            let pos = SourcePos 1 1 0
                span = SourceSpan pos pos
                location = ErrorLocation span "test.typus"
                error = errorWithCategory location TypeMismatch "Type mismatch error"
            case error of
                TypeError msg sev cat loc ctx rec sug rel ts -> do
                    msg @?= "Type mismatch error"
                    sev @?= Error
                    cat @?= TypeMismatch

        , testCase "warningWithCategory sets correct category" $ do
            let pos = SourcePos 1 1 0
                span = SourceSpan pos pos
                location = ErrorLocation span "test.typus"
                warning = warningWithCategory location Deprecated "Deprecated feature warning"
            case warning of
                TypeError msg sev cat loc ctx rec sug rel ts -> do
                    msg @?= "Deprecated feature warning"
                    sev @?= Warning
                    cat @?= Deprecated
        ]

    , testGroup "Error collector operations"
        [ testCase "newErrorCollector creates empty collector" $ do
            let collector = newErrorCollector
            hasErrors collector @?= False
            hasWarnings collector @?= False
            length (getAllMessages collector) @?= 0

        , testCase "addError adds error to collector" $ do
            let collector = newErrorCollector
                pos = SourcePos 1 1 0
                span = SourceSpan pos pos
                location = ErrorLocation span "test.typus"
                error = errorAt location "Test error"
                updatedCollector = addError error collector
            hasErrors updatedCollector @?= True
            length (getErrors updatedCollector) @?= 1

        , testCase "addWarning adds warning to collector" $ do
            let collector = newErrorCollector
                pos = SourcePos 1 1 0
                span = SourceSpan pos pos
                location = ErrorLocation span "test.typus"
                warning = warningAt location "Test warning"
                updatedCollector = addWarning warning collector
            hasWarnings updatedCollector @?= True
            length (getWarnings updatedCollector) @?= 1

        , testCase "addInfo adds info message to collector" $ do
            let collector = newErrorCollector
                pos = SourcePos 1 1 0
                span = SourceSpan pos pos
                location = ErrorLocation span "test.typus"
                info = infoAt location "Test info"
                updatedCollector = addInfo info collector
            length (getInfo updatedCollector) @?= 1
        ]

    , testGroup "Error filtering and analysis"
        [ testCase "filterBySeverity filters correctly" $ do
            let pos = SourcePos 1 1 0
                span = SourceSpan pos pos
                location = ErrorLocation span "test.typus"
                error = errorAt location "Error"
                warning = warningAt location "Warning"
                info = infoAt location "Info"
                allMessages = [error, warning, info]
                errorsOnly = filterBySeverity Error allMessages
            length errorsOnly @?= 1

        , testCase "filterByCategory filters correctly" $ do
            let pos = SourcePos 1 1 0
                span = SourceSpan pos pos
                location = ErrorLocation span "test.typus"
                typeError = errorWithCategory location TypeMismatch "Type error"
                syntaxError = errorWithCategory location Syntax "Syntax error"
                allMessages = [typeError, syntaxError]
                typeErrorsOnly = filterByCategory TypeMismatch allMessages
            length typeErrorsOnly @?= 1

        , testCase "hasCategory checks category correctly" $ do
            let pos = SourcePos 1 1 0
                span = SourceSpan pos pos
                location = ErrorLocation span "test.typus"
                typeError = errorWithCategory location TypeMismatch "Type error"
            hasCategory TypeMismatch typeError @?= True
            hasCategory Syntax typeError @?= False
        ]

    , testGroup "Error recovery strategies"
        [ testCase "canRecoverFrom determines recoverability" $ do
            let pos = SourcePos 1 1 0
                span = SourceSpan pos pos
                location = ErrorLocation span "test.typus"
                recoverableError = errorAt location "Recoverable error"
                fatalErr = fatalError "Fatal error"
            canRecoverFrom recoverableError @?= True
            canRecoverFrom fatalErr @?= False

        , testCase "shouldContinueAfter determines continuation" $ do
            let pos = SourcePos 1 1 0
                span = SourceSpan pos pos
                location = ErrorLocation span "test.typus"
                warning = warningAt location "Warning"
                error = errorAt location "Error"
                fatalErr = fatalError "Fatal error"
            shouldContinueAfter warning @?= True
            shouldContinueAfter error @?= True
            shouldContinueAfter fatalErr @?= False
        ]

    , testGroup "Error combination"
        [ testCase "combineErrors combines errors correctly" $ do
            let pos = SourcePos 1 1 0
                span = SourceSpan pos pos
                location = ErrorLocation span "test.typus"
                error1 = errorAt location "First error"
                error2 = errorAt location "Second error"
                combined = combineErrors [error1, error2]
            case combined of
                CombinedError errors -> length errors @?= 2

        , testCase "combinedErrorSeverity determines highest severity" $ do
            let pos = SourcePos 1 1 0
                span = SourceSpan pos pos
                location = ErrorLocation span "test.typus"
                warning = warningAt location "Warning"
                error = errorAt location "Error"
                fatal = fatalError "Fatal"
                combined = combineErrors [warning, error, fatal]
            combinedErrorSeverity combined @?= Fatal
        ]

    , testGroup "Error formatting"
        [ testCase "formatError produces non-empty string" $ do
            let pos = SourcePos 1 1 0
                span = SourceSpan pos pos
                location = ErrorLocation span "test.typus"
                error = errorAt location "Test error"
                formatted = formatError error
            assertBool "Formatted error should not be empty" (not $ null formatted)

        , testCase "formatErrors formats multiple errors" $ do
            let pos = SourcePos 1 1 0
                span = SourceSpan pos pos
                location = ErrorLocation span "test.typus"
                error1 = errorAt location "First error"
                error2 = errorAt location "Second error"
                formatted = formatErrors [error1, error2]
            assertBool "Formatted errors should not be empty" (not $ null formatted)
        ]

    , testGroup "Error context and suggestions"
        [ testCase "withContext adds context to error" $ do
            let pos = SourcePos 1 1 0
                span = SourceSpan pos pos
                location = ErrorLocation span "test.typus"
                baseError = errorAt location "Base error"
                contextError = withContext baseError ["Context line 1", "Context line 2"]
            case contextError of
                TypeError msg sev cat loc ctx rec sug rel ts -> do
                    length ctx @?= 2

        , testCase "withSuggestions adds suggestions to error" $ do
            let pos = SourcePos 1 1 0
                span = SourceSpan pos pos
                location = ErrorLocation span "test.typus"
                baseError = errorAt location "Base error"
                suggestionError = withSuggestions baseError ["Suggestion 1", "Suggestion 2"]
            case suggestionError of
                TypeError msg sev cat loc ctx rec sug rel ts -> do
                    length sug @?= 2
        ]
    ]