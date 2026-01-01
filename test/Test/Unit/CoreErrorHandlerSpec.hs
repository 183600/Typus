module Test.Unit.CoreErrorHandlerSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, choose, oneof, elements, listOf)
import qualified Data.Map.Strict as Map
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.Text as T

import Compiler.Errors.Core as Core
import Compiler.Errors.Compiler as CE

-- | Core functionality tests for ErrorHandler module
tests :: TestTree
tests =
  testGroup "Core ErrorHandler Tests"
    [ testGroup "ErrorSeverity operations"
        [ testCase "severityPriority returns correct priorities" $ do
            Core.severityPriority Core.Fatal @?= 100
            Core.severityPriority Core.Error @?= 80
            Core.severityPriority Core.Warning @?= 30
            Core.severityPriority Core.Info @?= 10

        , testCase "compareSeverity orders correctly" $ do
            compareSeverity Fatal Error @?= GT
            compareSeverity Error Warning @?= GT
            compareSeverity Warning Info @?= GT
            compareSeverity Error Error @?= EQ

        , testCase "isAtLeast works correctly" $ do
            assertBool "Fatal is at least Error" $ isAtLeast Error Fatal
            assertBool "Error is at least Error" $ isAtLeast Error Error
            assertBool "Warning is not at least Error" $ not (isAtLeast Error Warning)
            assertBool "Info is at least Info" $ isAtLeast Info Info
        ]

    , testGroup "ErrorLocation operations"
        [ testCase "creates locations correctly" $ do
            let loc1 = _atLocation 5 10
                loc2 = _atFileLocation "test.typus" 3 7
                loc3 = _atRange 1 1 2 20
            getErrorLine loc1 @?= 5
            getErrorColumn loc1 @?= 10
            filePath loc1 @?= Nothing
            filePath loc2 @?= Just "test.typus"
            endLine loc3 @?= Just 2
            endColumn loc3 @?= Just 20

        , testCase "unknownLocation has correct values" $ do
            getErrorLine _unknownLocation @?= 0
            getErrorColumn _unknownLocation @?= 0
            filePath _unknownLocation @?= Nothing
        ]

    , testGroup "ErrorContext operations"
        [ testCase "emptyContext has no information" $ do
            contextCode emptyContext @?= Nothing
            contextFunction emptyContext @?= Nothing
            contextVariable emptyContext @?= Nothing
            contextType emptyContext @?= Nothing
            contextAdditional emptyContext @?= []

        , testCase "context can be created with fields" $ do
            let ctx = ErrorContext
                    { contextCode = Just "x := 5"
                    , contextFunction = Just "main"
                    , contextVariable = Just "x"
                    , contextType = Just "int"
                    , contextAdditional = [("scope", "global")]
                    }
            contextCode ctx @?= Just "x := 5"
            contextFunction ctx @?= Just "main"
            contextVariable ctx @?= Just "x"
            contextType ctx @?= Just "int"
            contextAdditional ctx @?= [("scope", "global")]
        ]

    , testGroup "ErrorRecovery operations"
        [ testCase "predefined recovery strategies have correct properties" $ do
            assertBool "fatal recovery cannot recover" $ not (canRecover fatalRecovery)
            assertBool "fatal recovery should not continue" $ not (shouldContinue fatalRecovery)
            assertBool "error recovery can recover" $ canRecover errorRecovery
            assertBool "error recovery should continue" $ shouldContinue errorRecovery
            assertBool "warning recovery can recover" $ canRecover warningRecovery
            assertBool "warning recovery should continue" $ shouldContinue warningRecovery

        , testCase "custom recovery works correctly" $ do
            let custom = customRecovery True False (Just "retry") (Just "check input") 25 0.8
            canRecover custom @?= True
            shouldContinue custom @?= False
            recoveryAction custom @?= Just "retry"
            recoveryHint custom @?= Just "check input"
            recoveryCost custom @?= 25
            recoveryConfidence custom @?= 0.8
        ]

    , testGroup "TypeError construction L.and manipulation"
        [ testCase "errorAt "test-id" = ErrorContext (Just "code") (Just "func") Nothing Nothing []
                suggestions = ["Try this", "Try that"]
                modified = withContext ctx $ withSuggestions (map T.pack suggestions) baseErr
            context modified @?= ctx
            suggestions modified @?= map T.pack suggestions

        , testCase "wrapError adds to error chain" $ do
            let loc = _atLocation 1 1
                inner = errorAt "test-id" (getErrors errors) @?= 2  -- Error + Fatal
            L.length (getWarnings errors) @?= 1
            L.length (getInfo errors) @?= 1
            hasErrors errors @?= True
            hasWarnings errors @?= True

        , testCase "getAllMessages returns L.all errors" $ do
            let loc = _atLocation 1 1
                errors = [errorAt "test-id" (getAllMessages errors) @?= 2
        ]

    , testGroup "Error formatting"
        [ testCase "formatError includes severity L.and category" $ do
            let loc = _atLocation 1 1
                err = errorWithCategory "ERR001" TypeChecking "Type error" loc
                formatted = formatError err
            assertBool "contains ERROR" $ "ERROR" `L.isInfixOf` formatted
            assertBool "contains TypeChecking" $ "TypeChecking" `L.isInfixOf` formatted
            assertBool "contains message" $ "Type error" `L.isInfixOf` formatted

        , testCase "formatErrorWithLocation includes location" $ do
            let loc = _atFileLocation "test.typus" 5 10
                err = errorAt "test-id" (hasCategory TypeChecking) typeErrors @?= True

        , testCase "filterBySeverity works correctly" $ do
            let loc = _atLocation 1 1
                errors = 
                    [ errorAt "test-id" == Error) errorOnly @?= True

        , testCase "hasCategory checks correctly" $ do
            let loc = _atLocation 1 1
                err = errorWithCategory "E001" TypeChecking "Type error" loc
            hasCategory TypeChecking err @?= True
            hasCategory Ownership err @?= False
        ]

    , testGroup "Property-based tests"
        [ testProperty "severityPriority is consistent with compareSeverity" $
            \sev1 sev2 -> 
                let cmp = compareSeverity sev1 sev2
                    prio1 = severityPriority sev1
                    prio2 = severityPriority sev2
                in case cmp of
                    GT -> prio1 > prio2
                    EQ -> prio1 == prio2
                    LT -> prio1 < prio2

        , testProperty "error filtering preserves total count" $
            \errors -> 
                let allMsgs = getAllMessages errors
                    errs = getErrors errors
                    warns = getWarnings errors
                    infos = getInfo errors
                in L.length allMsgs == L.length errs + L.length warns + L.length infos

        , testProperty "wrapError increases error chain L.length" $
            \err wrapperMsg -> 
                let wrapped = wrapError wrapperMsg err
                    originalChain = L.length (errorChain err)
                    newChain = L.length (errorChain wrapped)
                in newChain == originalChain + 1

        , testProperty "error statistics L.sum to total" $
            \errors ->
                let stats = getErrorStatistics errors
                    total = Map.findWithDefault 0 "total" stats
                    fatal = Map.findWithDefault 0 "fatal" stats
                    errs = Map.findWithDefault 0 "errors" stats
                    warnings = Map.findWithDefault 0 "warnings" stats
                    info = Map.findWithDefault 0 "info" stats
                in total == fatal + errs + warnings + info
        ]
    ]