module Test.Unit.CoreErrorHandlerSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, choose, oneof, elements, listOf)
import qualified Data.Map.Strict as Map
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.Text as T

import Compiler.Errors.Core as Core
-- import Compiler.Errors.Compiler as CE

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
            Core.compareSeverity Core.Fatal Core.Error @?= GT
            Core.compareSeverity Core.Error Core.Warning @?= GT
            Core.compareSeverity Core.Warning Core.Info @?= GT
            Core.compareSeverity Core.Error Core.Error @?= EQ

        , testCase "isAtLeast works correctly" $ do
            assertBool "Fatal is at least Error" $ Core.isAtLeast Core.Error Core.Fatal
            assertBool "Error is at least Error" $ Core.isAtLeast Core.Error Core.Error
            assertBool "Warning is not at least Error" $ not (Core.isAtLeast Core.Error Core.Warning)
            assertBool "Info is at least Info" $ Core.isAtLeast Core.Info Core.Info
        ]

    , testGroup "ErrorLocation operations"
        [ testCase "creates locations correctly" $ do
            let loc1 = Core.ErrorLocation Nothing 5 10 Nothing Nothing
                loc2 = Core.ErrorLocation (Just "test.typus") 3 7 Nothing Nothing
                loc3 = Core.ErrorLocation Nothing 1 1 Nothing Nothing
            Core.getErrorLine loc1 @?= 5
            Core.getErrorColumn loc1 @?= 10
            Core.filePath loc1 @?= Nothing
            Core.filePath loc2 @?= Just "test.typus"

        , testCase "unknownLocation has correct values" $ do
            let unknownLoc = Core.ErrorLocation Nothing 0 0 Nothing Nothing
            Core.getErrorLine unknownLoc @?= 0
            Core.getErrorColumn unknownLoc @?= 0
            Core.filePath unknownLoc @?= Nothing
        ]

    , testGroup "ErrorContext operations"
        [ testCase "emptyContext has no information" $ do
            Core.contextCode Core.emptyContext @?= Nothing
            Core.contextFunction Core.emptyContext @?= Nothing
            Core.contextVariable Core.emptyContext @?= Nothing
            Core.contextType Core.emptyContext @?= Nothing
            Core.contextAdditional Core.emptyContext @?= []

        , testCase "context can be created with fields" $ do
            let ctx = Core.ErrorContext
                    { Core.contextCode = Just "x := 5"
                    , Core.contextFunction = Just "main"
                    , Core.contextVariable = Just "x"
                    , Core.contextType = Just "int"
                    , Core.contextAdditional = [("scope", "global")]
                    }
            Core.contextCode ctx @?= Just "x := 5"
            Core.contextFunction ctx @?= Just "main"
            Core.contextVariable ctx @?= Just "x"
            Core.contextType ctx @?= Just "int"
            Core.contextAdditional ctx @?= [("scope", "global")]
        ]

    , testGroup "ErrorRecovery operations"
        [ testCase "predefined recovery strategies have correct properties" $ do
            let unknownLoc = Core.ErrorLocation Nothing 0 0 Nothing Nothing
                fatalErr = Core.TypeError "FATAL" Core.Fatal Core.Unknown (T.pack "fatal error") unknownLoc Core.emptyContext Core.errorRecovery [] [] [] Nothing
                errorErr = Core.TypeError "ERROR" Core.Error Core.Unknown (T.pack "error message") unknownLoc Core.emptyContext Core.errorRecovery [] [] [] Nothing
                warningErr = Core.TypeError "WARN" Core.Warning Core.Unknown (T.pack "warning message") unknownLoc Core.emptyContext Core.errorRecovery [] [] [] Nothing
            assertBool "fatal recovery cannot recover" $ not (Core.canRecoverFrom fatalErr)
            assertBool "fatal recovery should not continue" $ not (Core.shouldContinueAfter fatalErr)
            assertBool "error recovery can recover" $ Core.canRecoverFrom errorErr
            assertBool "error recovery should continue" $ Core.shouldContinueAfter errorErr
            assertBool "warning recovery can recover" $ Core.canRecoverFrom warningErr
            assertBool "warning recovery should continue" $ Core.shouldContinueAfter warningErr

        , testCase "custom recovery works correctly" $ do
            let recovery = Core.customRecovery True False (Just "retry") (Just "check input") 25 0.8
                unknownLoc = Core.ErrorLocation Nothing 0 0 Nothing Nothing
                custom = Core.TypeError "CUSTOM" Core.Error Core.Unknown (T.pack "custom error") unknownLoc Core.emptyContext recovery [] [] [] Nothing
            Core.canRecoverFrom custom @?= True
            Core.shouldContinueAfter custom @?= False
            Core.recoveryAction (Core.recovery custom) @?= Just "retry"
            Core.recoveryHint (Core.recovery custom) @?= Just "check input"
            Core.recoveryCost (Core.recovery custom) @?= 25
            Core.recoveryConfidence (Core.recovery custom) @?= 0.8
        ]

    , testGroup "TypeError construction and manipulation"
        [ testCase "errorAt creates basic error" $ do
            let loc = Core.ErrorLocation Nothing 1 1 Nothing Nothing
                err = Core.errorAt "ERR001" (T.pack "test error") loc
            Core.getErrorLine (Core.location err) @?= 1
            Core.getErrorColumn (Core.location err) @?= 1

        , testCase "withContext adds context to error" $ do
            let loc = Core.ErrorLocation Nothing 1 1 Nothing Nothing
                baseErr = Core.errorAt "ERR001" (T.pack "test error") loc
                ctx = Core.emptyContext { Core.contextCode = Just "x := 5" }
                modified = Core.withContext baseErr ctx
            Core.context modified @?= ctx

        , testCase "wrapError adds to error chain" $ do
            let loc = Core.ErrorLocation Nothing 1 1 Nothing Nothing
                inner = Core.errorAt "ERR002" (T.pack "inner error") loc
                wrapper = T.pack "wrapper message"
                wrapped = Core.wrapError wrapper inner
            Core.errorChain wrapped @?= inner : Core.errorChain inner

        , testCase "filterBySeverity works correctly" $ do
            let errors = 
                    let loc1 = Core.ErrorLocation Nothing 1 1 Nothing Nothing
                    in [ Core.errorAt "ERR001" (T.pack "error 1") loc1
                    , Core.warningAt "WARN001" (T.pack "warning 1") loc1
                    , Core.errorAt "ERR002" (T.pack "error 2") loc1
                    ]
                errorOnly = Core.filterBySeverity Core.Error errors
            L.length errorOnly @?= 2

        , testCase "hasCategory checks correctly" $ do
            let loc = Core.ErrorLocation Nothing 1 1 Nothing Nothing
                err = Core.errorWithCategory "E001" Core.TypeChecking (T.pack "Type error") loc
            Core.hasCategory Core.TypeChecking err @?= True
            Core.hasCategory Core.Ownership err @?= False
        ]

    , testGroup "Error formatting"
        [ testCase "formatError includes severity and category" $ do
            let loc = Core.ErrorLocation Nothing 1 1 Nothing Nothing
                err = Core.errorWithCategory "E001" Core.TypeChecking (T.pack "Type error") loc
                formatted = Core.formatError err
            assertBool "contains ERROR" $ "ERROR" `L.isInfixOf` formatted
            assertBool "contains TypeChecking" $ "TypeChecking" `L.isInfixOf` formatted
            assertBool "contains message" $ "Type error" `L.isInfixOf` formatted

        , testCase "formatErrorWithLocation includes location" $ do
            let loc = Core.ErrorLocation (Just "test.typus") 5 10 Nothing Nothing
                err = Core.errorAtWithUTCTime (read "2023-01-01 00:00:00 UTC") "ERR001" (T.pack "test error") loc
                formatted = Core.formatErrorWithLocation err
            assertBool "contains file path" $ "test.typus" `L.isInfixOf` formatted
            assertBool "contains line number" $ "5" `L.isInfixOf` formatted
            assertBool "contains column number" $ "10" `L.isInfixOf` formatted
        ]

--     , testGroup "Property-based tests"
--         [ testProperty "severityPriority is consistent with compareSeverity" $
--             \sev1 sev2 -> 
--                 let cmp = Core.compareSeverity sev1 sev2
--                     prio1 = Core.severityPriority sev1
--                     prio2 = Core.severityPriority sev2
--                 in case cmp of
--                     GT -> prio1 > prio2
--                     EQ -> prio1 == prio2
--                     LT -> prio1 < prio2

--         , testProperty "error filtering preserves total count" $
--             \errors -> 
--                 let allMsgs = Core.getAllMessages errors
--                     errs = Core.getErrors errors
--                     warns = Core.getWarnings errors
--                     infos = Core.getInfo errors
--                 in L.length allMsgs == L.length errs + L.length warns + L.length infos

--         , testProperty "wrapError increases error chain length" $
--             \err wrapperMsg -> 
--                 let wrapped = Core.wrapError wrapperMsg err
--                     originalChain = L.length (Core.errorChain err)
--                     newChain = L.length (Core.errorChain wrapped)
--                 in newChain == originalChain + 1

--         , testProperty "error statistics sum to total" $
--             \errors ->
--                 let stats = Core.getErrorStatistics errors
--                     total = Map.findWithDefault 0 "total" stats
--                     fatal = Map.findWithDefault 0 "fatal" stats
--                     errs = Map.findWithDefault 0 "errors" stats
--                     warnings = Map.findWithDefault 0 "warnings" stats
--                     info = Map.findWithDefault 0 "info" stats
--                 in total == fatal + errs + warnings + info
--         ]
        -- Temporarily disabled - missing Arbitrary instances
    ]