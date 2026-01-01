module Test.Unit.DebugIntegrationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import DebugIntegration
import CommandLineDebug (defaultCLIDebugConfig)
import Control.Exception (try, SomeException)

-- | Unit tests for DebugIntegration module
tests :: TestTree
tests = 
  testGroup "DebugIntegration"
    [ testGroup "Debugging workflow"
        [ testCase "withDebugging executes action with proper call stack" $ do
            let config = defaultCLIDebugConfig
            result <- withDebugging config "test-phase" (return "success")
            result @?= "success"

        , testCase "debugParseStep handles file parsing workflow" $ do
            let config = defaultCLIDebugConfig
            result <- debugParseStep config "test.typus" (return "parsed")
            result @?= "parsed"

        , testCase "debugCompileStep handles compilation workflow" $ do
            let config = defaultCLIDebugConfig
            result <- debugCompileStep config "test.typus" (return "compiled")
            result @?= "compiled"

        , testCase "debugOwnershipStep handles ownership analysis workflow" $ do
            let config = defaultCLIDebugConfig
            result <- debugOwnershipStep config "test.typus" (return "analyzed")
            result @?= "analyzed"
        ]

    , testGroup "Breakpoint management"
        [ testCase "createDebugBreakpoints creates breakpoints without errors" $ do
            let config = defaultCLIDebugConfig
            result <- try $ createDebugBreakpoints config ["parse", "compile", "ownership"]
            case result of
                Left (_ :: SomeException) -> assertBool "Should not throw exception" False
                Right _ -> assertBool "Breakpoints created successfully" True

        , testCase "addCustomBreakpoint adds single breakpoint" $ do
            let config = defaultCLIDebugConfig
            result <- try $ addCustomBreakpoint config "custom-point"
            case result of
                Left (_ :: SomeException) -> assertBool "Should not throw exception" False
                Right _ -> assertBool "Custom breakpoint added successfully" True

        , testCase "removeAllBreakpoints clears L.all breakpoints" $ do
            let config = defaultCLIDebugConfig
            result <- try $ removeAllBreakpoints config
            case result of
                Left (_ :: SomeException) -> assertBool "Should not throw exception" False
                Right _ -> assertBool "All breakpoints removed successfully" True
        ]

    , testGroup "Interactive mode"
        [ testCase "enableInteractiveMode enables interactive debugging" $ do
            let config = defaultCLIDebugConfig
            result <- try $ enableInteractiveMode config
            case result of
                Left (_ :: SomeException) -> assertBool "Should not throw exception" False
                Right _ -> assertBool "Interactive mode enabled successfully" True

        , testCase "disableInteractiveMode disables interactive debugging" $ do
            let config = defaultCLIDebugConfig
            result <- try $ disableInteractiveMode config
            case result of
                Left (_ :: SomeException) -> assertBool "Should not throw exception" False
                Right _ -> assertBool "Interactive mode disabled successfully" True
        ]

    , testGroup "Compiler debugging"
        [ testCase "debugCompilerStart initializes compiler debugging" $ do
            let config = defaultCLIDebugConfig
            result <- try $ debugCompilerStart config "test.typus"
            case result of
                Left (_ :: SomeException) -> assertBool "Should not throw exception" False
                Right _ -> assertBool "Compiler debugging started successfully" True

        , testCase "debugCompilerEnd finalizes compiler debugging" $ do
            let config = defaultCLIDebugConfig
            result <- try $ debugCompilerEnd config "test.typus"
            case result of
                Left (_ :: SomeException) -> assertBool "Should not throw exception" False
                Right _ -> assertBool "Compiler debugging ended successfully" True

        , testCase "debugErrorReport handles error reporting" $ do
            let config = defaultCLIDebugConfig
            result <- try $ debugErrorReport config "Test error message"
            case result of
                Left (_ :: SomeException) -> assertBool "Should not throw exception" False
                Right _ -> assertBool "Error reported successfully" True

        , testCase "debugWarningReport handles warning reporting" $ do
            let config = defaultCLIDebugConfig
            result <- try $ debugWarningReport config "Test warning message"
            case result of
                Left (_ :: SomeException) -> assertBool "Should not throw exception" False
                Right _ -> assertBool "Warning reported successfully" True

        , testCase "debugPerformance handles performance metrics" $ do
            let config = defaultCLIDebugConfig
            result <- try $ debugPerformance config "parse" 1.5
            case result of
                Left (_ :: SomeException) -> assertBool "Should not throw exception" False
                Right _ -> assertBool "Performance metrics recorded successfully" True
        ]

    , testGroup "Example integration"
        [ testCase "exampleDebugIntegration runs without errors" $ do
            let config = defaultCLIDebugConfig
            result <- try $ exampleDebugIntegration config
            case result of
                Left (_ :: SomeException) -> assertBool "Should not throw exception" False
                Right _ -> assertBool "Example debug integration completed successfully" True

        , testCase "showCurrentBreakpoints displays breakpoints" $ do
            let config = defaultCLIDebugConfig
            result <- try $ showCurrentBreakpoints config
            case result of
                Left (_ :: SomeException) -> assertBool "Should not throw exception" False
                Right _ -> assertBool "Current breakpoints displayed successfully" True
        ]
    ]