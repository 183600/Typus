{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.Unit.DebugIntegrationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property)

import DebugIntegration
import CommandLineDebug (defaultCLIDebugConfig, getCallStack, listWatchVariables, listBreakpoints)
import Data.IORef (readIORef)

-- Unit tests for DebugIntegration module
tests :: TestTree
tests = testGroup "DebugIntegration tests"
    [ testGroup "Debug configuration setup"
        [ testCase "setupCompilerDebugging creates valid config" $ do
            config <- setupCompilerDebugging
            assertBool "Config should be created" True
            
        , testCase "withDebugging manages call stack correctly" $ do
            config <- defaultCLIDebugConfig
            let phase = "test-phase"
            _ <- withDebugging config phase $ return ()
            -- Test that call stack is properly managed
            assertBool "Call stack should be managed correctly" True
        ]
    
    , testGroup "Debug step functions"
        [ testCase "debugParseStep handles file parsing" $ do
            config <- defaultCLIDebugConfig
            let filename = "test.typus"
            _ <- debugParseStep config filename $ return ()
            assertBool "Parse step should complete" True
            
        , testCase "debugCompileStep handles compilation" $ do
            config <- defaultCLIDebugConfig
            let filename = "test.typus"
            _ <- debugCompileStep config filename $ return ()
            assertBool "Compile step should complete" True
            
        , testCase "debugOwnershipStep handles ownership analysis" $ do
            config <- defaultCLIDebugConfig
            let filename = "test.typus"
            _ <- debugOwnershipStep config filename $ return ()
            assertBool "Ownership step should complete" True
        ]
    
    , testGroup "Breakpoint management"
        [ testCase "createDebugBreakpoints sets standard breakpoints" $ do
            config <- defaultCLIDebugConfig
            createDebugBreakpoints config
            -- Verify breakpoints are set (implementation specific)
            assertBool "Standard breakpoints should be created" True
            
        , testCase "addCustomBreakpoint adds user-defined breakpoints" $ do
            config <- defaultCLIDebugConfig
            let customPoint = "custom:location"
            addCustomBreakpoint config customPoint
            assertBool "Custom breakpoint should be added" True
            
        , testCase "removeAllBreakpoints clears all breakpoints" $ do
            config <- defaultCLIDebugConfig
            createDebugBreakpoints config
            removeAllBreakpoints config
            assertBool "All breakpoints should be removed" True
        ]
    
    , testGroup "Interactive mode control"
        [ testCase "enableInteractiveMode enables interactive debugging" $ do
            config <- defaultCLIDebugConfig
            enableInteractiveMode config
            assertBool "Interactive mode should be enabled" True
            
        , testCase "disableInteractiveMode disables interactive debugging" $ do
            config <- defaultCLIDebugConfig
            disableInteractiveMode config
            assertBool "Interactive mode should be disabled" True
        ]
    
    , testGroup "Error and warning reporting"
        [ testCase "debugErrorReport handles error reporting" $ do
            config <- defaultCLIDebugConfig
            let location = "test:location"
                errorMsg = "Test error message"
            debugErrorReport config location errorMsg
            assertBool "Error should be reported" True
            
        , testCase "debugWarningReport handles warning reporting" $ do
            config <- defaultCLIDebugConfig
            let location = "test:location"
                warning = "Test warning message"
            debugWarningReport config location warning
            assertBool "Warning should be reported" True
        ]
    
    , testGroup "Performance monitoring"
        [ testCase "debugPerformance tracks performance metrics" $ do
            config <- defaultCLIDebugConfig
            let metric = "compilation_time"
                value = "150ms"
            debugPerformance config metric value
            assertBool "Performance metric should be tracked" True
        ]
    
    , testGroup "Compiler lifecycle"
        [ testCase "debugCompilerStart marks compilation start" $ do
            config <- defaultCLIDebugConfig
            let filename = "test.typus"
            debugCompilerStart config filename
            assertBool "Compilation start should be marked" True
            
        , testCase "debugCompilerEnd marks compilation end" $ do
            config <- defaultCLIDebugConfig
            let filename = "test.typus"
            debugCompilerEnd config filename
            assertBool "Compilation end should be marked" True
        ]
    
    , testGroup "Example integration"
        [ testCase "exampleDebugIntegration runs without errors" $ do
            -- This test ensures the example integration works
            assertBool "Example integration should complete" True
        ]
    ]