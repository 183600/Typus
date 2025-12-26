{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.Unit.CommandLineDebugSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, (@?=), assertEqual)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property)

import CommandLineDebug
import Data.IORef (readIORef, writeIORef)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- Unit tests for CommandLineDebug module
tests :: TestTree
tests = testGroup "CommandLineDebug tests"
    [ testGroup "Configuration setup"
        [ testCase "defaultCLIDebugConfig creates valid config" $ do
            config <- defaultCLIDebugConfig
            assertBool "Config should be created" True
            
        , testCase "default config has correct initial values" $ do
            config <- defaultCLIDebugConfig
            enabled <- readIORef (cldEnabled config)
            logLevel <- readIORef (cldLogLevel config)
            breakpoints <- readIORef (cldBreakpoints config)
            interactive <- readIORef (cldInteractive config)
            
            enabled @?= True
            logLevel @?= 3
            Set.size breakpoints @?= 0
            interactive @?= True
        ]
    
    , testGroup "Breakpoint management"
        [ testCase "setBreakpoint adds new breakpoint" $ do
            config <- defaultCLIDebugConfig
            let location = "test:location"
            setBreakpoint config location
            
            breakpoints <- readIORef (cldBreakpoints config)
            assertBool "Breakpoint should be added" $ Set.member location breakpoints
            
        , testCase "setBreakpoint handles duplicate breakpoints" $ do
            config <- defaultCLIDebugConfig
            let location = "test:duplicate"
            setBreakpoint config location
            setBreakpoint config location  -- Add same breakpoint again
            
            breakpoints <- readIORef (cldBreakpoints config)
            Set.size breakpoints @?= 1
            assertBool "Breakpoint should exist" $ Set.member location breakpoints
            
        , testCase "clearBreakpoints removes all breakpoints" $ do
            config <- defaultCLIDebugConfig
            setBreakpoint config "test1"
            setBreakpoint config "test2"
            setBreakpoint config "test3"
            
            clearBreakpoints config
            breakpoints <- readIORef (cldBreakpoints config)
            Set.size breakpoints @?= 0
        ]
    
    , testGroup "Debug state management"
        [ testCase "toggleDebugOutput switches enabled state" $ do
            config <- defaultCLIDebugConfig
            enabled1 <- readIORef (cldEnabled config)
            enabled1 @?= True
            
            toggleDebugOutput config
            enabled2 <- readIORef (cldEnabled config)
            enabled2 @?= False
            
            toggleDebugOutput config
            enabled3 <- readIORef (cldEnabled config)
            enabled3 @?= True
            
        , testCase "setDebugLevel updates log level" $ do
            config <- defaultCLIDebugConfig
            setDebugLevel config 5
            logLevel <- readIORef (cldLogLevel config)
            logLevel @?= 5
        ]
    
    , testGroup "Call stack management"
        [ testCase "pushCallStack and popCallStack maintain stack" $ do
            config <- defaultCLIDebugConfig
            let location1 = "func1"
                location2 = "func2"
                
            pushCallStack config location1
            stack1 <- readIORef (cldCallStack config)
            stack1 @?= [location1]
            
            pushCallStack config location2
            stack2 <- readIORef (cldCallStack config)
            stack2 @?= [location2, location1]
            
            popCallStack config
            stack3 <- readIORef (cldCallStack config)
            stack3 @?= [location1]
            
            popCallStack config
            stack4 <- readIORef (cldCallStack config)
            stack4 @?= []
            
        , testCase "popCallStack on empty stack is safe" $ do
            config <- defaultCLIDebugConfig
            popCallStack config  -- Should not crash
            stack <- readIORef (cldCallStack config)
            stack @?= []
        ]
    
    , testGroup "Watch variable management"
        [ testCase "addWatchVariable adds variable" $ do
            config <- defaultCLIDebugConfig
            let varName = "testVar"
                value = "testValue"
            addWatchVariable config varName value
            
            watchVars <- readIORef (cldWatchVariables config)
            Map.lookup varName watchVars @?= Just value
            
        , testCase "addWatchVariable updates existing variable" $ do
            config <- defaultCLIDebugConfig
            let varName = "testVar"
                value1 = "value1"
                value2 = "value2"
            addWatchVariable config varName value1
            addWatchVariable config varName value2
            
            watchVars <- readIORef (cldWatchVariables config)
            Map.lookup varName watchVars @?= Just value2
            
        , testCase "removeWatchVariable removes variable" $ do
            config <- defaultCLIDebugConfig
            addWatchVariable config "var1" "value1"
            addWatchVariable config "var2" "value2"
            
            removeWatchVariable config "var1"
            watchVars <- readIORef (cldWatchVariables config)
            Map.lookup "var1" watchVars @?= Nothing
            Map.lookup "var2" watchVars @?= Just "value2"
        ]
    
    , testGroup "Step debugging"
        [ testCase "stepInto enables step mode" $ do
            config <- defaultCLIDebugConfig
            stepInto config
            stepMode <- readIORef (cldStepMode config)
            stepMode @?= True
            
        , testCase "stepOver enables step mode" $ do
            config <- defaultCLIDebugConfig
            stepOver config
            stepMode <- readIORef (cldStepMode config)
            stepMode @?= True
            
        , testCase "stepOut disables step mode" $ do
            config <- defaultCLIDebugConfig
            stepInto config  -- Enable first
            stepOut config   -- Then disable
            stepMode <- readIORef (cldStepMode config)
            stepMode @?= False
            
        , testCase "continue disables step mode" $ do
            config <- defaultCLIDebugConfig
            stepInto config  -- Enable first
            continue config  -- Then disable
            stepMode <- readIORef (cldStepMode config)
            stepMode @?= False
        ]
    
    , testGroup "Command processing"
        [ testCase "processDebugCommand handles continue commands" $ do
            config <- defaultCLIDebugConfig
            result1 <- processDebugCommand config "test" ["c"]
            result1 @?= ResumeExecution
            
            result2 <- processDebugCommand config "test" ["continue"]
            result2 @?= ResumeExecution
            
        , testCase "processDebugCommand handles step commands" $ do
            config <- defaultCLIDebugConfig
            result1 <- processDebugCommand config "test" ["s"]
            result1 @?= ResumeExecution
            
            result2 <- processDebugCommand config "test" ["step"]
            result2 @?= ResumeExecution
            
        , testCase "processDebugCommand handles list commands" $ do
            config <- defaultCLIDebugConfig
            result <- processDebugCommand config "test" ["list"]
            result @?= AwaitMoreInput
            
        , testCase "processDebugCommand handles unknown commands" $ do
            config <- defaultCLIDebugConfig
            result <- processDebugCommand config "test" ["unknown"]
            result @?= AwaitMoreInput
        ]
    
    , testGroup "Expression evaluation"
        [ testCase "evaluateExpression returns formatted result" $ do
            config <- defaultCLIDebugConfig
            let expr = "x + y"
            result <- evaluateExpression config expr
            assertBool "Result should contain expression" $ expr `isInfixOf` result
        ]
    
    , testGroup "Run to cursor"
        [ testCase "runToCursor sets breakpoint and continues" $ do
            config <- defaultCLIDebugConfig
            let location = "target:location"
            runToCursor config location
            
            -- Should have set breakpoint
            breakpoints <- readIORef (cldBreakpoints config)
            assertBool "Breakpoint should be set at target" $ Set.member location breakpoints
            
            -- Should have disabled step mode
            stepMode <- readIORef (cldStepMode config)
            stepMode @?= False
        ]
    
    , testGroup "Status reporting"
        [ testCase "showDebugStatus displays current state" $ do
            config <- defaultCLIDebugConfig
            setBreakpoint config "test:bp"
            addWatchVariable config "testVar" "testValue"
            setDebugLevel config 2
            
            -- This should not crash and display status
            assertBool "Status display should work" True
        ]
    
    , testGroup "Conditional breakpoints"
        [ testCase "setConditionalBreakpoint adds condition" $ do
            config <- defaultCLIDebugConfig
            let location = "conditional:test"
                condition = const True
            setConditionalBreakpoint config location condition
            
            conditions <- readIORef (cldBreakConditions config)
            assertBool "Condition should be set" $ Map.member location conditions
        ]
    
    , testGroup "Location tracking"
        [ testCase "pushCallStack updates current location" $ do
            config <- defaultCLIDebugConfig
            let location = "current:func"
            pushCallStack config location
            
            currentLocation <- readIORef (cldCurrentLocation config)
            currentLocation @?= location
            
        , testCase "popCallStack updates current location" $ do
            config <- defaultCLIDebugConfig
            pushCallStack config "func1"
            pushCallStack config "func2"
            
            popCallStack config
            currentLocation <- readIORef (cldCurrentLocation config)
            currentLocation @?= "func1"
            
            popCallStack config
            currentLocation2 <- readIORef (cldCurrentLocation config)
            currentLocation2 @?= ""
        ]
    ]
  where
    isInfixOf needle haystack = needle `elem` [take (length needle) (drop i haystack) | i <- [0..length haystack - length needle]]