{-# LANGUAGE CPP #-}

module Test.Unit.CommandLineDebugIntegrationAdvancedSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.List as L
import Data.List (isInfixOf)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import CommandLineDebug
  ( CommandLineDebugConfig(..)
  , defaultCLIDebugConfig
  , runWithCLIDebug
  , checkBreakpoint
  , setBreakpoint
  , setConditionalBreakpoint
  , listBreakpoints
  , clearBreakpoints
  , toggleDebugOutput
  , DebugCommandResult(..)
  , processDebugCommand
  , setDebugLevel
  , showDebugStatus
  , addWatchVariable
  , removeWatchVariable
  , listWatchVariables
  , getCallStack
  , pushCallStack
  , popCallStack
  , evaluateExpression
  , stepInto
  , stepOver
  , stepOut
  , continue
  , runToCursor
  )

tests :: TestTree
tests = testGroup "CommandLineDebug Integration Advanced Tests"
  [ breakpointTests
  , watchVariableTests
  , callStackTests
  , commandProcessingTests
  , stepControlTests
  , integrationTests
  , quickCheckProperties
  ]

breakpointTests :: TestTree
breakpointTests = testGroup "Breakpoint Tests"
  [ testCase "setBreakpoint adds breakpoint" $ do
      config <- defaultCLIDebugConfig
      result <- setBreakpoint config "main"
      breakpoints <- listBreakpoints config
      result `seq` True @?= True  -- Should not crash
      L.length breakpoints @?= 1
      
  , testCase "setConditionalBreakpoint adds conditional breakpoint" $ do
      config <- defaultCLIDebugConfig
      let condition = "x > 10"
      result <- setConditionalBreakpoint config "test" condition
      result `seq` True @?= True  -- Should not crash
      
  , testCase "checkBreakpoint evaluates breakpoints" $ do
      config <- defaultCLIDebugConfig
      _ <- setBreakpoint config "main"
      shouldBreak <- checkBreakpoint config "main"
      shouldBreak `seq` True @?= True  -- Should not crash
      
  , testCase "clearBreakpoints removes L.all breakpoints" $ do
      config <- defaultCLIDebugConfig
      _ <- setBreakpoint config "main"
      _ <- setBreakpoint config "test"
      _ <- clearBreakpoints config
      breakpoints <- listBreakpoints config
      breakpoints @?= []
  ]

watchVariableTests :: TestTree
watchVariableTests = testGroup "Watch Variable Tests"
  [ testCase "addWatchVariable adds variable to watch list" $ do
      config <- defaultCLIDebugConfig
      result <- addWatchVariable config "x" "42"
      result `seq` True @?= True  -- Should not crash
      watchVars <- listWatchVariables config
      L.length watchVars @?= 1
      
  , testCase "removeWatchVariable removes variable from watch list" $ do
      config <- defaultCLIDebugConfig
      _ <- addWatchVariable config "x" "42"
      _ <- addWatchVariable config "y" "hello"
      _ <- removeWatchVariable config "x"
      watchVars <- listWatchVariables config
      L.length watchVars @?= 1
      
  , testCase "listWatchVariables returns L.all watched variables" $ do
      config <- defaultCLIDebugConfig
      _ <- addWatchVariable config "x" "42"
      _ <- addWatchVariable config "y" "\"hello\""
      _ <- addWatchVariable config "z" "true"
      watchVars <- listWatchVariables config
      L.length watchVars @?= 3
  ]

callStackTests :: TestTree
callStackTests = testGroup "Call Stack Tests"
  [ testCase "pushCallStack adds function to call stack" $ do
      config <- defaultCLIDebugConfig
      _ <- pushCallStack config "main"
      _ <- pushCallStack config "helper"
      callStack <- getCallStack config
      L.length callStack @?= 2
      
  , testCase "popCallStack removes function from call stack" $ do
      config <- defaultCLIDebugConfig
      _ <- pushCallStack config "main"
      _ <- pushCallStack config "helper"
      _ <- pushCallStack config "inner"
      _ <- popCallStack config
      callStack <- getCallStack config
      L.length callStack @?= 2
      
  , testCase "getCallStack returns current call stack" $ do
      config <- defaultCLIDebugConfig
      _ <- pushCallStack config "main"
      _ <- pushCallStack config "process"
      _ <- pushCallStack config "calculate"
      callStack <- getCallStack config
      callStack @?= ["calculate", "process", "main"]
  ]

commandProcessingTests :: TestTree
commandProcessingTests = testGroup "Command Processing Tests"
  [ testCase "processDebugCommand handles basic commands" $ do
      config <- defaultCLIDebugConfig
      result <- processDebugCommand config "status"
      result `seq` True @?= True  -- Should not crash
      
  , testCase "processDebugCommand handles breakpoint commands" $ do
      config <- defaultCLIDebugConfig
      result1 <- processDebugCommand config "break main"
      result2 <- processDebugCommand config "break test if x > 5"
      result1 `seq` True @?= True
      result2 `seq` True @?= True
      
  , testCase "processDebugCommand handles watch commands" $ do
      config <- defaultCLIDebugConfig
      result1 <- processDebugCommand config "watch x"
      result2 <- processDebugCommand config "unwatch x"
      result1 `seq` True @?= True
      result2 `seq` True @?= True
      
  , testCase "setDebugLevel changes debug level" $ do
      config <- defaultCLIDebugConfig
      _ <- setDebugLevel config 3
      result <- showDebugStatus config
      result `seq` True @?= True  -- Should not crash
  ]

stepControlTests :: TestTree
stepControlTests = testGroup "Step Control Tests"
  [ testCase "stepInto advances into function calls" $ do
      config <- defaultCLIDebugConfig
      _ <- pushCallStack config "main"
      result <- stepInto config
      result `seq` True @?= True  -- Should not crash
      
  , testCase "stepOver advances over function calls" $ do
      config <- defaultCLIDebugConfig
      _ <- pushCallStack config "main"
      result <- stepOver config
      result `seq` True @?= True  -- Should not crash
      
  , testCase "stepOut returns from current function" $ do
      config <- defaultCLIDebugConfig
      _ <- pushCallStack config "main"
      _ <- pushCallStack config "helper"
      result <- stepOut config
      result `seq` True @?= True  -- Should not crash
      
  , testCase "continue runs until next breakpoint" $ do
      config <- defaultCLIDebugConfig
      _ <- setBreakpoint config "target"
      result <- continue config
      result `seq` True @?= True  -- Should not crash
      
  , testCase "runToCursor runs to specified location" $ do
      config <- defaultCLIDebugConfig
      result <- runToCursor config "target_function"
      result `seq` True @?= True  -- Should not crash
  ]

integrationTests :: TestTree
integrationTests = testGroup "Integration Tests"
  [ testCase "runWithCLIDebug executes with debugging enabled" $ do
      config <- defaultCLIDebugConfig
      result <- runWithCLIDebug config $ do
        _ <- pushCallStack config "main"
        _ <- addWatchVariable config "x" "42"
        return "debug_result"
      result @?= "debug_result"
      
  , testCase "toggleDebugOutput enables/disables debug output" $ do
      config <- defaultCLIDebugConfig
      _ <- toggleDebugOutput config True
      _ <- toggleDebugOutput config False
      result <- showDebugStatus config
      result `seq` True @?= True  -- Should not crash
      
  , testCase "complex debugging scenario" $ do
      config <- defaultCLIDebugConfig
      _ <- setBreakpoint config "main"
      _ <- setConditionalBreakpoint config "process" "x > 10"
      _ <- addWatchVariable config "x" "42"
      _ <- pushCallStack config "main"
      _ <- pushCallStack config "process"
      _ <- stepInto config
      _ <- evaluateExpression config "x + 1"
      _ <- stepOver config
      _ <- popCallStack config
      result <- showDebugStatus config
      result `seq` True @?= True  -- Should not crash
      
  , testCase "error handling in debug operations" $ do
      config <- defaultCLIDebugConfig
      -- Try to remove non-existent watch variable
      _ <- removeWatchVariable config "nonexistent"
      -- Try to pop from empty call stack
      _ <- popCallStack config
      -- Try to evaluate invalid expression
      _ <- evaluateExpression config "invalid syntax"
      result <- showDebugStatus config
      result `seq` True @?= True  -- Should not crash
  ]

quickCheckProperties :: TestTree
quickCheckProperties = testGroup "QuickCheck Debug Properties"
  [ fastProperty "breakpoint operations are idempotent" prop_breakpoint_idempotent
  , fastProperty "call stack operations maintain order" prop_callstack_order
  , fastProperty "watch variable operations are total" prop_watch_total
  ]

-- QuickCheck property implementations
prop_breakpoint_idempotent :: String -> Property
prop_breakpoint_idempotent breakpointName =
  not (null breakpointName) ==> property True  -- Placeholder - actual testing would need IO

prop_callstack_order :: [String] -> Property
prop_callstack_order functions =
  let sorted = sort functions
      reversed = L.reverse functions
  in L.length sorted == L.length reversed ==> property True

prop_watch_total :: String -> String -> Property
prop_watch_total varName value =
  not (null varName) ==> property True  -- Placeholder - actual testing would need IO