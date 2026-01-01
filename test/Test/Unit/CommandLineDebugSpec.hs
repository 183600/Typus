module Test.Unit.CommandLineDebugSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, assertBool, assertEqual)
import Test.Tasty.QuickCheck (testProperty, Property, forAll, Gen, arbitrary, elements)
import Test.QuickCheck.Gen (generate)
import Control.Monad (when)
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
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
tests = testGroup "CommandLineDebug Tests"
  [ testDefaultConfig
  , testBreakpointManagement
  , testConditionalBreakpoints
  , testWatchVariables
  , testCallStackManagement
  , testDebugCommands
  , testDebugLevel
  , testRunWithCLIDebug
  , testToggleDebugOutput
  , testExpressionEvaluation
  ]

testDefaultConfig :: TestTree
testDefaultConfig = testCase "Default debug configuration" $ do
  config <- defaultCLIDebugConfig
  enabled <- readIORef (cldEnabled config)
  logLevel <- readIORef (cldLogLevel config)
  breakpoints <- readIORef (cldBreakpoints config)
  interactive <- readIORef (cldInteractive config)
  callStack <- readIORef (cldCallStack config)
  watchVars <- readIORef (cldWatchVariables config)
  stepMode <- readIORef (cldStepMode config)
  currentLocation <- readIORef (cldCurrentLocation config)
  
  assertBool "Debug should be enabled by default" enabled
  assertEqual "Default log level should be 3" 3 logLevel
  assertBool "No breakpoints by default" (Set.null breakpoints)
  assertBool "Interactive mode should be enabled by default" interactive
  assertEqual "Call stack should be empty initially" [] callStack
  assertBool "No watch variables by default" (Map.null watchVars)
  assertBool "Step mode should be disabled by default" (not stepMode)
  assertEqual "Current location should be empty initially" "" currentLocation

testBreakpointManagement :: TestTree
testBreakpointManagement = testCase "Breakpoint management" $ do
  config <- defaultCLIDebugConfig
  
  -- Test setting breakpoints
  setBreakpoint config "test-location-1"
  setBreakpoint config "test-location-2"
  breakpoints1 <- readIORef (cldBreakpoints config)
  assertEqual "Should have 2 breakpoints" 
    (Set.fromList ["test-location-1", "test-location-2"]) breakpoints1
  
  -- Test listing breakpoints
  breakpointList <- listBreakpoints config
  assertEqual "List should contain both breakpoints" 2 (L.length breakpointList)
  assertBool "Should contain first breakpoint" ("test-location-1" `elem` breakpointList)
  assertBool "Should contain second breakpoint" ("test-location-2" `elem` breakpointList)
  
  -- Test clearing breakpoints
  clearBreakpoints config
  breakpoints2 <- readIORef (cldBreakpoints config)
  assertBool "No breakpoints after clearing" (Set.null breakpoints2)

testConditionalBreakpoints :: TestTree
testConditionalBreakpoints = testCase "Conditional breakpoints" $ do
  config <- defaultCLIDebugConfig
  
  -- Set a conditional breakpoint
  setConditionalBreakpoint config "conditional-loc" (== "trigger")
  
  -- Verify the condition is set (indirectly through checkBreakpoint)
  -- This is a basic test since we can't directly inspect conditions
  checkBreakpoint config "conditional-loc"  -- Should not trigger with wrong condition
  
  -- Test with matching condition (simplified test)
  result <- processDebugCommand config "break conditional-loc if trigger"
  -- processDebugCommand doesn't handle "break" commands, so it returns AwaitMoreInput
  assertEqual "Should return AwaitMoreInput for unknown command" AwaitMoreInput result

testWatchVariables :: TestTree
testWatchVariables = testCase "Watch variable management" $ do
  config <- defaultCLIDebugConfig
  
  -- Add watch variables
  addWatchVariable config "var1" "value1"
  addWatchVariable config "var2" "42"
  
  watchVars1 <- readIORef (cldWatchVariables config)
  assertEqual "Should have 2 watch variables" 
    (Map.fromList [("var1", "value1"), ("var2", "42")]) watchVars1
  
  -- List watch variables
  watchList <- listWatchVariables config
  assertEqual "List should contain both variables" 2 (L.length watchList)
  
  -- Remove a watch variable
  removeWatchVariable config "var1"
  watchVars2 <- readIORef (cldWatchVariables config)
  assertEqual "Should have 1 variable after removal" 
    (Map.singleton "var2" "42") watchVars2

testCallStackManagement :: TestTree
testCallStackManagement = testCase "Call stack management" $ do
  config <- defaultCLIDebugConfig
  
  -- Push items to call stack
  pushCallStack config "function1"
  pushCallStack config "function2"
  pushCallStack config "function3"
  
  callStack1 <- getCallStack config
  assertEqual "Should have 3 functions in call stack" 
    ["function3", "function2", "function1"] callStack1
  
  -- Pop from call stack
  popCallStack config
  callStack2 <- getCallStack config
  assertEqual "Should have 2 functions after pop" 
    ["function2", "function1"] callStack2

testDebugCommands :: TestTree
testDebugCommands = testCase "Debug command processing" $ do
  config <- defaultCLIDebugConfig
  
  -- Test basic commands
  result1 <- processDebugCommand config "help"
  assertEqual "Help command should await more input" AwaitMoreInput result1
  
  result2 <- processDebugCommand config "step"
  assertEqual "Step command should resume execution" ResumeExecution result2
  
  result3 <- processDebugCommand config "continue"
  assertEqual "Continue command should resume execution" ResumeExecution result3
  
  result4 <- processDebugCommand config "invalid-command"
  assertBool "Invalid command should return AwaitMoreInput" 
    (case result4 of AwaitMoreInput -> True; _ -> False)

testDebugLevel :: TestTree
testDebugLevel = testCase "Debug level management" $ do
  config <- defaultCLIDebugConfig
  
  -- Set debug level
  setDebugLevel config 5
  logLevel <- readIORef (cldLogLevel config)
  assertEqual "Debug level should be updated" 5 logLevel
  
  -- Test status display
  status <- showDebugStatus config
  assertBool "Status should contain level information" 
    ("level" `elem` L.map (take 5) (words status))

testRunWithCLIDebug :: TestTree
testRunWithCLIDebug = testCase "Running with CLI debug" $ do
  config <- defaultCLIDebugConfig
  let testAction = return "test-result"
  
  result <- runWithCLIDebug config "test-location" testAction
  assertEqual "Action should return expected result" "test-result" result

testToggleDebugOutput :: TestTree
testToggleDebugOutput = testCase "Toggle debug output" $ do
  config <- defaultCLIDebugConfig
  
  -- Initial state should be enabled
  enabled1 <- readIORef (cldEnabled config)
  assertBool "Debug should be enabled initially" enabled1
  
  -- Toggle to disabled
  toggleDebugOutput config
  enabled2 <- readIORef (cldEnabled config)
  assertBool "Debug should be disabled after toggle" (not enabled2)
  
  -- Toggle back to enabled
  toggleDebugOutput config
  enabled3 <- readIORef (cldEnabled config)
  assertBool "Debug should be enabled after second toggle" enabled3

testExpressionEvaluation :: TestTree
testExpressionEvaluation = testCase "Expression evaluation" $ do
  config <- defaultCLIDebugConfig
  
  -- Add a watch variable for testing
  addWatchVariable config "testVar" "123"
  
  -- Evaluate simple expressions
  result1 <- evaluateExpression config "testVar"
  assertBool "Should evaluate existing variable" (not $ null result1)
  
  result2 <- evaluateExpression config "nonExistentVar"
  assertBool "Should handle non-existent variable gracefully" (not $ null result2)

-- QuickCheck property for breakpoint consistency
testBreakpointConsistency :: TestTree
testBreakpointConsistency = testCase "Breakpoint operations are consistent" $ do
  location <- generate arbitraryLocation
  config <- defaultCLIDebugConfig
  setBreakpoint config location
  breakpoints <- listBreakpoints config
  assertBool "Breakpoint should be set" $ location `elem` breakpoints

-- Helper generator for test locations
arbitraryLocation :: Gen String
arbitraryLocation = elements 
  [ "function-start", "function-end", "loop-entry", "condition-check"
  , "variable-decl", "return-statement", "error-handler", "main-entry"
  ]