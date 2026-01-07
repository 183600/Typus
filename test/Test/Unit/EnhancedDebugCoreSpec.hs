module Test.Unit.EnhancedDebugCoreSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import EnhancedDebug

-- Test enhanced debug session creation
prop_debug_session_creation :: String -> Property
prop_debug_session_creation sessionId =
  let session1 = createDebugSession sessionId
      session2 = createDebugSession sessionId
  in property $ getDebugSessionId session1 === getDebugSessionId session2

-- Test debug breakpoint management
prop_breakpoint_management :: String -> Int -> Property
prop_breakpoint_management file line =
  let session = createDebugSession "test"
      sessionWithBreakpoint = addBreakpoint session file line
      hasBreakpoint = hasBreakpointAt sessionWithBreakpoint file line
      sessionWithoutBreakpoint = removeBreakpoint sessionWithBreakpoint file line
      noBreakpoint = not $ hasBreakpointAt sessionWithoutBreakpoint file line
  in property $ hasBreakpoint && noBreakpoint

-- Test debug variable inspection
prop_variable_inspection :: String -> String -> Property
prop_variable_inspection varName varValue =
  let session = createDebugSession "test"
      sessionWithVar = setVariable session varName varValue
      inspected = inspectVariable sessionWithVar varName
  in property $ inspected === Just varValue

-- Test debug step execution
prop_step_execution_preserves_state :: String -> Property
prop_step_execution_preserves_state sessionId =
  let session = createDebugSession sessionId
      stepped = executeStep session
      originalState = getDebugState session
      steppedState = getDebugState stepped
  in property $ getExecutionLine steppedState >= getExecutionLine originalState

-- Test debug call stack tracking
prop_call_stack_tracking :: [String] -> Property
prop_call_stack_tracking functionNames =
  let session = createDebugSession "test"
      sessionWithStack = foldl pushFunctionCall session functionNames
      stack = getCallStack sessionWithStack
  in property $ stack === reverse functionNames

tests :: TestTree
tests = testGroup "EnhancedDebug Core Tests"
  [ testProperty "debug session creation" prop_debug_session_creation
  , testProperty "breakpoint management" prop_breakpoint_management
  , testProperty "variable inspection" prop_variable_inspection
  , testProperty "step execution preserves state" prop_step_execution_preserves_state
  , testProperty "call stack tracking" prop_call_stack_tracking
  ]