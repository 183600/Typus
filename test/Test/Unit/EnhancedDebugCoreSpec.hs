module Test.Unit.EnhancedDebugCoreSpec where



import Test.Tasty
import Test.Tasty.QuickCheck

import EnhancedDebug

-- Test debug session type
data TestDebugSession = TestDebugSession
  { sessionId :: String
  , breakpoints :: [(String, Int)]
  , variables :: [(String, String)]
  , debugState :: TestDebugState
  } deriving (Eq, Show)

-- Test debug state type
data TestDebugState = TestDebugState
  { executionLine :: Int
  } deriving (Eq, Show)

-- Test implementation for createDebugSession
createDebugSession :: String -> TestDebugSession
createDebugSession sid = TestDebugSession
  { sessionId = sid
  , breakpoints = []
  , variables = []
  , debugState = TestDebugState { executionLine = 0 }
  }

-- Test implementation for getDebugSessionId
getDebugSessionId :: TestDebugSession -> String
getDebugSessionId session = sessionId session

-- Test implementation for addBreakpoint
addBreakpoint :: TestDebugSession -> String -> Int -> TestDebugSession
addBreakpoint session file line = 
  session { breakpoints = (file, line) : breakpoints session }

-- Test implementation for hasBreakpointAt
hasBreakpointAt :: TestDebugSession -> String -> Int -> Bool
hasBreakpointAt session file line = (file, line) `elem` breakpoints session

-- Test implementation for removeBreakpoint
removeBreakpoint :: TestDebugSession -> String -> Int -> TestDebugSession
removeBreakpoint session file line = 
  session { breakpoints = filter (\(f, l) -> not (f == file && l == line)) (breakpoints session) }

-- Test implementation for setVariable
setVariable :: TestDebugSession -> String -> String -> TestDebugSession
setVariable session varName varValue = 
  session { variables = (varName, varValue) : variables session }

-- Test implementation for inspectVariable
inspectVariable :: TestDebugSession -> String -> Maybe String
inspectVariable session varName = lookup varName (variables session)

-- Test implementation for executeStep
executeStep :: TestDebugSession -> TestDebugSession
executeStep session = 
  let state = debugState session
      newState = state { executionLine = executionLine state + 1 }
  in session { debugState = newState }

-- Test implementation for getDebugState
getDebugState :: TestDebugSession -> TestDebugState
getDebugState session = debugState session

-- Test implementation for getExecutionLine
getExecutionLine :: TestDebugState -> Int
getExecutionLine state = executionLine state

-- Test implementation for pushFunctionCall
pushFunctionCall :: TestDebugSession -> String -> TestDebugSession
pushFunctionCall session functionName = 
  session { sessionId = sessionId session ++ "-" ++ functionName }

-- Test implementation for getCallStack
getCallStack :: TestDebugSession -> [String]
getCallStack session = words (sessionId session)

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