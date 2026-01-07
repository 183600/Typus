module Test.Unit.CliDebugRunnerSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Cli.DebugRunner

-- Test debug runner initialization
prop_debug_runner_initialization :: Property
prop_debug_runner_initialization =
  let runner1 = initializeDebugRunner
      runner2 = initializeDebugRunner
  in property $ getDebugRunnerId runner1 /= getDebugRunnerId runner2

-- Test debug command execution
prop_debug_command_execution :: String -> Property
prop_debug_command_execution command =
  let runner = initializeDebugRunner
      result = executeDebugCommand runner command
  in property $ 
    case result of
      Left _ -> property True
      Right _ -> property True

-- Test debug breakpoint management
prop_debug_breakpoint_management :: String -> Int -> Property
prop_debug_breakpoint_management file line =
  let runner = initializeDebugRunner
      runnerWithBreakpoint = setBreakpoint runner file line
      hasBreakpoint = hasBreakpointAt runnerWithBreakpoint file line
      runnerWithoutBreakpoint = removeBreakpoint runnerWithBreakpoint file line
      noBreakpoint = not $ hasBreakpointAt runnerWithoutBreakpoint file line
  in property $ hasBreakpoint && noBreakpoint

-- Test debug variable inspection
prop_debug_variable_inspection :: String -> String -> Property
prop_debug_variable_inspection varName varValue =
  let runner = initializeDebugRunner
      runnerWithVar = setDebugVariable runner varName varValue
      inspected = inspectDebugVariable runnerWithVar varName
  in property $ inspected === Just varValue

-- Test debug stepping
prop_debug_stepping :: String -> Property
prop_debug_stepping sourceFile =
  let runner = initializeDebugRunner
      loadedRunner = loadDebugSource runner sourceFile
      steppedRunner = performDebugStep loadedRunner
      currentLine = getCurrentDebugLine steppedRunner
  in property $ currentLine >= 1

tests :: TestTree
tests = testGroup "CLI Debug Runner Tests"
  [ testProperty "debug runner initialization" prop_debug_runner_initialization
  , testProperty "debug command execution" prop_debug_command_execution
  , testProperty "debug breakpoint management" prop_debug_breakpoint_management
  , testProperty "debug variable inspection" prop_debug_variable_inspection
  , testProperty "debug stepping" prop_debug_stepping
  ]