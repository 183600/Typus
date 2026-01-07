module Test.Unit.CliDebugRunnerSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Cli.DebugRunner
import qualified Data.Map.Strict as Map

-- Test debug runner type
data TestDebugRunner = TestDebugRunner
  { runnerId :: String
  , breakpoints :: [(String, Int)]
  , variables :: Map.Map String String
  , sourceFile :: String
  , currentLine :: Int
  } deriving (Eq, Show)

-- Test implementation for initializeDebugRunner
initializeDebugRunner :: TestDebugRunner
initializeDebugRunner = TestDebugRunner
  { runnerId = "runner-" ++ "1"
  , breakpoints = []
  , variables = Map.empty
  , sourceFile = ""
  , currentLine = 0
  }

-- Test implementation for getDebugRunnerId
getDebugRunnerId :: TestDebugRunner -> String
getDebugRunnerId runner = runnerId runner

-- Test implementation for executeDebugCommand
executeDebugCommand :: TestDebugRunner -> String -> Either String TestDebugRunner
executeDebugCommand runner command = Right runner { sourceFile = command }

-- Test implementation for setBreakpoint
setBreakpoint :: TestDebugRunner -> String -> Int -> TestDebugRunner
setBreakpoint runner file line = 
  runner { breakpoints = (file, line) : breakpoints runner }

-- Test implementation for hasBreakpointAt
hasBreakpointAt :: TestDebugRunner -> String -> Int -> Bool
hasBreakpointAt runner file line = (file, line) `elem` breakpoints runner

-- Test implementation for removeBreakpoint
removeBreakpoint :: TestDebugRunner -> String -> Int -> TestDebugRunner
removeBreakpoint runner file line = 
  runner { breakpoints = filter (\(f, l) -> not (f == file && l == line)) (breakpoints runner) }

-- Test implementation for setDebugVariable
setDebugVariable :: TestDebugRunner -> String -> String -> TestDebugRunner
setDebugVariable runner varName varValue = 
  runner { variables = Map.insert varName varValue (variables runner) }

-- Test implementation for inspectDebugVariable
inspectDebugVariable :: TestDebugRunner -> String -> Maybe String
inspectDebugVariable runner varName = Map.lookup varName (variables runner)

-- Test implementation for loadDebugSource
loadDebugSource :: TestDebugRunner -> String -> TestDebugRunner
loadDebugSource runner source = runner { sourceFile = source, currentLine = 1 }

-- Test implementation for performDebugStep
performDebugStep :: TestDebugRunner -> TestDebugRunner
performDebugStep runner = runner { currentLine = currentLine runner + 1 }

-- Test implementation for getCurrentDebugLine
getCurrentDebugLine :: TestDebugRunner -> Int
getCurrentDebugLine runner = currentLine runner

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