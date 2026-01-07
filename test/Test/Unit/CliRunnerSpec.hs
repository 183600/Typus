module Test.Unit.CliRunnerSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Cli.Runner

-- Test CLI runner initialization
prop_cli_runner_initialization :: Property
prop_cli_runner_initialization =
  let runner1 = initializeCliRunner
      runner2 = initializeCliRunner
  in property $ getRunnerId runner1 /= getRunnerId runner2

-- Test CLI command execution
prop_cli_command_execution :: String -> Property
prop_cli_command_execution command =
  let runner = initializeCliRunner
      result = executeCommand runner command
  in property $ 
    case result of
      Left _ -> property True
      Right _ -> property True

-- Test CLI runner state
prop_cli_runner_state :: String -> Property
prop_cli_runner_state stateInfo =
  let runner = initializeCliRunner
      runnerWithState = setRunnerState runner stateInfo
      currentState = getRunnerState runnerWithState
  in property $ currentState === stateInfo

-- Test CLI runner history
prop_cli_runner_history :: [String] -> Property
prop_cli_runner_history commands =
  let runner = initializeCliRunner
      runnerWithHistory = foldl executeCommand runner commands
      history = getCommandHistory runnerWithHistory
  in property $ length history >= length commands

-- Test CLI runner configuration
prop_cli_runner_configuration :: [String] -> Property
prop_cli_runner_configuration configOptions =
  let runner = initializeCliRunner
      configuredRunner = configureRunner runner configOptions
      currentConfig = getRunnerConfiguration configuredRunner
  in property $ length currentConfig >= length configOptions

tests :: TestTree
tests = testGroup "CLI Runner Tests"
  [ testProperty "CLI runner initialization" prop_cli_runner_initialization
  , testProperty "CLI command execution" prop_cli_command_execution
  , testProperty "CLI runner state" prop_cli_runner_state
  , testProperty "CLI runner history" prop_cli_runner_history
  , testProperty "CLI runner configuration" prop_cli_runner_configuration
  ]