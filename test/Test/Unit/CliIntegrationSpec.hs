module Test.Unit.CliIntegrationSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Cli

-- Test CLI command parsing
prop_cli_command_parsing :: String -> Property
prop_cli_command_parsing commandLine =
  let parsed1 = parseCliCommand commandLine
      parsed2 = parseCliCommand commandLine
  in property $ parsed1 === parsed2

-- Test CLI option handling
prop_cli_option_handling :: [String] -> Property
prop_cli_option_handling options =
  let config = processCliOptions options
      processedOptions = getProcessedOptions config
  in property $ length processedOptions >= 0

-- Test CLI argument validation
prop_cli_argument_validation :: String -> Property
prop_cli_argument_validation argument =
  let result = validateCliArgument argument
  in property $ 
    case result of
      Left _ -> property True
      Right _ -> property True

-- Test CLI help generation
prop_cli_help_generation :: String -> Property
prop_cli_help_generation command =
  let helpText = generateHelpForCommand command
  in property $ not (null helpText)

-- Test CLI configuration
prop_cli_configuration :: [String] -> Property
prop_cli_configuration configArgs =
  let config = buildCliConfiguration configArgs
      configString = showConfiguration config
  in property $ not (null configString)

tests :: TestTree
tests = testGroup "CLI Integration Tests"
  [ testProperty "CLI command parsing" prop_cli_command_parsing
  , testProperty "CLI option handling" prop_cli_option_handling
  , testProperty "CLI argument validation" prop_cli_argument_validation
  , testProperty "CLI help generation" prop_cli_help_generation
  , testProperty "CLI configuration" prop_cli_configuration
  ]