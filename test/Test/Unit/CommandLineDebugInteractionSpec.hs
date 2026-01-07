module Test.Unit.CommandLineDebugInteractionSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import CommandLineDebug

-- Test command line parsing
prop_cli_parsing_consistent :: String -> Property
prop_cli_parsing_consistent commandLine =
  let parsed1 = parseCommandLine commandLine
      parsed2 = parseCommandLine commandLine
  in property $ parsed1 === parsed2

-- Test debug flag handling
prop_debug_flag_activation :: [String] -> Property
prop_debug_flag_activation flags =
  let config = parseDebugFlags flags
      debugEnabled = isDebugEnabled config
  in property $ 
    if "--debug" `elem` flags || "-d" `elem` flags
    then debugEnabled
    else not debugEnabled

-- Test verbosity level setting
prop_verbosity_level_setting :: Int -> Property
prop_verbosity_level_setting level =
  let config = setVerbosityLevel level
      actualLevel = getVerbosityLevel config
  in property $ actualLevel === max 0 (min 5 level)

-- Test debug output format
prop_debug_output_format :: DebugConfig -> String -> Property
prop_debug_output_format config message =
  let formatted = formatDebugMessage config message
  in property $ message `isInfixOf` formatted

-- Test debug interaction history
prop_debug_history_preservation :: [String] -> Property
prop_debug_history_preservation commands =
  let history = processDebugCommands commands
      preserved = getDebugHistory history
  in property $ length preserved === length commands

tests :: TestTree
tests = testGroup "CommandLineDebug Interaction Tests"
  [ testProperty "CLI parsing consistent" prop_cli_parsing_consistent
  , testProperty "debug flag activation" prop_debug_flag_activation
  , testProperty "verbosity level setting" prop_verbosity_level_setting
  , testProperty "debug output format" prop_debug_output_format
  , testProperty "debug history preservation" prop_debug_history_preservation
  ]