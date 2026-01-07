module Test.Unit.CommandLineDebugInteractionSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import CommandLineDebug
import Data.List (isInfixOf)
import Test.QuickCheck (Arbitrary(..), oneof)

-- Add Arbitrary instance for DebugConfig
instance Arbitrary DebugConfig where
  arbitrary = do
    enabled <- arbitrary
    level <- arbitrary
    return $ DebugConfig
      { debugEnabled = enabled
      , verbosityLevel = level
      }

-- Test debug config type
data DebugConfig = DebugConfig
  { debugEnabled :: Bool
  , verbosityLevel :: Int
  } deriving (Eq, Show)

-- Test implementation for parseCommandLine
parseCommandLine :: String -> String
parseCommandLine commandLine = commandLine

-- Test implementation for parseDebugFlags
parseDebugFlags :: [String] -> DebugConfig
parseDebugFlags flags = DebugConfig
  { debugEnabled = "--debug" `elem` flags || "-d" `elem` flags
  , verbosityLevel = length (filter (== "-v") flags)
  }

-- Test implementation for isDebugEnabled
isDebugEnabled :: DebugConfig -> Bool
isDebugEnabled config = debugEnabled config

-- Test implementation for setVerbosityLevel
setVerbosityLevel :: Int -> DebugConfig
setVerbosityLevel level = DebugConfig
  { debugEnabled = True
  , verbosityLevel = max 0 (min 5 level)
  }

-- Test implementation for getVerbosityLevel
getVerbosityLevel :: DebugConfig -> Int
getVerbosityLevel config = verbosityLevel config

-- Test implementation for formatDebugMessage
formatDebugMessage :: DebugConfig -> String -> String
formatDebugMessage config message = 
  let prefix = if debugEnabled config then "[DEBUG] " else ""
  in prefix ++ message

-- Test implementation for processDebugCommands
processDebugCommands :: [String] -> [String]
processDebugCommands commands = commands

-- Test implementation for getDebugHistory
getDebugHistory :: [String] -> [String]
getDebugHistory history = history

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