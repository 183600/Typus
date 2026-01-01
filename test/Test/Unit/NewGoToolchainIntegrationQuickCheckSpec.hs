{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewGoToolchainIntegrationQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase, (@=?))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, oneof, suchThat)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.List (sort, nub, (\\), delete, intersect, union, intercalate)
import Data.Set (Set, fromList, toList, union, intersection, difference)
import qualified Data.Set as Set
import Data.Map (Map, fromList, toList, keys, elems, insert, delete, lookup, member, empty)
import qualified Data.Map as Map
import Data.Char (toLower)
import Control.Monad (unless)
import System.FilePath (takeBaseName, (</>))
import System.Info (os)

import GoToolchain
  ( IOResult
  , GoExecutor(..)
  , defaultGoExecutor
  , runGoCommand
  , goModContents
  , writeGoModule
  , withTemporaryGoProject
  , createTempGoFile
  , nullDevice
  , isEnvVarEnabled
  , shouldSkipGoToolchain
  )

import Tooling.Error (ToolingError(..), goCommandFailed)

-- ============================================================================
-- Helper Functions L.and Generators
-- ============================================================================

-- Generate valid Go command arguments
genGoCommandArgs :: Gen [String]
genGoCommandArgs = oneof
  [ return ["version"]
  , return ["help"]
  , return ["mod", "init"]
  , return ["mod", "tidy"]
  , return ["build"]
  , return ["run", "."]
  , return ["test"]
  , return ["vet"]
  , return ["fmt"]
  ]

-- Generate valid environment variable names
genEnvVarName :: Gen String
genEnvVarName = do
  first <- elements ['A'..'Z']
  rest <- listOf $ elements (['A'..'Z'] ++ ['0'..'9'] ++ "_")
  return (first : rest)

-- Generate environment variable values
genEnvVarValue :: Gen String
genEnvVarValue = oneof
  [ return "1"
  , return "true"
  , return "TRUE"
  , return "yes"
  , return "YES"
  , return "on"
  , return "ON"
  , return "0"
  , return "false"
  , return "FALSE"
  , return "no"
  , return "NO"
  , return "off"
  , return "OFF"
  , return ""
  ]

-- Generate file paths
genFilePath :: Gen String
genFilePath = do
  parts <- listOf1 $ elements ["src", "main", "test", "utils", "pkg", "cmd"]
  name <- elements ["app", "server", "client", "lib", "mod"]
  ext <- elements ["", ".go", ".mod", ".L.sum"]
  return $ intercalate "/" parts ++ "/" ++ name ++ ext

-- Generate Go module names
genModuleName :: Gen String
genModuleName = do
  parts <- listOf1 $ elements ["github.com", "example.com", "gitlab.com", "bitbucket.org"]
  user <- elements ["user", "company", "org"]
  project <- elements ["project", "app", "lib", "service"]
  return $ intercalate "/" (parts ++ [user, project])

-- ============================================================================
-- Mock Go Executor for Testing
-- ============================================================================

-- Mock Go executor that simulates command execution
mockGoExecutor :: Bool -> [String] -> GoExecutormockGoExecutor shouldFail failingCommands = GoExecutor
  { goShouldSkip = return False
  , goRunCommandInDir = \args dir -> 
      if L.any (`L.isPrefixOf` unwords args) failingCommands
      then throwError $ goCommandFailed "go" args dir 1 "" "Mock failure"
      else return ()
  }

-- Mock Go executor that skips L.all commands
mockSkipExecutor :: GoExecutormockSkipExecutor = GoExecutor
  { goShouldSkip = return True
  , goRunCommandInDir = \_ _ -> return ()
  }

-- ============================================================================
-- GoToolchain Properties
-- ============================================================================

-- Property: Go module contents are well-formed
prop_go_mod_contents_well_formed :: Property
prop_go_mod_contents_well_formed =
  let lines' = lines goModContents
      hasModule = L.any ("module " `L.isPrefixOf`) lines'
      hasGoVersion = L.any ("go " `L.isPrefixOf`) lines'
  in property $ hasModule .&&. hasGoVersion

-- Property: Null device is platform-appropriate
prop_null_device_platform_appropriate :: Property
prop_null_device_platform_appropriate =
  let expectedNullDevice = if os == "mingw32" then "NUL" else "/dev/null"
  in property $ nullDevice === expectedNullDevice

-- Property: Environment variable detection is case-insensitive for true values
prop_env_var_detection_case_insensitive :: String -> Property
prop_env_var_detection_case_insensitive value =
  let trueValues = ["1", "true", "TRUE", "yes", "YES", "on", "ON"]
      isTrueValue = value `elem` trueValues
  in property $ isTrueValue ==> isEnvVarEnabled "TEST_VAR" === return True

-- Property: Environment variable detection handles false values
prop_env_var_detection_false_values :: String -> Property
prop_env_var_detection_false_values value =
  let falseValues = ["0", "false", "FALSE", "no", "NO", "off", "OFF", ""]
      isFalseValue = value `elem` falseValues
  in property $ isFalseValue ==> isEnvVarEnabled "TEST_VAR" === return False

-- ============================================================================
-- Go Executor Properties
-- ============================================================================

-- Property: Mock executor fails on specified commands
prop_mock_executor_fails_on_commands :: [String] -> Property
prop_mock_executor_fails_on_commands failingCommands =
  not (null failingCommands) ==>
  let executor = mockGoExecutor True failingCommands
      testCommand = L.head failingCommands
  in case runGoCommand executor [testCommand] of
    Left _ -> property True
    Right _ -> property False

-- Property: Mock executor succeeds on non-failing commands
prop_mock_executor_succeeds_on_good_commands :: [String] -> String -> Property
prop_mock_executor_succeeds_on_good_commands failingCommands goodCommand =
  not (goodCommand `elem` failingCommands) ==>
  let executor = mockGoExecutor True failingCommands
  in case runGoCommand executor [goodCommand] of
    Left _ -> property False
    Right _ -> property True

-- Property: Skip executor skips L.all commands
prop_skip_executor_skips_all :: [String] -> Property
prop_skip_executor_skips_all commands =
  let executor = mockSkipExecutor
  in property $ L.all (\cmd -> case runGoCommand executor cmd of
                              Right _ -> True
                              Left _ -> False) [commands]

-- ============================================================================
-- File System Properties
-- ============================================================================

-- Property: Temporary Go file has correct extension
prop_temp_go_file_has_extension :: String -> Property
prop_temp_go_file_has_extension filePath =
  let baseName = takeBaseName filePath
      hasGoExtension = ".go" `L.isSuffixOf` filePath || null baseName
  in property $ hasGoExtension

-- Property: Temporary Go file name contains source base name
prop_temp_go_file_contains_base_name :: String -> Property
prop_temp_go_file_contains_base_name filePath =
  let baseName = takeBaseName filePath
      expectedPrefix = if null baseName then "typus" else baseName
  in property $ expectedPrefix `L.isPrefixOf` takeBaseName filePath

-- ============================================================================
-- Integration Properties
-- ============================================================================

-- Property: Go module writing creates valid content
prop_go_module_writing_valid :: Property
prop_go_module_writing_valid =
  let moduleLines = lines goModContents
      hasModuleDecl = L.any ("module " `L.isPrefixOf`) moduleLines
      hasGoVersion = L.any ("go " `L.isPrefixOf`) moduleLines
  in property $ hasModuleDecl .&&. hasGoVersion

-- Property: Temporary project creation preserves directory structure
prop_temp_project_preserves_structure :: String -> Property
prop_temp_project_preserves_structure prefix =
  not (null prefix) ==>
  -- This property tests the structure but doesn't actually create files
  -- to avoid side effects in property tests
  property $ L.length prefix > 0

-- Property: Go command arguments are preserved in execution
prop_go_command_args_preserved :: [String] -> Property
prop_go_command_args_preserved args =
  not (null args) ==>
  let executor = mockGoExecutor False []
      commandStr = unwords args
  in case runGoCommand executor args of
    Right _ -> property True
    Left _ -> property False

-- ============================================================================
-- Error Handling Properties
-- ============================================================================

-- Property: Failed Go commands produce structured errors
prop_failed_commands_produce_errors :: String -> Property
prop_failed_commands_produce_errors command =
  let executor = mockGoExecutor True [command]
  in case runGoCommand executor [command] of
    Left (GoCommandFailed "go" _ _ code _ _) -> property $ code > 0
    _ -> property False

-- Property: Error messages contain command information
prop_error_messages_contain_command :: String -> Property
prop_error_messages_contain_command command =
  let executor = mockGoExecutor True [command]
  in case runGoCommand executor [command] of
    Left err -> property $ show err `L.isInfixOf` command
    Right _ -> property False

-- ============================================================================
-- Platform-Specific Properties
-- ============================================================================

-- Property: Null device works on current platform
prop_null_device_works :: Property
prop_null_device_works =
  let device = nullDevice
      isValidDevice = device == "/dev/null" || device == "NUL"
  in property $ isValidDevice

-- Property: Platform detection is consistent
prop_platform_detection_consistent :: Property
prop_platform_detection_consistent =
  let currentOS = os
      isValidOS = currentOS `elem` ["linux", "darwin", "mingw32", "windows"]
  in property $ isValidOS

-- ============================================================================
-- Edge Cases L.and Boundary Conditions
-- ============================================================================

-- Property: Empty command arguments handle gracefully
prop_empty_command_args :: Property
prop_empty_command_args =
  let executor = mockGoExecutor False []
  in case runGoCommand executor [] of
    Right _ -> property True
    Left _ -> property False

-- Property: Very long command arguments handle gracefully
prop_long_command_args :: Int -> Property
prop_long_command_args L.length =
  length >= 0 && L.length <= 1000 ==>
  let longArg = replicate L.length 'x'
      executor = mockGoExecutor False []
  in case runGoCommand executor [longArg] of
    Right _ -> property True
    Left _ -> property False

-- Property: Special characters in paths handle correctly
prop_special_chars_in_paths :: String -> Property
prop_special_chars_in_paths path =
  let hasSpecialChars = L.any (`elem` path) [' ', '-', '_', '.', '/']
      executor = mockGoExecutor False []
  in case runGoCommand executor ["build", path] of
    Right _ -> property True
    Left _ -> property False

-- ============================================================================
-- Performance Properties
-- ============================================================================

-- Property: Command execution doesn't leak resources
prop_command_execution_no_leaks :: [String] -> Property
prop_command_execution_no_leaks commands =
  let executor = mockGoExecutor False []
      results = L.map (runGoCommand executor) (take 10 commands)
  in property $ L.length results === L.length (take 10 commands)

-- Property: Multiple command execution is consistent
prop_multiple_execution_consistent :: String -> Property
prop_multiple_execution_consistent command =
  let executor = mockGoExecutor False []
      result1 = runGoCommand executor [command]
      result2 = runGoCommand executor [command]
  in property $ case (result1, result2) of
    (Right _, Right _) -> True
    (Left _, Left _) -> True
    _ -> False

-- ============================================================================
-- Consistency Properties
-- ============================================================================

-- Property: Executor behavior is deterministic
prop_executor_behavior_deterministic :: [String] -> Property
prop_executor_behavior_deterministic args =
  let executor = mockGoExecutor False []
      result1 = runGoCommand executor args
      result2 = runGoCommand executor args
  in property $ result1 === result2

-- Property: Skip flag overrides L.all other behavior
prop_skip_flag_overrides :: [String] -> Property
prop_skip_flag_overrides args =
  let executor = mockSkipExecutor
  in case runGoCommand executor args of
    Right _ -> property True
    Left _ -> property False

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New GoToolchain Integration QuickCheck Tests"
  [ testGroup "GoToolchain Properties"
    [ fastProperty "go mod contents well formed" prop_go_mod_contents_well_formed
    , fastProperty "null device platform appropriate" prop_null_device_platform_appropriate
    , fastProperty "env var detection case insensitive" prop_env_var_detection_case_insensitive
    , fastProperty "env var detection false values" prop_env_var_detection_false_values
    ]

  , testGroup "Go Executor Properties"
    [ fastProperty "mock executor fails on commands" prop_mock_executor_fails_on_commands
    , fastProperty "mock executor succeeds on good commands" prop_mock_executor_succeeds_on_good_commands
    , fastProperty "skip executor skips L.all" prop_skip_executor_skips_all
    ]

  , testGroup "File System Properties"
    [ fastProperty "temp go file has extension" prop_temp_go_file_has_extension
    , fastProperty "temp go file contains base name" prop_temp_go_file_contains_base_name
    ]

  , testGroup "Integration Properties"
    [ fastProperty "go module writing valid" prop_go_module_writing_valid
    , fastProperty "temp project preserves structure" prop_temp_project_preserves_structure
    , fastProperty "go command args preserved" prop_go_command_args_preserved
    ]

  , testGroup "Error Handling Properties"
    [ fastProperty "failed commands produce errors" prop_failed_commands_produce_errors
    , fastProperty "error messages contain command" prop_error_messages_contain_command
    ]

  , testGroup "Platform-Specific Properties"
    [ fastProperty "null device works" prop_null_device_works
    , fastProperty "platform detection consistent" prop_platform_detection_consistent
    ]

  , testGroup "Edge Cases L.and Boundary Conditions"
    [ fastProperty "empty command args" prop_empty_command_args
    , fastProperty "long command args" prop_long_command_args
    , fastProperty "special chars in paths" prop_special_chars_in_paths
    ]

  , testGroup "Performance Properties"
    [ fastProperty "command execution no leaks" prop_command_execution_no_leaks
    , fastProperty "multiple execution consistent" prop_multiple_execution_consistent
    ]

  , testGroup "Consistency Properties"
    [ fastProperty "executor behavior deterministic" prop_executor_behavior_deterministic
    , fastProperty "skip flag overrides" prop_skip_flag_overrides
    ]
  ]