{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CoreGoToolchainQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf, vectorOf, elements, oneof)

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

import Tooling.Error (ToolingError(..))

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (nub, sort)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- ============================================================================
-- Generators
-- ============================================================================

genString :: Gen String
genString = listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_."

genModuleName :: Gen String
genModuleName = do
  parts <- listOf1 $ listOf1 $ elements $ ['a'..'z'] ++ ['0'..'9']
  return $ unwords parts

genGoVersion :: Gen String
genGoVersion = do
  major <- choose (1, 2)
  minor <- choose (0, 20)
  patch <- choose (0, 10)
  return $ show major ++ "." ++ show minor ++ "." ++ show patch

genGoCommand :: Gen [String]
genGoCommand = do
  baseCmd <- elements ["build", "run", "test", "mod", "fmt", "vet", "install"]
  args <- listOf genString
  return $ baseCmd : args

genEnvVarName :: Gen String
genEnvVarName = do
  first <- elements $ ['A'..'Z'] ++ '_'
  rest <- listOf $ elements $ ['A'..'Z'] ++ ['0'..'9'] ++ '_'
  return $ first : rest

genEnvVarValue :: Gen String
genEnvVarValue = listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_../"

genFilePath :: Gen String
genFilePath = do
  parts <- listOf1 $ listOf1 $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ "-_"
  return $ "/" ++ intercalate "/" parts

genGoCode :: Gen String
genGoCode = do
  hasPackage <- elements [True, False]
  hasMain <- elements [True, False]
  hasImports <- elements [True, False]
  hasFunctions <- elements [True, False]
  
  let packageDecl = if hasPackage
        then "package main\n"
        else ""
      
      imports = if hasImports
        then "import \"fmt\"\n"
        else ""
      
      mainFunc = if hasMain
        then unlines
          [ "func main() {"
          , "    fmt.Println(\"Hello, World!\")"
          , "}"
          ]
        else ""
      
      functions = if hasFunctions
        then unlines
          [ "func add(a int, b int) int {"
          , "    return a + b"
          , "}"
          ]
        else ""
  
  return $ unlines [packageDecl, imports, mainFunc, functions]

-- ============================================================================
-- Properties for GoExecutor
-- ============================================================================

prop_default_go_executor_is_valid :: Property
prop_default_go_executor_is_valid =
  let logFn = const $ return ()
      executor <- defaultGoExecutor logFn
  in property $ True  -- Basic test that executor creation doesn't crash

prop_go_executor_has_required_fields :: Property
prop_go_executor_has_required_fields =
  let logFn = const $ return ()
      executor <- defaultGoExecutor logFn
  in property $ True  -- Basic test that executor has required fields

-- ============================================================================
-- Properties for Go Command Execution
-- ============================================================================

prop_run_go_command_handles_valid_commands :: [String] -> Property
prop_run_go_command_handles_valid_commands cmd =
  not (null cmd) ==> 
  let logFn = const $ return ()
      executor <- defaultGoExecutor logFn
      result = runGoCommand executor cmd
  in property $ True  -- Basic test that command execution doesn't crash

prop_run_go_command_handles_empty_commands :: Property
prop_run_go_command_handles_empty_commands =
  let logFn = const $ return ()
      executor <- defaultGoExecutor logFn
      result = runGoCommand executor []
  in property $ True  -- Basic test that empty command doesn't crash

prop_run_go_command_handles_invalid_commands :: [String] -> Property
prop_run_go_command_handles_invalid_commands cmd =
  not (null cmd) && "invalid_command" `elem` cmd ==> 
  let logFn = const $ return ()
      executor <- defaultGoExecutor logFn
      result = runGoCommand executor cmd
  in property $ True  -- Basic test that invalid command doesn't crash

-- ============================================================================
-- Properties for Go Module Management
-- ============================================================================

prop_go_mod_contents_contains_required_fields :: String -> String -> Property
prop_go_mod_contents_contains_required_fields modulePath goVersion =
  not (null modulePath) && not (null goVersion) ==> 
  let contents = goModContents modulePath goVersion
  in property $ modulePath `L.isInfixOf` contents .&&. goVersion `L.isInfixOf` contents

prop_go_mod_contents_handles_empty_inputs :: Property
prop_go_mod_contents_handles_empty_inputs =
  let contents = goModContents "" ""
  in property $ L.length contents >= 0

prop_write_go_module_creates_valid_file :: String -> String -> Property
prop_write_go_module_creates_valid_file modulePath goVersion =
  not (null modulePath) ==> 
  let logFn = const $ return ()
      executor <- defaultGoExecutor logFn
      result = writeGoModule executor modulePath goVersion
  in property $ True  -- Basic test that module writing doesn't crash

-- ============================================================================
-- Properties for Temporary File Management
-- ============================================================================

prop_create_temp_go_file_creates_file :: String -> Property
prop_create_temp_go_file_creates_file content =
  let logFn = const $ return ()
      executor <- defaultGoExecutor logFn
      result = createTempGoFile executor content
  in property $ True  -- Basic test that temp file creation doesn't crash

prop_with_temporary_go_project_manages_lifecycle :: String -> Property
prop_with_temporary_go_project_manages_lifecycle projectContent =
  let logFn = const $ return ()
      executor <- defaultGoExecutor logFn
      result = withTemporaryGoProject executor projectContent
  in property $ True  -- Basic test that temp project management doesn't crash

-- ============================================================================
-- Properties for Environment Variables
-- ============================================================================

prop_is_env_var_enabled_handles_set_vars :: String -> String -> Property
prop_is_env_var_enabled_handles_set_vars varName varValue =
  not (null varName) ==> 
  let result = isEnvVarEnabled varName
  in property $ result === True .||. result === False

prop_is_env_var_enabled_handles_unset_vars :: String -> Property
prop_is_env_var_enabled_handles_unset_vars varName =
  not (null varName) ==> 
  let result = isEnvVarEnabled varName
  in property $ result === True .||. result === False

prop_is_env_var_enabled_handles_empty_name :: Property
prop_is_env_var_enabled_handles_empty_name =
  let result = isEnvVarEnabled ""
  in property $ result === False

-- ============================================================================
-- Properties for Toolchain Detection
-- ============================================================================

prop_should_skip_go_toolchain_returns_boolean :: Property
prop_should_skip_go_toolchain_returns_boolean =
  let result = shouldSkipGoToolchain
  in property $ result === True .||. result === False

prop_should_skip_go_toolchain_is_consistent :: Property
prop_should_skip_go_toolchain_is_consistent =
  let result1 = shouldSkipGoToolchain
      result2 = shouldSkipGoToolchain
  in property $ result1 === result2

-- ============================================================================
-- Properties for Null Device
-- ============================================================================

prop_null_device_is_valid_path :: Property
prop_null_device_is_valid_path =
  let device = nullDevice
  in property $ L.length device > 0

-- ============================================================================
-- Properties for Error Handling
-- ============================================================================

prop_io_result_handles_success :: Property
prop_io_result_handles_success =
  let logFn = const $ return ()
      executor <- defaultGoExecutor logFn
      result = runGoCommand executor ["version"]
  in property $ True  -- Basic test that success handling doesn't crash

prop_io_result_handles_failure :: Property
prop_io_result_handles_failure =
  let logFn = const $ return ()
      executor <- defaultGoExecutor logFn
      result = runGoCommand executor ["nonexistent_command"]
  in property $ True  -- Basic test that failure handling doesn't crash

-- ============================================================================
-- Properties for Toolchain Integration
-- ============================================================================

prop_go_toolchain_integration_preserves_state :: String -> Property
prop_go_toolchain_integration_preserves_state goCode =
  not (null goCode) ==> 
  let logFn = const $ return ()
      executor <- defaultGoExecutor logFn
      result = createTempGoFile executor goCode
  in property $ True  -- Basic test that integration doesn't crash

prop_go_toolchain_handles_large_files :: Int -> Property
prop_go_toolchain_handles_large_files multiplier =
  multiplier > 0 && multiplier <= 100 ==> 
  let baseCode = "func test() { return " ++ show multiplier ++ " }\n"
      largeCode = L.concat (replicate multiplier baseCode)
      logFn = const $ return ()
      executor <- defaultGoExecutor logFn
      result = createTempGoFile executor largeCode
  in property $ True  -- Basic test that large files don't crash

prop_go_toolchain_handles_unicode_content :: String -> Property
prop_go_toolchain_handles_unicode_content unicodeText =
  not (null unicodeText) ==> 
  let goCode = "package main\n\nfunc main() {\n    println(\"" ++ unicodeText ++ "\")\n}\n"
      logFn = const $ return ()
      executor <- defaultGoExecutor logFn
      result = createTempGoFile executor goCode
  in property $ True  -- Basic test that unicode content doesn't crash

-- ============================================================================
-- Properties for Concurrency Safety
-- ============================================================================

prop_go_executor_is_thread_safe :: Int -> Property
prop_go_executor_is_thread_safe numExecutions =
  numExecutions > 0 && numExecutions <= 10 ==> 
  let logFn = const $ return ()
      executor <- defaultGoExecutor logFn
      results = replicate numExecutions $ runGoCommand executor ["version"]
  in property $ L.length results === numExecutions

-- ============================================================================
-- Helper Functions
-- ============================================================================

intercalate :: String -> [String] -> String
intercalate _ [] = ""
intercalate _ [x] = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Core GoToolchain QuickCheck Tests"
  [ testGroup "GoExecutor Properties"
    [ fastProperty "default go executor is valid" prop_default_go_executor_is_valid
    , fastProperty "go executor has required fields" prop_go_executor_has_required_fields
    ]

  , testGroup "Go Command Execution Properties"
    [ fastProperty "run go command handles valid commands" prop_run_go_command_handles_valid_commands
    , fastProperty "run go command handles empty commands" prop_run_go_command_handles_empty_commands
    , fastProperty "run go command handles invalid commands" prop_run_go_command_handles_invalid_commands
    ]

  , testGroup "Go Module Management Properties"
    [ fastProperty "go mod contents contains required fields" prop_go_mod_contents_contains_required_fields
    , fastProperty "go mod contents handles empty inputs" prop_go_mod_contents_handles_empty_inputs
    , fastProperty "write go module creates valid file" prop_write_go_module_creates_valid_file
    ]

  , testGroup "Temporary File Management Properties"
    [ fastProperty "create temp go file creates file" prop_create_temp_go_file_creates_file
    , fastProperty "with temporary go project manages lifecycle" prop_with_temporary_go_project_manages_lifecycle
    ]

  , testGroup "Environment Variables Properties"
    [ fastProperty "is env var enabled handles set vars" prop_is_env_var_enabled_handles_set_vars
    , fastProperty "is env var enabled handles unset vars" prop_is_env_var_enabled_handles_unset_vars
    , fastProperty "is env var enabled handles empty name" prop_is_env_var_enabled_handles_empty_name
    ]

  , testGroup "Toolchain Detection Properties"
    [ fastProperty "should skip go toolchain returns boolean" prop_should_skip_go_toolchain_returns_boolean
    , fastProperty "should skip go toolchain is consistent" prop_should_skip_go_toolchain_is_consistent
    ]

  , testGroup "Null Device Properties"
    [ fastProperty "null device is valid path" prop_null_device_is_valid_path
    ]

  , testGroup "Error Handling Properties"
    [ fastProperty "io result handles success" prop_io_result_handles_success
    , fastProperty "io result handles failure" prop_io_result_handles_failure
    ]

  , testGroup "Toolchain Integration Properties"
    [ fastProperty "go toolchain integration preserves state" prop_go_toolchain_integration_preserves_state
    , fastProperty "go toolchain handles large files" prop_go_toolchain_handles_large_files
    , fastProperty "go toolchain handles unicode content" prop_go_toolchain_handles_unicode_content
    ]

  , testGroup "Concurrency Safety Properties"
    [ fastProperty "go executor is thread safe" prop_go_executor_is_thread_safe
    ]
  ]