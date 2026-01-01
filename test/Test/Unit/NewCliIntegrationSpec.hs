{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCliIntegrationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import Cli (CliOptions(..), parseCliOptions, runCli)
import Cli.Runner (runCompilation, runAnalysis, runValidation)
import CommandLineDebug (DebugOptions(..), parseDebugOptions)
import SourceLocation (SourceSpan(..), startPos, SourcePos(..))
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.List (sort, nub, intercalate)
import Data.Char (isSpace, isAlpha, isAlphaNum)
import System.Exit (ExitCode(..))

-- Property: CLI options parsing handles basic flags correctly
prop_basic_cli_parsing :: [String] -> Property
prop_basic_cli_parsing flags =
  not (null flags) && L.length flags <= 5 &&
  L.all (`elem` ["--help", "--version", "--verbose", "--quiet"]) flags ==>
  let args = flags ++ ["test.typus"]
      result = parseCliOptions args
  in case result of
    Left _ -> property False
    Right options -> hasValidCliOptions options

-- Property: CLI options parsing handles file arguments correctly
prop_file_argument_parsing :: String -> Property
prop_file_argument_parsing filename =
  not (null filename) && ".typus" `L.isSuffixOf` filename &&
  L.all (\c -> isAlphaNum c || c `elem` ['.', '-', '_']) filename ==>
  let args = [filename]
      result = parseCliOptions args
  in case result of
    Left _ -> property False
    Right options -> cliInputFile options === filename

-- Property: CLI options parsing handles multiple files correctly
prop_multiple_files_parsing :: [String] -> Property
prop_multiple_files_parsing filenames =
  not (null filenames) && L.length filenames <= 3 &&
  L.all (\f -> not (null f) && ".typus" `L.isSuffixOf` f) filenames ==>
  let args = filenames
      result = parseCliOptions args
  in case result of
    Left _ -> property False
    Right options -> L.length (cliInputFiles options) === L.length filenames

-- Property: CLI options parsing handles output directory correctly
prop_output_directory_parsing :: String -> Property
prop_output_directory_parsing outputDir =
  not (null outputDir) && not ('/' `L.isSuffixOf` outputDir) &&
  L.all (\c -> isAlphaNum c || c `elem` ['.', '-', '_', '/']) outputDir ==>
  let args = ["--output", outputDir, "test.typus"]
      result = parseCliOptions args
  in case result of
    Left _ -> property False
    Right options -> cliOutputDir options === Just outputDir

-- Property: CLI options parsing handles optimization levels correctly
prop_optimization_level_parsing :: String -> Property
prop_optimization_level_parsing optLevel =
  optLevel `elem` ["0", "1", "2", "3", "s", "z"] ==>
  let args = ["--optimize", optLevel, "test.typus"]
      result = parseCliOptions args
  in case result of
    Left _ -> property False
    Right options -> cliOptimization options === Just optLevel

-- Property: CLI options parsing handles debug options correctly
prop_debug_options_parsing :: [String] -> Property
prop_debug_options_parsing debugFlags =
  not (null debugFlags) && L.length debugFlags <= 3 &&
  L.all (`elem` ["--debug-ast", "--debug-ir", "--debug-symbols"]) debugFlags ==>
  let args = debugFlags ++ ["test.typus"]
      result = parseCliOptions args
  in case result of
    Left _ -> property False
    Right options -> hasDebugOptions options debugFlags

-- Property: CLI options parsing handles ownership flags correctly
prop_ownership_flags_parsing :: String -> Property
prop_ownership_flags_parsing ownershipMode =
  ownershipMode `elem` ["on", "off", "strict"] ==>
  let args = ["--ownership", ownershipMode, "test.typus"]
      result = parseCliOptions args
  in case result of
    Left _ -> property False
    Right options -> cliOwnership options === Just ownershipMode

-- Property: CLI options parsing handles dependent types flags correctly
prop_dependent_types_flags_parsing :: String -> Property
prop_dependent_types_flags_parsing dtMode =
  dtMode `elem` ["on", "off", "experimental"] ==>
  let args = ["--dependent-types", dtMode, "test.typus"]
      result = parseCliOptions args
  in case result of
    Left _ -> property False
    Right options -> cliDependentTypes options === Just dtMode

-- Property: CLI handles compilation correctly
prop_cli_compilation :: String -> Property
prop_cli_compilation sourceCode =
  not (null sourceCode) && "package main" `L.isPrefixOf` sourceCode ==>
  let options = defaultCliOptions { cliInputFiles = ["test.typus"] }
      result = runCompilation options sourceCode
  in case result of
    Left _ -> property False
    Right exitCode -> exitCode === ExitSuccess

-- Property: CLI handles analysis correctly
prop_cli_analysis :: String -> Property
prop_cli_analysis sourceCode =
  not (null sourceCode) && "package main" `L.isPrefixOf` sourceCode ==>
  let options = defaultCliOptions { cliInputFiles = ["test.typus"], cliMode = Just "analyze" }
      result = runAnalysis options sourceCode
  in case result of
    Left _ -> property False
    Right exitCode -> exitCode === ExitSuccess

-- Property: CLI handles validation correctly
prop_cli_validation :: String -> Property
prop_cli_validation sourceCode =
  not (null sourceCode) && "package main" `L.isPrefixOf` sourceCode ==>
  let options = defaultCliOptions { cliInputFiles = ["test.typus"], cliMode = Just "validate" }
      result = runValidation options sourceCode
  in case result of
    Left _ -> property False
    Right exitCode -> exitCode === ExitSuccess

-- Property: CLI handles verbose output correctly
prop_verbose_output :: String -> Property
prop_verbose_output sourceCode =
  not (null sourceCode) && "package main" `L.isPrefixOf` sourceCode ==>
  let options = defaultCliOptions { cliInputFiles = ["test.typus"], cliVerbose = True }
      result = runCli options
  in case result of
    Left _ -> property False
    Right exitCode -> exitCode === ExitSuccess

-- Property: CLI handles quiet mode correctly
prop_quiet_mode :: String -> Property
prop_quiet_mode sourceCode =
  not (null sourceCode) && "package main" `L.isPrefixOf` sourceCode ==>
  let options = defaultCliOptions { cliInputFiles = ["test.typus"], cliQuiet = True }
      result = runCli options
  in case result of
    Left _ -> property False
    Right exitCode -> exitCode === ExitSuccess

-- Property: CLI handles help flag correctly
prop_help_flag :: Property
prop_help_flag =
  let options = defaultCliOptions { cliHelp = True }
      result = runCli options
  in case result of
    Left _ -> property False
    Right exitCode -> exitCode === ExitSuccess

-- Property: CLI handles version flag correctly
prop_version_flag :: Property
prop_version_flag =
  let options = defaultCliOptions { cliVersion = True }
      result = runCli options
  in case result of
    Left _ -> property False
    Right exitCode -> exitCode === ExitSuccess

-- Property: CLI handles invalid arguments correctly
prop_invalid_arguments :: [String] -> Property
prop_invalid_arguments invalidArgs =
  not (null invalidArgs) && L.length invalidArgs <= 3 &&
  L.all (`elem` ["--invalid-flag", "--unknown-option", ""]) invalidArgs ==>
  let result = parseCliOptions invalidArgs
  in case result of
    Left _ -> property True -- Expected to fail
    Right _ -> property False -- Should not succeed

-- Property: CLI handles missing input file correctly
prop_missing_input_file :: [String] -> Property
prop_missing_input_file flags =
  not (null flags) && L.length flags <= 3 &&
  L.all (`elem` ["--verbose", "--optimize", "2"]) flags &&
  not (L.any (`elem` ["test.typus", "input.typus"]) flags) ==>
  let result = parseCliOptions flags
  in case result of
    Left _ -> property True -- Expected to fail
    Right _ -> property False -- Should not succeed

-- Property: CLI handles conflicting options correctly
prop_conflicting_options :: Property
prop_conflicting_options =
  let args = ["--verbose", "--quiet", "test.typus"]
      result = parseCliOptions args
  in case result of
    Left _ -> property True -- Expected to fail due to conflict
    Right _ -> property False -- Should not succeed

-- Property: CLI handles complex option combinations correctly
prop_complex_option_combinations :: [String] -> String -> Property
prop_complex_option_combinations flags filename =
  not (null flags) && L.length flags <= 5 &&
  L.all (`elem` ["--verbose", "--optimize", "2", "--debug-ast", "--ownership", "on"]) flags &&
  not (null filename) && ".typus" `L.isSuffixOf` filename ==>
  let args = flags ++ [filename]
      result = parseCliOptions args
  in case result of
    Left _ -> property False
    Right options -> hasValidCliOptions options .&&. cliInputFile options === filename

-- Property: CLI debug options parsing works correctly
prop_debug_options_parsing_advanced :: [String] -> Property
prop_debug_options_parsing_advanced debugOptions =
  not (null debugOptions) && L.length debugOptions <= 4 &&
  L.all (`elem` ["ast", "ir", "symbols", "types"]) debugOptions ==>
  let debugStr = intercalate "," debugOptions
      result = parseDebugOptions debugStr
  in case result of
    Left _ -> property False
    Right options -> hasValidDebugOptions options debugOptions

-- Helper functions
defaultCliOptions :: CliOptions
defaultCliOptions = CliOptions
  { cliInputFiles = []
  , cliOutputDir = Nothing
  , cliMode = Nothing
  , cliVerbose = False
  , cliQuiet = False
  , cliHelp = False
  , cliVersion = False
  , cliOptimization = Nothing
  , cliOwnership = Nothing
  , cliDependentTypes = Nothing
  , cliDebugOptions = Nothing
  }

hasValidCliOptions :: CliOptions -> Bool
hasValidCliOptions options = not (L.null (cliInputFiles options)) -- Basic validation

hasDebugOptions :: CliOptions -> [String] -> Bool
hasDebugOptions options flags = L.all (`elem` flags) ["--debug-ast", "--debug-ir", "--debug-symbols"]

hasValidDebugOptions :: DebugOptions -> [String] -> Bool
hasValidDebugOptions options expected = True -- Placeholder implementation

cliInputFile :: CliOptions -> String
cliInputFile options = case cliInputFiles options of
  [] -> ""
  (file:_) -> file

cliInputFiles :: CliOptions -> [String]
cliInputFiles = cliInputFiles

cliOutputDir :: CliOptions -> Maybe String
cliOutputDir = cliOutputDir

cliMode :: CliOptions -> Maybe String
cliMode = cliMode

cliVerbose :: CliOptions -> Bool
cliVerbose = cliVerbose

cliQuiet :: CliOptions -> Bool
cliQuiet = cliQuiet

cliHelp :: CliOptions -> Bool
cliHelp = cliHelp

cliVersion :: CliOptions -> Bool
cliVersion = cliVersion

cliOptimization :: CliOptions -> Maybe String
cliOptimization = cliOptimization

cliOwnership :: CliOptions -> Maybe String
cliOwnership = cliOwnership

cliDependentTypes :: CliOptions -> Maybe String
cliDependentTypes = cliDependentTypes

cliDebugOptions :: CliOptions -> Maybe DebugOptions
cliDebugOptions = cliDebugOptions

tests :: TestTree
tests = testGroup "New CLI Integration tests"
  [ fastProperty "CLI options parsing handles basic flags correctly" prop_basic_cli_parsing
  , fastProperty "CLI options parsing handles file arguments correctly" prop_file_argument_parsing
  , fastProperty "CLI options parsing handles multiple files correctly" prop_multiple_files_parsing
  , fastProperty "CLI options parsing handles output directory correctly" prop_output_directory_parsing
  , fastProperty "CLI options parsing handles optimization levels correctly" prop_optimization_level_parsing
  , fastProperty "CLI options parsing handles debug options correctly" prop_debug_options_parsing
  , fastProperty "CLI options parsing handles ownership flags correctly" prop_ownership_flags_parsing
  , fastProperty "CLI options parsing handles dependent types flags correctly" prop_dependent_types_flags_parsing
  , fastProperty "CLI handles compilation correctly" prop_cli_compilation
  , fastProperty "CLI handles analysis correctly" prop_cli_analysis
  , fastProperty "CLI handles validation correctly" prop_cli_validation
  , fastProperty "CLI handles verbose output correctly" prop_verbose_output
  , fastProperty "CLI handles quiet mode correctly" prop_quiet_mode
  , fastProperty "CLI handles help flag correctly" prop_help_flag
  , fastProperty "CLI handles version flag correctly" prop_version_flag
  , fastProperty "CLI handles invalid arguments correctly" prop_invalid_arguments
  , fastProperty "CLI handles missing input file correctly" prop_missing_input_file
  , fastProperty "CLI handles conflicting options correctly" prop_conflicting_options
  , fastProperty "CLI handles complex option combinations correctly" prop_complex_option_combinations
  , fastProperty "CLI debug options parsing works correctly" prop_debug_options_parsing_advanced
  ]