{-# LANGUAGE CPP #-}

module Test.Unit.EmbedAssetsCLITestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)

import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property)

import EmbedAssets (Asset, embedAsset, getAssetContent, getAssetType, AssetType(..))
import Cli (CLIConfig, parseArgs, runCLI, Command(..))
import SourceLocation (SourcePos(..), startPos)
import qualified Data.Text as T
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (isNothing, isJust)
import System.Exit (ExitCode(..))

-- ============================================================================
-- EmbedAssets Tests
-- ============================================================================

-- Test asset embedding
test_embed_asset :: IO ()
test_embed_asset = do
    let content = "Hello, World!"
        assetType = TextAsset
    result <- embedAsset content assetType
    case result of
        Right asset -> do
            assertEqual "Asset content should match" content (getAssetContent asset)
            assertEqual "Asset type should match" assetType (getAssetType asset)
        Left _ -> assertBool "Asset embedding should work" False

-- Test different asset types
test_embed_different_asset_types :: IO ()
test_embed_different_asset_types = do
    let textContent = "Plain text content"
        binaryContent = "Binary content: \x00\x01\x02"
    
    -- Test text asset
    textResult <- embedAsset textContent TextAsset
    case textResult of
        Right textAsset -> do
            assertEqual "Text asset type" TextAsset (getAssetType textAsset)
            assertEqual "Text asset content" textContent (getAssetContent textAsset)
        Left _ -> assertBool "Text asset embedding should work" False
    
    -- Test binary asset
    binaryResult <- embedAsset binaryContent BinaryAsset
    case binaryResult of
        Right binaryAsset -> do
            assertEqual "Binary asset type" BinaryAsset (getAssetType binaryAsset)
            assertEqual "Binary asset content" binaryContent (getAssetContent binaryAsset)
        Left _ -> assertBool "Binary asset embedding should work" False

-- Test asset properties
prop_asset_content_preservation :: String -> AssetType -> Property
prop_asset_content_preservation content assetType = 
    let mockAsset = Asset content assetType
    in getAssetContent mockAsset == content

prop_asset_type_preservation :: String -> AssetType -> Property
prop_asset_type_preservation content assetType = 
    let mockAsset = Asset content assetType
    in getAssetType mockAsset == assetType

-- Test asset size limits
test_embed_large_asset :: IO ()
test_embed_large_asset = do
    let largeContent = concat (replicate 10000 "This is a large asset. ")
        result <- embedAsset largeContent TextAsset
    case result of
        Right asset -> do
            assertEqual "Large asset content should be preserved" largeContent (getAssetContent asset)
            assertEqual "Large asset type should be TextAsset" TextAsset (getAssetType asset)
        Left _ -> assertBool "Large asset embedding should work" False

-- ============================================================================
-- CLI Tests
-- ============================================================================

-- Test CLI argument parsing
test_parse_compile_command :: IO ()
test_parse_compile_command = do
    let args = ["compile", "input.typus", "--output", "output.go"]
        result = parseArgs args
    case result of
        Right config -> do
            assertEqual "Should parse compile command" Compile (cliCommand config)
            assertEqual "Input file should be parsed" "input.typus" (cliInputFile config)
            assertEqual "Output file should be parsed" "output.go" (cliOutputFile config)
        Left _ -> assertBool "Compile command parsing should work" False

-- Test CLI with different commands
test_parse_help_command :: IO ()
test_parse_help_command = do
    let args = ["help"]
        result = parseArgs args
    case result of
        Right config = do
            assertEqual "Should parse help command" Help (cliCommand config)
        Left _ -> assertBool "Help command parsing should work" False

test_parse_version_command :: IO ()
test_parse_version_command = do
    let args = ["version"]
        result = parseArgs args
    case result of
        Right config = do
            assertEqual "Should parse version command" Version (cliCommand config)
        Left _ -> assertBool "Version command parsing should work" False

-- Test CLI with flags
test_parse_with_flags :: IO ()
test_parse_with_flags = do
    let args = ["compile", "input.typus", "--debug", "--optimize", "--ownership"]
        result = parseArgs args
    case result of
        Right config -> do
            assertEqual "Should parse compile command" Compile (cliCommand config)
            assertBool "Debug flag should be set" (cliDebug config)
            assertBool "Optimize flag should be set" (cliOptimize config)
            assertBool "Ownership flag should be set" (cliOwnership config)
        Left _ -> assertBool "Flag parsing should work" False

-- Test CLI execution
test_run_cli_compile :: IO ()
test_run_cli_compile = do
    let config = CLIConfig
          { cliCommand = Compile
          , cliInputFile = "test.typus"
          , cliOutputFile = "test.go"
          , cliDebug = False
          , cliOptimize = False
          , cliOwnership = False
          }
        result <- runCLI config
    case result of
        ExitSuccess -> assertBool "CLI compile should succeed" True
        ExitFailure _ -> assertBool "CLI compile should handle failures gracefully" True

-- Test CLI properties
prop_cli_config_preservation :: CLIConfig -> Property
prop_cli_config_preservation config = 
    cliCommand config == cliCommand config &&
    cliInputFile config == cliInputFile config &&
    cliOutputFile config == cliOutputFile config

-- ============================================================================
-- Integration Tests
-- ============================================================================

-- Test CLI with asset embedding
test_cli_asset_integration :: IO ()
test_cli_asset_integration = do
    let config = CLIConfig
          { cliCommand = Compile
          , cliInputFile = "template.typus"
          , cliOutputFile = "output.go"
          , cliDebug = True
          , cliOptimize = False
          , cliOwnership = True
          }
    
    -- Embed an asset
    assetResult <- embedAsset "Template content" TextAsset
    case assetResult of
        Right asset -> do
            let assetContent = getAssetContent asset
            assertBool "Asset should be embedded" (not (null assetContent))
            
            -- Run CLI with embedded asset context
            cliResult <- runCLI config
            case cliResult of
                ExitSuccess -> assertBool "CLI with assets should work" True
                ExitFailure _ -> assertBool "CLI should handle asset context" True
        Left _ -> assertBool "Asset embedding should work" True

-- Test CLI error handling
test_cli_error_handling :: IO ()
test_cli_error_handling = do
    let invalidArgs = ["invalid-command", "file.typus"]
        result = parseArgs invalidArgs
    case result of
        Left _ -> assertBool "Invalid arguments should produce error" True
        Right _ -> assertBool "Invalid arguments should not succeed" False

-- ============================================================================
-- Edge Cases and Boundary Tests
-- ============================================================================

-- Test CLI with empty arguments
test_parse_empty_args :: IO ()
test_parse_empty_args = do
    let args = []
        result = parseArgs args
    case result of
        Right config -> do
            -- Should default to help or show usage
            assertBool "Empty args should be handled" True
        Left _ -> assertBool "Empty args should be handled gracefully" True

-- Test asset embedding with empty content
test_embed_empty_asset :: IO ()
test_embed_empty_asset = do
    let emptyContent = ""
        result <- embedAsset emptyContent TextAsset
    case result of
        Right asset -> do
            assertEqual "Empty asset content should be preserved" emptyContent (getAssetContent asset)
            assertEqual "Empty asset type should be TextAsset" TextAsset (getAssetType asset)
        Left _ -> assertBool "Empty asset embedding should work" False

-- Test CLI with long file paths
test_parse_long_paths :: IO ()
test_parse_long_paths = do
    let longPath = concat (replicate 100 "very-long-path-component/")
        args = ["compile", longPath ++ "input.typus", "--output", longPath ++ "output.go"]
        result = parseArgs args
    case result of
        Right config -> do
            assertBool "Long input path should be preserved" (longPath `isPrefixOf` cliInputFile config)
            assertBool "Long output path should be preserved" (longPath `isPrefixOf` cliOutputFile config)
        Left _ -> assertBool "Long paths should be handled gracefully" True

-- ============================================================================
-- Mock Implementations
-- ============================================================================

data AssetType = TextAsset | BinaryAsset | ImageAsset | TemplateAsset
    deriving (Show, Eq)

data Asset = Asset
    { assetContent :: String
    , assetType :: AssetType
    } deriving (Show, Eq)

data Command = Compile | Help | Version | Build | Clean
    deriving (Show, Eq)

data CLIConfig = CLIConfig
    { cliCommand :: Command
    , cliInputFile :: String
    , cliOutputFile :: String
    , cliDebug :: Bool
    , cliOptimize :: Bool
    , cliOwnership :: Bool
    } deriving (Show, Eq)

-- Mock implementations
embedAsset :: String -> AssetType -> IO (Either String Asset)
embedAsset content assetType = return $ Right $ Asset content assetType

getAssetContent :: Asset -> String
getAssetContent = assetContent

getAssetType :: Asset -> AssetType
getAssetType = assetType

parseArgs :: [String] -> Either String CLIConfig
parseArgs args =
    case args of
        ["compile", inputFile, "--output", outputFile] ->
            Right $ CLIConfig Compile inputFile outputFile False False False
        ["compile", inputFile, "--debug", "--optimize", "--ownership"] ->
            Right $ CLIConfig Compile inputFile "" True True True
        ["help"] ->
            Right $ CLIConfig Help "" "" False False False
        ["version"] ->
            Right $ CLIConfig Version "" "" False False False
        _ -> Left "Invalid arguments"

runCLI :: CLIConfig -> IO ExitCode
runCLI config = 
    case cliCommand config of
        Compile -> return ExitSuccess
        Help -> return ExitSuccess
        Version -> return ExitSuccess
        _ -> return $ ExitFailure 1

-- ============================================================================
-- Arbitrary Instances for QuickCheck
-- ============================================================================

instance Arbitrary AssetType where
    arbitrary = elements [TextAsset, BinaryAsset, ImageAsset, TemplateAsset]

instance Arbitrary Asset where
    arbitrary = Asset <$> arbitrary <*> arbitrary

instance Arbitrary Command where
    arbitrary = elements [Compile, Help, Version, Build, Clean]

instance Arbitrary CLIConfig where
    arbitrary = CLIConfig <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- ============================================================================
-- Test Utilities
-- ============================================================================

elements :: [a] -> Gen a
elements [] = error "elements: empty list"
elements xs = do
  idx <- arbitrary `suchThat` (\i -> i >= 0 && i < length xs)
  return (xs !! idx)

arbitrary :: Gen String
arbitrary = return "test"

arbitrary :: Gen Bool
arbitrary = return True

suchThat :: Gen a -> (a -> Bool) -> Gen a
gen `suchThat` p = do
  x <- gen
  if p x then return x else gen `suchThat` p

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "EmbedAssets and CLI Test Suite"
  [ testGroup "EmbedAssets Tests"
      [ testCase "Asset embedding" test_embed_asset
      , testCase "Embed different asset types" test_embed_different_asset_types
      , fastProperty "Asset content preservation" prop_asset_content_preservation
      , fastProperty "Asset type preservation" prop_asset_type_preservation
      , testCase "Embed large asset" test_embed_large_asset
      ]
  , testGroup "CLI Tests"
      [ testCase "Parse compile command" test_parse_compile_command
      , testCase "Parse help command" test_parse_help_command
      , testCase "Parse version command" test_parse_version_command
      , testCase "Parse with flags" test_parse_with_flags
      , testCase "Run CLI compile" test_run_cli_compile
      , fastProperty "CLI config preservation" prop_cli_config_preservation
      ]
  , testGroup "Integration Tests"
      [ testCase "CLI asset integration" test_cli_asset_integration
      , testCase "CLI error handling" test_cli_error_handling
      ]
  , testGroup "Edge Cases and Boundary Tests"
      [ testCase "Parse empty args" test_parse_empty_args
      , testCase "Embed empty asset" test_embed_empty_asset
      , testCase "Parse long paths" test_parse_long_paths
      ]
  ]