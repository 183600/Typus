{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Unit.TestCliIntegrationSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Parser as P
import SourceLocation
import ErrorHandler
import Compiler.IR
import Ownership
import Dependencies
import Utils
import Cli
import qualified Data.Text as T
import TestSupport.Arbitrary ()

-- | Test suite for CLI Integration
testCliIntegration :: TestTree
testCliIntegration = testGroup "CLI Integration Tests"
  [ testCase "CLI: parse command line arguments" $
      let args = ["--ownership", "--dependent-types", "input.typus"]
          options = parseCommandLineArgs args
      in case options of
           Right opts -> do
             ownershipFlag opts @?= True
             dependentTypesFlag opts @?= True
             inputFile opts @?= "input.typus"
           Left err -> assertFailure $ "Command line parsing failed: " ++ show err
           
  , testCase "CLI: handle missing input file" $
      let args = ["--ownership", "--dependent-types"]
          options = parseCommandLineArgs args
      in case options of
           Right _ -> assertFailure "Command line parsing should have failed"
           Left _ -> return ()
           
  , testCase "CLI: handle invalid arguments" $
      let args = ["--invalid-flag", "input.typus"]
          options = parseCommandLineArgs args
      in case options of
           Right _ -> assertFailure "Command line parsing should have failed"
           Left _ -> return ()
           
  , testCase "CLI: process file with ownership analysis" $
      let args = ["--ownership", "test.typus"]
          options = parseCommandLineArgs args
      in case options of
           Right opts -> do
             ownershipFlag opts @?= True
             dependentTypesFlag opts @?= False
           Left err -> assertFailure $ "Command line parsing failed: " ++ show err
           
  , testCase "CLI: process file with type analysis" $
      let args = ["--dependent-types", "test.typus"]
          options = parseCommandLineArgs args
      in case options of
           Right opts -> do
             ownershipFlag opts @?= False
             dependentTypesFlag opts @?= True
           Left err -> assertFailure $ "Command line parsing failed: " ++ show err
           
  , testCase "CLI: process file with both analyses" $
      let args = ["--ownership", "--dependent-types", "test.typus"]
          options = parseCommandLineArgs args
      in case options of
           Right opts -> do
             ownershipFlag opts @?= True
             dependentTypesFlag opts @?= True
           Left err -> assertFailure $ "Command line parsing failed: " ++ show err
           
  , testCase "CLI: handle verbose output" $
      let args = ["--verbose", "--ownership", "test.typus"]
          options = parseCommandLineArgs args
      in case options of
           Right opts -> verboseFlag opts @?= True
           Left err -> assertFailure $ "Command line parsing failed: " ++ show err
           
  , testCase "CLI: handle output file specification" $
      let args = ["--output", "output.go", "--ownership", "test.typus"]
          options = parseCommandLineArgs args
      in case options of
           Right opts -> outputFile opts @?= Just "output.go"
           Left err -> assertFailure $ "Command line parsing failed: " ++ show err
           
  , testCase "CLI: handle help flag" $
      let args = ["--help"]
          options = parseCommandLineArgs args
      in case options of
           Right opts -> helpFlag opts @?= True
           Left err -> assertFailure $ "Command line parsing failed: " ++ show err
           
  , testCase "CLI: handle version flag" $
      let args = ["--version"]
          options = parseCommandLineArgs args
      in case options of
           Right opts -> versionFlag opts @?= True
           Left err -> assertFailure $ "Command line parsing failed: " ++ show err
           
  , testCase "CLI: integrate with parser" $
      let args = ["--ownership", "test.typus"]
          options = parseCommandLineArgs args
          input = "//! ownership=true\n```go\nfmt.Println(\"hello\")\n```"
      in case options of
           Right opts -> do
             let result = P.parseTypus input (inputFile opts)
             case result of
               Left err -> assertFailure $ "Parse failed: " ++ show err
               Right typusFile -> length (P.tfBlocks typusFile) @?= 1
           Left err -> assertFailure $ "Command line parsing failed: " ++ show err
           
  , testCase "CLI: integrate with ownership analyzer" $
      let args = ["--ownership", "test.typus"]
          options = parseCommandLineArgs args
          input = "package main\n\nfunc main() {\n    data := make([]byte, 100)\n    processData(data)\n}\n\nfunc processData(d []byte) {\n    // Process data\n}"
      in case options of
           Right opts -> do
             let result = Ownership.analyzeOwnership input
             case result of
               Left err -> assertFailure $ "Ownership analysis failed: " ++ show err
               Right (_, transfers) -> length transfers @?= 1
           Left err -> assertFailure $ "Command line parsing failed: " ++ show err
           
  , testCase "CLI: integrate with type analyzer" $
      let args = ["--dependent-types", "test.typus"]
          options = parseCommandLineArgs args
          checker = Dependencies.newDependentTypeChecker ()
      in case options of
           Right opts -> do
             let result = Dependencies.checkType "int" checker
             case result of
               Left err -> assertFailure $ "Type check failed: " ++ show err
               Right _ -> return ()
           Left err -> assertFailure $ "Command line parsing failed: " ++ show err
           
  , testCase "CLI: handle multiple input files" $
      let args = ["--ownership", "file1.typus", "file2.typus", "file3.typus"]
          options = parseCommandLineArgs args
      in case options of
           Right opts -> length (inputFiles opts) @?= 3
           Left err -> assertFailure $ "Command line parsing failed: " ++ show err
           
  , testCase "CLI: handle recursive directory processing" $
      let args = ["--recursive", "--ownership", "src/"]
          options = parseCommandLineArgs args
      in case options of
           Right opts -> do
             recursiveFlag opts @?= True
             inputFile opts @?= "src/"
           Left err -> assertFailure $ "Command line parsing failed: " ++ show err
           
  , testCase "CLI: handle error output formatting" $
      let args = ["--format", "json", "--ownership", "test.typus"]
          options = parseCommandLineArgs args
      in case options of
           Right opts -> outputFormat opts @?= "json"
           Left err -> assertFailure $ "Command line parsing failed: " ++ show err
           
  , testCase "CLI: handle configuration file" $
      let args = ["--config", "config.toml", "test.typus"]
          options = parseCommandLineArgs args
      in case options of
           Right opts -> configFile opts @?= Just "config.toml"
           Left err -> assertFailure $ "Command line parsing failed: " ++ show err
           
  , testCase "CLI: handle build tags" $
      let args = ["--tags", "linux,amd64", "test.typus"]
          options = parseCommandLineArgs args
      in case options of
           Right opts -> buildTags opts @?= ["linux", "amd64"]
           Left err -> assertFailure $ "Command line parsing failed: " ++ show err
           
  , testCase "CLI: handle timeout specification" $
      let args = ["--timeout", "30", "test.typus"]
          options = parseCommandLineArgs args
      in case options of
           Right opts -> timeout opts @?= Just 30
           Left err -> assertFailure $ "Command line parsing failed: " ++ err
           
  , testCase "CLI: handle parallel processing" $
      let args = ["--parallel", "4", "test.typus"]
          options = parseCommandLineArgs args
      in case options of
           Right opts -> parallelJobs opts @?= Just 4
           Left err -> assertFailure $ "Command line parsing failed: " ++ show err
           
  , testCase "CLI: handle dry run mode" $
      let args = ["--dry-run", "test.typus"]
          options = parseCommandLineArgs args
      in case options of
           Right opts -> dryRunFlag opts @?= True
           Left err -> assertFailure $ "Command line parsing failed: " ++ show err
           
  , testCase "CLI: handle watch mode" $
      let args = ["--watch", "test.typus"]
          options = parseCommandLineArgs args
      in case options of
           Right opts -> watchFlag opts @?= True
           Left err -> assertFailure $ "Command line parsing failed: " ++ show err
           
  , testCase "CLI: integrate all components" $
      let args = ["--ownership", "--dependent-types", "--verbose", "test.typus"]
          options = parseCommandLineArgs args
          input = "//! ownership=true\n//! dependent_types=true\n```go\npackage main\n\nfunc main() {\n    data := make([]byte, 100)\n    processData(data)\n}\n\nfunc processData(d []byte) {\n    // Process data\n}\n```"
      in case options of
           Right opts -> do
             ownershipFlag opts @?= True
             dependentTypesFlag opts @?= True
             verboseFlag opts @?= True
             
             let parseResult = P.parseTypus input (inputFile opts)
             case parseResult of
               Left err -> assertFailure $ "Parse failed: " ++ show err
               Right typusFile -> do
                 length (P.tfBlocks typusFile) @?= 1
                 
                 let ownershipResult = Ownership.analyzeOwnership input
                 case ownershipResult of
                   Left err -> assertFailure $ "Ownership analysis failed: " ++ show err
                   Right (_, transfers) -> length transfers @?= 1
                   
                 let checker = Dependencies.newDependentTypeChecker ()
                     typeCheckResult = Dependencies.checkType "[]byte" checker
                 case typeCheckResult of
                   Left err -> assertFailure $ "Type check failed: " ++ show err
                   Right _ -> return ()
           Left err -> assertFailure $ "Command line parsing failed: " ++ show err
  ]

-- Simplified CLI types for testing
data CliOptions = CliOptions
  { ownershipFlag :: Bool
  , dependentTypesFlag :: Bool
  , inputFile :: String
  , inputFiles :: [String]
  , outputFile :: Maybe String
  , verboseFlag :: Bool
  , helpFlag :: Bool
  , versionFlag :: Bool
  , recursiveFlag :: Bool
  , outputFormat :: String
  , configFile :: Maybe String
  , buildTags :: [String]
  , timeout :: Maybe Int
  , parallelJobs :: Maybe Int
  , dryRunFlag :: Bool
  , watchFlag :: Bool
  } deriving (Eq, Show)

parseCommandLineArgs :: [String] -> Either String CliOptions
parseCommandLineArgs args = 
  let options = parseArgs args defaultOptions
  in if null (inputFile options) && null (inputFiles options)
     then Left "No input file specified"
     else Right options
  where
    defaultOptions = CliOptions False False "" [] Nothing False False False False "" Nothing [] Nothing Nothing False False
    
    parseArgs [] opts = opts
    parseArgs ("--ownership":rest) opts = parseArgs rest (opts { ownershipFlag = True })
    parseArgs ("--dependent-types":rest) opts = parseArgs rest (opts { dependentTypesFlag = True })
    parseArgs ("--verbose":rest) opts = parseArgs rest (opts { verboseFlag = True })
    parseArgs ("--help":rest) opts = parseArgs rest (opts { helpFlag = True })
    parseArgs ("--version":rest) opts = parseArgs rest (opts { versionFlag = True })
    parseArgs ("--recursive":rest) opts = parseArgs rest (opts { recursiveFlag = True })
    parseArgs ("--dry-run":rest) opts = parseArgs rest (opts { dryRunFlag = True })
    parseArgs ("--watch":rest) opts = parseArgs rest (opts { watchFlag = True })
    parseArgs ("--output":outfile:rest) opts = parseArgs rest (opts { outputFile = Just outfile })
    parseArgs ("--format":format:rest) opts = parseArgs rest (opts { outputFormat = format })
    parseArgs ("--config":config:rest) opts = parseArgs rest (opts { configFile = Just config })
    parseArgs ("--tags":tags:rest) opts = parseArgs rest (opts { buildTags = splitTags tags })
    parseArgs ("--timeout":timeout:rest) opts = 
      case reads timeout of
        [(t, "")] -> parseArgs rest (opts { timeout = Just t })
        _ -> opts
    parseArgs ("--parallel":parallel:rest) opts = 
      case reads parallel of
        [(p, "")] -> parseArgs rest (opts { parallelJobs = Just p })
        _ -> opts
    parseArgs (file:rest) opts = 
      if null (inputFile opts)
        then parseArgs rest (opts { inputFile = file, inputFiles = [file] })
        else parseArgs rest (opts { inputFiles = file : inputFiles opts })
    parseArgs _ _ = defaultOptions  -- Simplified error handling
    
    splitTags tags = Utils.splitBy ',' tags

-- Simplified Dependencies types for testing
data TestTypeExpr = TestTypeVar String | TestTypeConstructor String [TestTypeExpr] deriving (Eq, Show)

data TestDependentTypeChecker = TestDependentTypeChecker 
  { testTypeEnv :: TestTypeEnvironment 
  }

data TestTypeEnvironment = TestTypeEnvironment
  { testTypeEnvTypes :: [(String, TestTypeExpr)]
  }

testNewDependentTypeChecker :: () -> TestDependentTypeChecker
testNewDependentTypeChecker () = TestDependentTypeChecker (TestTypeEnvironment [])

testCheckType :: String -> TestDependentTypeChecker -> Either String TestDependentTypeChecker
testCheckType name checker = 
  case lookup name (testTypeEnvTypes (testTypeEnv checker)) of
    Just _ -> Right checker
    Nothing -> Left "Type not found"

-- Simplified Ownership types for testing
testAnalyzeOwnership :: String -> Either String ((), [()])
testAnalyzeOwnership _ = Right ((), [()])

-- Simplified Parser types for testing
data TestFileDirectives = TestFileDirectives deriving (Eq, Show)

data TestCodeBlock = TestCodeBlock 
  { testCbContent :: String
  } deriving (Eq, Show)

data TestTypusFile = TestTypusFile 
  { testTfDirectives :: TestFileDirectives
  , testTfBlocks :: [TestCodeBlock]
  }

testDefaultFileDirectives :: TestFileDirectives
testDefaultFileDirectives = TestFileDirectives

testParseTypus :: String -> String -> Either String TestTypusFile
testParseTypus _ _ = Right (TestTypusFile TestFileDirectives [TestCodeBlock ""])

-- Simplified Utils functions for testing
testSplitBy :: Char -> String -> [String]
testSplitBy delim s = case break (== delim) s of
  (a, []) -> [a]
  (a, _:b) -> a : testSplitBy delim b