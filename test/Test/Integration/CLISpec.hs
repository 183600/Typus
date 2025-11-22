module Test.Integration.CLISpec (tests) where

import Data.List (isInfixOf)
import System.Directory (doesFileExist, findExecutable)
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess(..), proc, readCreateProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, (@?=), testCase)

-- | End-to-end CLI tests that exercise the real 'typus' executable via Stack.
tests :: TestTree
tests =
  testGroup "CLI smoke tests"
    [ testCase "typus --version prints the version banner" $ do
        (exitCode, stdout, stderr) <- runTypusCli ["--version"]
        exitCode @?= ExitSuccess
        assertBool "version banner missing" ("typus version" `isInfixOf` stdout)
        stderr @?= ""

    , testCase "typus convert emits Go output" $ do
        withSystemTempDirectory "typus-cli" $ \tmpDir -> do
          let input = "test/data/simple_go_code.typus"
              output = tmpDir </> "generated.go"
          (exitCode, _stdout, stderr) <- runTypusCli ["convert", input, "-o", output]
          exitCode @?= ExitSuccess
          assertBool "convert should not emit stderr" (null stderr)
          exists <- doesFileExist output
          assertBool "converted Go file should exist" exists
          generated <- readFile output
          assertBool "expected generated Go code to declare package main" ("package main" `isInfixOf` generated)
    ]

runTypusCli :: [String] -> IO (ExitCode, String, String)
runTypusCli args = do
  stackPath <- requireStackBinary
  envVars <- getEnvironment
  let sanitizedEnv = ("TYPUS_SKIP_GO_BUILD","1") : filter ((/= "TYPUS_SKIP_GO_BUILD") . fst) envVars
      processSpec = (proc stackPath ("exec" : "typus" : "--" : args))
        { env = Just sanitizedEnv
        }
  readCreateProcessWithExitCode processSpec ""

requireStackBinary :: IO FilePath
requireStackBinary = do
  mStack <- findExecutable "stack"
  case mStack of
    Just path -> pure path
    Nothing -> do
      assertFailure "The stack executable is required to run CLI integration tests"
      pure "stack"
