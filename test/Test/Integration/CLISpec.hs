module Test.Integration.CLISpec (tests) where

import qualified Cli.Runner as CliRunner
import Control.Exception (bracket_)
import System.Directory (copyFile, doesFileExist, findExecutable)
import System.Environment (getEnvironment, lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeFileName)
import System.IO (stderr)
import System.IO.Silently (capture, hCapture)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess(..), proc, readCreateProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, (@?=), testCase)
import Data.List (isInfixOf)

-- | Filter out HPC deprecation warnings from stderr output
filterHpcWarnings :: String -> String
filterHpcWarnings input = 
    if "Deprecation warning:" `isInfixOf` input && "tix file" `isInfixOf` input
    then ""
    else input

-- | End-to-end CLI tests that exercise the real 'typus' executable when available.
--   We first try to run the 'typus' binary directly (thanks to build-tool-depends putting it on PATH).
--   If it's not found, we fall back to running the CLI in-process.
tests :: TestTree
tests =
  testGroup "CLI smoke tests"
    [ testCase "typus --version prints the version banner" $ do
        (exitCode, stdout, stderrOutput) <- runTypusCli ["--version"]
        exitCode @?= ExitSuccess
        assertBool "version banner missing" ("typus version" `isInfixOf` stdout)
        stderrOutput @?= ""

    , testCase "typus convert emits Go output" $ do
        withSystemTempDirectory "typus-cli" $ \tmpDir -> do
          let input = "test/data/simple_go_code.typus"
              output = tmpDir </> "generated.go"
          (exitCode, _stdout, stderrOutput) <- runTypusCli ["convert", input, "-o", output]
          exitCode @?= ExitSuccess
          let filteredStderr = filterHpcWarnings stderrOutput
          assertBool "convert should not emit stderr" (null filteredStderr)
          exists <- doesFileExist output
          assertBool "converted Go file should exist" exists
          generated <- readFile output
          assertBool "expected generated Go code to declare package main" ("package main" `isInfixOf` generated)

    , testCase "typus check reports success for well-formed modules" $ do
        (exitCode, stdout, stderrOutput) <- runTypusCli ["check", "test/data/cli_valid.typus"]
        exitCode @?= ExitSuccess
        assertBool "check should confirm success" ("Typus syntax and compilation OK" `isInfixOf` stdout)
        let filteredStderr = filterHpcWarnings stderrOutput
        filteredStderr @?= ""

    , testCase "typus check fails fast on syntax errors" $ do
        (exitCode, stdout, _stderrOutput) <- runTypusCli ["check", "test/data/cli_invalid.typus"]
        exitCode @?= ExitFailure 1
        assertBool "check failure should mention syntax" ("Syntax validation failed" `isInfixOf` stdout || "Error:" `isInfixOf` stdout)

    , testCase "typus build converts directories and invokes go" $ do
        withTempFixture "test/data/cli_valid.typus" $ \projectDir _ -> do
          (exitCode, stdout, stderrOutput) <- runTypusCli ["build", projectDir]
          exitCode @?= ExitSuccess
          assertBool "build should log conversions" ("Converted:" `isInfixOf` stdout)
          assertBool "build should invoke go" ("Skipping Go command: go build" `isInfixOf` stdout)
          let filteredStderr = filterHpcWarnings stderrOutput
          filteredStderr @?= ""

    , testCase "typus build --strict-embed surfaces missing assets" $ do
        withTempFixture "test/data/cli_missing_embed.typus" $ \projectDir _ -> do
          (exitCode, stdout, _stderrOutput) <- runTypusCli ["build", "--strict-embed", projectDir]
          exitCode @?= ExitFailure 1
          assertBool "missing embed error should be rendered" ("Missing embedded assets" `isInfixOf` stdout)

    , testCase "typus run shells out to go run via the stub" $ do
        withTempFixture "test/data/cli_valid.typus" $ \_ mainFile -> do
          (exitCode, stdout, stderrOutput) <- runTypusCli ["run", mainFile]
          exitCode @?= ExitSuccess
          assertBool "run should trigger go run" ("Skipping Go command: go run" `isInfixOf` stdout)
          let filteredStderr = filterHpcWarnings stderrOutput
          filteredStderr @?= ""

    , testCase "typus run fails when the entrypoint is missing" $ do
        (exitCode, stdout, _stderrOutput) <- runTypusCli ["run", "does-not-exist.typus"]
        exitCode @?= ExitFailure 1
        assertBool "missing file error should be rendered" ("File not found" `isInfixOf` stdout)
    ]

data CliExecution
  = UseTypusBinary FilePath
  | UseInProcessRunner

runTypusCli :: [String] -> IO (ExitCode, String, String)
runTypusCli args = do
  execution <- resolveExecution
  case execution of
    UseTypusBinary exePath -> runViaExe exePath args
    UseInProcessRunner     -> runInProcess args

resolveExecution :: IO CliExecution
resolveExecution = do
  -- Optional override: set TYPUS_BIN to point to a specific typus executable
  mOverride <- lookupEnv "TYPUS_BIN"
  case mOverride of
    Just path -> pure (UseTypusBinary path)
    Nothing -> do
      mTypus <- findExecutable "typus"
      pure (maybe UseInProcessRunner UseTypusBinary mTypus)

runViaExe :: FilePath -> [String] -> IO (ExitCode, String, String)
runViaExe typusPath args = do
  envVars <- getEnvironment
  -- Ensure we always skip actual Go toolchain calls during tests
  let sanitizedEnv = ("TYPUS_SKIP_GO_BUILD","1") : filter ((/= "TYPUS_SKIP_GO_BUILD") . fst) envVars
      processSpec  = (proc typusPath args) { env = Just sanitizedEnv }
  readCreateProcessWithExitCode processSpec ""

runInProcess :: [String] -> IO (ExitCode, String, String)
runInProcess args =
  withEnvOverride "TYPUS_SKIP_GO_BUILD" "1" $ do
    (stdoutOutput, (stderrOutput, exitCode)) <- capture (hCapture [stderr] (CliRunner.runWithArgs args))
    pure (exitCode, stdoutOutput, stderrOutput)

withEnvOverride :: String -> String -> IO a -> IO a
withEnvOverride key value action = do
  original <- lookupEnv key
  bracket_ (setEnv key value) (restore original) action
  where
    restore (Just prior) = setEnv key prior
    restore Nothing      = unsetEnv key

withTempFixture :: FilePath -> (FilePath -> FilePath -> IO a) -> IO a
withTempFixture fixture action =
  withSystemTempDirectory "typus-cli-fixture" $ \tmpDir -> do
    let target = tmpDir </> takeFileName fixture
    copyFile fixture target
    action tmpDir target