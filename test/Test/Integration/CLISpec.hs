module Test.Integration.CLISpec (tests) where

import qualified Cli.Runner as CliRunner
import Control.Exception (bracket_)
import Data.List (isInfixOf)
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

-- | End-to-end CLI tests that exercise the real 'typus' executable via Stack when available.
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
          assertBool "convert should not emit stderr" (null stderrOutput)
          exists <- doesFileExist output
          assertBool "converted Go file should exist" exists
          generated <- readFile output
          assertBool "expected generated Go code to declare package main" ("package main" `isInfixOf` generated)

    , testCase "typus check reports success for well-formed modules" $ do
        (exitCode, stdout, stderrOutput) <- runTypusCli ["check", "test/data/cli_valid.typus"]
        exitCode @?= ExitSuccess
        assertBool "check should confirm success" ("Typus syntax and compilation OK" `isInfixOf` stdout)
        stderrOutput @?= ""

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
          stderrOutput @?= ""

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
          stderrOutput @?= ""

    , testCase "typus run fails when the entrypoint is missing" $ do
        (exitCode, stdout, _stderrOutput) <- runTypusCli ["run", "does-not-exist.typus"]
        exitCode @?= ExitFailure 1
        assertBool "missing file error should be rendered" ("File not found" `isInfixOf` stdout)
    ]

data CliExecution
  = UseStackBinary FilePath
  | UseInProcessRunner


runTypusCli :: [String] -> IO (ExitCode, String, String)
runTypusCli args = do
  execution <- resolveExecution
  case execution of
    UseStackBinary stackPath -> runViaStack stackPath args
    UseInProcessRunner -> runInProcess args

resolveExecution :: IO CliExecution
resolveExecution = do
  envOverride <- lookupEnv "TYPUS_FAKE_STACK"
  case envOverride of
    Just path -> pure (UseStackBinary path)
    Nothing -> do
      mStack <- findExecutable "stack"
      pure (maybe UseInProcessRunner UseStackBinary mStack)

runViaStack :: FilePath -> [String] -> IO (ExitCode, String, String)
runViaStack stackPath args = do
  envVars <- getEnvironment
  let sanitizedEnv = ("TYPUS_SKIP_GO_BUILD","1") : filter ((/= "TYPUS_SKIP_GO_BUILD") . fst) envVars
      processSpec = (proc stackPath ("exec" : "typus" : "--" : args))
        { env = Just sanitizedEnv
        }
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
    restore Nothing = unsetEnv key

withTempFixture :: FilePath -> (FilePath -> FilePath -> IO a) -> IO a
withTempFixture fixture action =
  withSystemTempDirectory "typus-cli-fixture" $ \tmpDir -> do
    let target = tmpDir </> takeFileName fixture
    copyFile fixture target
    action tmpDir target
