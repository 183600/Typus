module Test.Integration.CLISpec (tests) where

import qualified Cli.Runner as CliRunner
import Control.Exception (bracket_)
import Data.List (isInfixOf)
import System.Directory (doesFileExist, findExecutable)
import System.Environment (getEnvironment, lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
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
    (stdoutOutput, (stderrOutput, exitCode)) <- capture (hCapture stderr (CliRunner.runWithArgs args))
    pure (exitCode, stdoutOutput, stderrOutput)

withEnvOverride :: String -> String -> IO a -> IO a
withEnvOverride key value action = do
  original <- lookupEnv key
  bracket_ (setEnv key value) (restore original) action
  where
    restore (Just prior) = setEnv key prior
    restore Nothing = unsetEnv key
