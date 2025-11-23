module Test.Unit.CliRunnerSpec (tests) where

import qualified Cli.Runner as CliRunner
import Control.Exception (bracket_)
import Data.List (isInfixOf)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode(..))
import System.IO (stderr)
import System.IO.Silently (capture, hCapture)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

withEnvOverride :: String -> Maybe String -> IO a -> IO a
withEnvOverride key value action = do
    original <- lookupEnv key
    let apply Nothing = unsetEnv key
        apply (Just v) = setEnv key v
    bracket_ (apply value) (apply original) action

runRunner :: [String] -> IO (ExitCode, String, String)
runRunner args =
    withEnvOverride "TYPUS_SKIP_GO_BUILD" (Just "1") $ do
        (stdoutOutput, (stderrOutput, exitCode)) <- capture (hCapture [stderr] (CliRunner.runWithArgs args))
        pure (exitCode, stdoutOutput, stderrOutput)

missingPath :: FilePath
missingPath = "test/data/this-file-does-not-exist.typus"

tests :: TestTree
tests =
    testGroup "Cli.Runner"
        [ testCase "run command without entrypoint fails fast" $ do
            (exitCode, stdoutOutput, stderrOutput) <- runRunner ["run"]
            exitCode @?= ExitFailure 1
            assertBool "error should mention invalid argument" ("Invalid argument" `isInfixOf` stdoutOutput)
            stderrOutput @?= ""

        , testCase "check surfaces file-not-found errors before invoking Go" $ do
            (exitCode, stdoutOutput, stderrOutput) <- runRunner ["check", missingPath]
            exitCode @?= ExitFailure 1
            assertBool "error should mention missing file" ("File not found" `isInfixOf` stdoutOutput)
            stderrOutput @?= ""

        , testCase "build reports missing directory inputs" $ do
            (exitCode, stdoutOutput, stderrOutput) <- runRunner ["build", "test/fixtures/non-existent-project"]
            exitCode @?= ExitFailure 1
            assertBool "error should mention missing path" ("Path does not exist" `isInfixOf` stdoutOutput)
            stderrOutput @?= ""
        ]
