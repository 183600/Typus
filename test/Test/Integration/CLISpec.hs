module Test.Integration.CLISpec (tests) where

import Cli (Args(..))
import Cli.Runner (runCliWithContext)
import CompilerUtils (CompilerContext, newCompilerContextWithExecutor, silentLogger)
import Control.Monad.Except (ExceptT(..))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (isInfixOf)
import GoToolchain (GoExecutor(..))
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, (@?=), testCase)
import Tooling.Error (ToolingError(..), renderToolingError)

-- | CLI integration tests that execute the dispatcher in-process while
-- exercising the real compiler utilities.
tests :: TestTree
tests =
  testGroup "CLI workflows"
    [ testCase "convert command emits Go output" testConvertCommand
    , testCase "check command triggers go build" testCheckCommandInvokesGo
    , testCase "check command surfaces syntax errors" testCheckCommandSyntaxFailure
    , testCase "build command forwards custom go args" testBuildCommandForwardsArgs
    , testCase "build command fails when target is missing" testBuildCommandMissingTarget
    , testCase "run command executes go run" testRunCommandInvokesGo
    , testCase "run command fails when file is missing" testRunCommandMissingFile
    ]

--------------------------------------------------------------------------------
-- Test cases
--------------------------------------------------------------------------------

testConvertCommand :: IO ()
testConvertCommand =
  withSystemTempDirectory "typus-cli-convert" $ \tmpDir -> do
    let input = "test/data/simple_go_code.typus"
        output = tmpDir </> "generated.go"
    ctx <- contextWith recordingExecutor
    result <- runCliWithContext ctx (Convert input output)
    assertCliSuccess result
    exists <- doesFileExist output
    assertBool "converted Go file should exist" exists
    generated <- readFile output
    assertBool "generated Go should declare package main" ("package main" `isInfixOf` generated)

testCheckCommandInvokesGo :: IO ()
testCheckCommandInvokesGo = do
  (ctx, invocations) <- contextWithRecorder
  result <- runCliWithContext ctx (Check "test/data/simple_go_code.typus")
  assertCliSuccess result
  recorded <- readIORef invocations
  assertBool "expected go build invocation" (any (invokesGoSubcommand "build") recorded)

testCheckCommandSyntaxFailure :: IO ()
testCheckCommandSyntaxFailure =
  withSystemTempDirectory "typus-cli-check-failure" $ \tmpDir -> do
    let invalid = tmpDir </> "broken.typus"
    writeFile invalid (unlines
      [ "package main"
      , "func main() {"
      , "    println(\"oops\")"
      ])
    ctx <- contextWith skippingExecutor
    result <- runCliWithContext ctx (Check invalid)
    case result of
      Left (SyntaxValidationFailed file issues) -> do
        file @?= invalid
        assertBool "expected syntax issues" (not (null issues))
      Left err -> assertFailure $ "unexpected error: " ++ renderToolingError err
      Right _ -> assertFailure "expected syntax validation failure"

testBuildCommandForwardsArgs :: IO ()
testBuildCommandForwardsArgs =
  withSystemTempDirectory "typus-cli-build" $ \tmpDir -> do
    let projectDir = tmpDir </> "proj"
        source = projectDir </> "main.typus"
        goArgs = [projectDir, "-v"]
    createDirectoryIfMissing True projectDir
    writeFile source sampleProgram
    (ctx, invocations) <- contextWithRecorder
    result <- runCliWithContext ctx (Build False goArgs)
    assertCliSuccess result
    recorded <- readIORef invocations
    case recorded of
      [] -> assertFailure "expected go build invocation"
      (args, _):_ -> do
        take 1 args @?= ["build"]
        drop 1 args @?= ["-v"]

testBuildCommandMissingTarget :: IO ()
testBuildCommandMissingTarget =
  withSystemTempDirectory "typus-cli-build-missing" $ \tmpDir -> do
    let missing = tmpDir </> "ghost-project"
    ctx <- contextWith recordingExecutor
    result <- runCliWithContext ctx (Build False [missing])
    case result of
      Left (PathDoesNotExist path) -> path @?= missing
      Left err -> assertFailure $ "unexpected error: " ++ renderToolingError err
      Right _ -> assertFailure "expected missing path failure"

testRunCommandInvokesGo :: IO ()
testRunCommandInvokesGo =
  withSystemTempDirectory "typus-cli-run" $ \tmpDir -> do
    let source = tmpDir </> "main.typus"
    writeFile source sampleProgram
    (ctx, invocations) <- contextWithRecorder
    result <- runCliWithContext ctx (Run False [source, "--debug"])
    assertCliSuccess result
    recorded <- readIORef invocations
    assertBool "expected go run invocation" (any (invokesGoSubcommand "run") recorded)
    -- The extra arguments should be forwarded to go run.
    let forwardedArgs = [args | ("run":args, _) <- recorded]
    assertBool "expected forwarded --debug flag" (any ("--debug" `elem`) forwardedArgs)

testRunCommandMissingFile :: IO ()
testRunCommandMissingFile = do
  ctx <- contextWith recordingExecutor
  result <- runCliWithContext ctx (Run False ["does-not-exist.typus"])
  case result of
    Left (FileNotFound path) -> path @?= "does-not-exist.typus"
    Left err -> assertFailure $ "unexpected error: " ++ renderToolingError err
    Right _ -> assertFailure "expected missing file failure"

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

type GoInvocation = ([String], FilePath)

contextWith :: IO (GoExecutor, IORef [GoInvocation]) -> IO CompilerContext
contextWith mkExecutor = do
  (executor, _) <- mkExecutor
  newCompilerContextWithExecutor silentLogger executor

contextWithRecorder :: IO (CompilerContext, IORef [GoInvocation])
contextWithRecorder = do
  (executor, ref) <- recordingExecutor
  ctx <- newCompilerContextWithExecutor silentLogger executor
  pure (ctx, ref)

assertCliSuccess :: Either ToolingError () -> IO ()
assertCliSuccess result =
  case result of
    Left err -> assertFailure ("CLI command failed: " ++ renderToolingError err)
    Right _  -> pure ()

invokesGoSubcommand :: String -> GoInvocation -> Bool
invokesGoSubcommand command (args, _) =
  case args of
    [] -> False
    (cmd:_) -> cmd == command

recordingExecutor :: IO (GoExecutor, IORef [GoInvocation])
recordingExecutor = do
  ref <- newIORef []
  let exec = GoExecutor
        { goShouldSkip = pure False
        , goRunCommandInDir = \args dir -> ExceptT $ do
            modifyIORef' ref (\xs -> (args, dir) : xs)
            pure (Right ())
        }
  pure (exec, ref)

skippingExecutor :: IO (GoExecutor, IORef [GoInvocation])
skippingExecutor = do
  ref <- newIORef []
  let exec = GoExecutor
        { goShouldSkip = pure True
        , goRunCommandInDir = \args dir -> ExceptT $ do
            modifyIORef' ref (\xs -> (args, dir) : xs)
            pure (Right ())
        }
  pure (exec, ref)

sampleProgram :: String
sampleProgram = unlines
  [ "package main"
  , "import \"fmt\""
  , "func main() {"
  , "    fmt.Println(\"hello\")"
  , "}"
  ]
