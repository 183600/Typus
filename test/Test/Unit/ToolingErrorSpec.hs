module Test.Unit.ToolingErrorSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual)

import Tooling.Error (ToolingError(..), GoCommandFailure(..), goCommandFailed, renderToolingError)
import qualified Data.Text as T

tests :: TestTree
tests = testGroup "Tooling.Error Tests"
  [ testGoCommandFailure
  , testGoCommandFailedHelper
  , testRenderToolingError
  , testToolingErrorEquality
  ]

testGoCommandFailure :: TestTree
testGoCommandFailure = testCase "GoCommandFailure structure" $ do
  let failure = GoCommandFailure
        { gcfCommand = "go"
        , gcfArgs = ["build", "-o", "test"]
        , gcfWorkingDir = "/tmp"
        , gcfExitCode = 1
        , gcfStdout = "Build output"
        , gcfStderr = "Error message"
        }
  
  assertEqual "Command should match" "go" (gcfCommand failure)
  assertEqual "Args should match" ["build", "-o", "test"] (gcfArgs failure)
  assertEqual "Working directory should match" "/tmp" (gcfWorkingDir failure)
  assertEqual "Exit code should match" 1 (gcfExitCode failure)
  assertEqual "Stdout should match" "Build output" (gcfStdout failure)
  assertEqual "Stderr should match" "Error message" (gcfStderr failure)

testGoCommandFailedHelper :: TestTree
testGoCommandFailedHelper = testCase "goCommandFailed helper function" $ do
  let error = goCommandFailed "go" ["build"] "/test" 2 "stdout" "stderr"
  
  case error of
    GoCommandFailed failure -> do
      assertEqual "Command should be 'go'" "go" (gcfCommand failure)
      assertEqual "Args should be ['build']" ["build"] (gcfArgs failure)
      assertEqual "Working dir should be '/test'" "/test" (gcfWorkingDir failure)
      assertEqual "Exit code should be 2" 2 (gcfExitCode failure)
      assertEqual "Stdout should match" "stdout" (gcfStdout failure)
      assertEqual "Stderr should match" "stderr" (gcfStderr failure)
    _ -> assertBool "Should be GoCommandFailed" False

testRenderToolingError :: TestTree
testRenderToolingError = testCase "renderToolingError produces non-empty output" $ do
  let errors = 
        [ FileNotFound "/missing/file"
        , PathDoesNotExist "/missing/path"
        , NotADirectory "/not/a/dir"
        , InvalidArgument "bad arg"
        , ParserError "/test/file" "parse error"
        , GoToolchainUnavailable "go not found"
        ]
  
  mapM_ (\err -> do
    let rendered = renderToolingError err
    assertBool ("Rendered error should not be empty for: " ++ show err) 
               (not $ T.null rendered)
  ) errors

testToolingErrorEquality :: TestTree
testToolingErrorEquality = testCase "ToolingError equality" $ do
  let error1 = FileNotFound "/test/file"
      error2 = FileNotFound "/test/file"
      error3 = FileNotFound "/other/file"
  
  assertBool "Same errors should be equal" (error1 == error2)
  assertBool "Different errors should not be equal" (error1 /= error3)