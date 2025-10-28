module CLICommandTests (
    cliCommandTestSuite,
    runCLICommandTests
) where

import Test.Tasty
import Test.Tasty.HUnit as HU
import System.Exit (ExitCode(..))
import System.Directory (doesFileExist, removeFile, createDirectoryIfMissing, findExecutable)
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Control.Exception (try, SomeException, throwIO)
import Control.Monad (when)
import System.Environment (withArgs, getArgs, lookupEnv)
-- Removed unused IO imports

-- Main test function
runCLICommandTests :: IO ()
runCLICommandTests = do
    putStrLn "=== Running CLI Command Tests ==="
    putStrLn "[DEBUG] Starting CLI command tests..."
    args <- getArgs
    putStrLn $ "[DEBUG] Process args: " ++ show args
    breakpointIfRequested
    -- Reset args so tasty doesn't choke on repo-level flags like --skip=performance
    result <- try (withArgs [] (defaultMain cliCommandTestSuite)) :: IO (Either SomeException ())
    case result of
        Left ex -> do
            putStrLn $ "[DEBUG] CLI command tests failed with exception: " ++ show ex
            throwIO ex
        Right () -> do
            putStrLn "[DEBUG] CLI command tests completed successfully"
            return ()

-- Test suite definition
cliCommandTestSuite :: TestTree
cliCommandTestSuite = testGroup "CLI Command Tests"
    [ testCase "Test typus --version command" testVersionCommand
    , testCase "Test typus -v command" testShortVersionCommand
    , testCase "Test typus check command with valid file" testCheckValidFile
    , testCase "Test typus check command with invalid file" testCheckInvalidFile
    , testCase "Test typus convert command" testConvertCommand
    , testCase "Test typus build command" testBuildCommand
    , testCase "Test typus run command" testRunCommand
    ]

-- Test version command
testVersionCommand :: IO ()
testVersionCommand = do
    (exitCode, stdout', _stderr) <- readProcessWithExitCode "stack" ["exec", "--", "typus", "--version"] ""
    case exitCode of
        ExitSuccess -> do
            assertBool "Version output should contain 'typus'" ("typus" `isInfixOf` stdout')
            assertBool "Version output should contain version number" (any (`elem` stdout') ['0'..'9'])
        ExitFailure code -> 
            assertFailure $ "typus --version failed with exit code: " ++ show code

-- Test short version command
testShortVersionCommand :: IO ()
testShortVersionCommand = do
    (exitCode, stdout', _stderr) <- readProcessWithExitCode "stack" ["exec", "--", "typus", "-v"] ""
    case exitCode of
        ExitSuccess -> do
            assertBool "Version output should contain 'typus'" ("typus" `isInfixOf` stdout')
            assertBool "Version output should contain version number" (any (`elem` stdout') ['0'..'9'])
        ExitFailure code -> 
            assertFailure $ "typus -v failed with exit code: " ++ show code

-- Test check command with valid file
testCheckValidFile :: IO ()
testCheckValidFile = do
    -- Create a temporary valid Typus file
    let tempFile = "test_temp_valid.typus"
    let validContent = unlines
            [ "package main"
            , "import \"fmt\""
            , "func main() {"
            , "    fmt.Println(\"Hello, Typus!\")"
            , "}"
            ]
    
    -- Write test file
    writeFile tempFile validContent
    
    -- Test check command
    (exitCode, _stdout, _stderr) <- readProcessWithExitCode "stack" ["exec", "--", "typus", "check", tempFile] ""
    
    -- Clean up
    removeFile tempFile
    
    case exitCode of
        ExitSuccess -> return () -- Check should succeed for valid file
        ExitFailure code -> 
            assertFailure $ "typus check failed on valid file with exit code: " ++ show code

-- Test check command with invalid file
testCheckInvalidFile :: IO ()
testCheckInvalidFile = do
    -- Create a temporary invalid Typus file
    let tempFile = "test_temp_invalid.typus"
    let invalidContent = unlines
            [ "package main"
            , "import \"fmt\""
            , "func main() {"
            , "    fmt.Println(\"Hello, Typus!\""  -- Missing closing parenthesis
            , "}"
            ]
    
    -- Write test file
    writeFile tempFile invalidContent
    
    -- Test check command
    (exitCode, _stdout, _stderr) <- readProcessWithExitCode "stack" ["exec", "--", "typus", "check", tempFile] ""
    
    -- Clean up
    removeFile tempFile
    
    -- Check command should fail for invalid file
    case exitCode of
        ExitFailure _ -> return () -- Expected to fail
        ExitSuccess -> assertFailure "typus check should have failed on invalid file"

-- Test convert command
testConvertCommand :: IO ()
testConvertCommand = do
    -- Create test directories
    createDirectoryIfMissing True "test_temp_input"
    createDirectoryIfMissing True "test_temp_output"
    
    -- Create a simple Typus file
    let inputFile = "test_temp_input" </> "simple.typus"
    let typusContent = unlines
            [ "package main"
            , "import \"fmt\""
            , "func main() {"
            , "    fmt.Println(\"Hello from Typus!\")"
            , "}"
            ]
    
    writeFile inputFile typusContent
    
    -- Test convert command (convert single file to single output file)
    let outputFile = "test_temp_output" </> "simple.go"
    (exitCode, _stdout, _stderr) <- readProcessWithExitCode "stack" 
        ["exec", "--", "typus", "convert", inputFile, "-o", outputFile] ""
    
    -- Check if output file was created
    outputExists <- doesFileExist outputFile
    
    -- Clean up
    removeFile inputFile
    when outputExists $ removeFile outputFile
    
    case exitCode of
        ExitSuccess -> do
            assertBool "Convert should create output file" outputExists
        ExitFailure code -> 
            assertFailure $ "typus convert failed with exit code: " ++ show code

-- Test build command
testBuildCommand :: IO ()
testBuildCommand = do
    putStrLn "[DEBUG] Starting testBuildCommand..."
    -- Create a temporary directory with a simple Typus project
    createDirectoryIfMissing True "test_temp_project"
    
    -- Create main.typus file
    let mainFile = "test_temp_project" </> "main.typus"
    let mainContent = unlines
            [ "package main"
            , "import \"fmt\""
            , "func main() {"
            , "    fmt.Println(\"Hello from Typus build!\")"
            , "}"
            ]
    
    writeFile mainFile mainContent
    putStrLn $ "[DEBUG] Created test file: " ++ mainFile
    
    -- Test build command (this should convert to Go and then call go build)
    putStrLn "[DEBUG] Running: stack exec -- typus build test_temp_project"
    (exitCode, stdout', stderr') <- readProcessWithExitCode "stack" 
        ["exec", "--", "typus", "build", "test_temp_project"] ""
    
    putStrLn $ "[DEBUG] Build command exit code: " ++ show exitCode
    putStrLn $ "[DEBUG] Build command stdout: " ++ stdout'
    putStrLn $ "[DEBUG] Build command stderr: " ++ stderr'
    
    -- Clean up
    removeFile mainFile
    
    -- If Go is available, build should succeed; otherwise, skip expectation
    goAvail <- isGoAvailable
    case (goAvail, exitCode) of
        (True, ExitSuccess) -> putStrLn "[DEBUG] Build command succeeded"
        (True, ExitFailure code) -> HU.assertFailure $ "typus build failed with exit code: " ++ show code ++ " while Go is available"
        (False, _) -> putStrLn "[DEBUG] Go toolchain not found; skipping strict assertion for build"

-- Test run command
testRunCommand :: IO ()
testRunCommand = do
    -- Create a temporary Typus file
    let tempFile = "test_temp_run.typus"
    let typusContent = unlines
            [ "package main"
            , "import \"fmt\""
            , "func main() {"
            , "    fmt.Println(\"Hello from Typus run!\")"
            , "}"
            ]
    
    writeFile tempFile typusContent
    
    -- Test run command
    (exitCode, stdout', _stderr) <- readProcessWithExitCode "stack" 
        ["exec", "--", "typus", "run", tempFile] ""
    
    -- Clean up
    removeFile tempFile
    
    goAvail <- isGoAvailable
    case (goAvail, exitCode) of
        (True, ExitSuccess) -> do
            assertBool "Run should produce output" (not (null stdout'))
        (True, ExitFailure code) -> HU.assertFailure $ "typus run failed with exit code: " ++ show code
        (False, _) -> putStrLn "[DEBUG] Go toolchain not found; skipping strict assertion for run"

-- Helper function for substring checking
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `isSubsequenceOf` haystack

-- Simple substring check
isSubsequenceOf :: String -> String -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (n:ns) (h:hs) 
    | n == h = isSubsequenceOf ns hs
    | otherwise = isSubsequenceOf (n:ns) hs

-- Optional CLI breakpoint controlled via env var
breakpointIfRequested :: IO ()
breakpointIfRequested = do
    m <- lookupEnv "TYPUS_CLI_DEBUG_WAIT"
    case m of
        Just v | v == "1" || v == "true" || v == "TRUE" -> do
            putStrLn "[BREAKPOINT] CLICommandTests: press Enter to continue..."
            _ <- getLine
            return ()
        _ -> return ()

-- Detect if Go toolchain is available in PATH
isGoAvailable :: IO Bool
isGoAvailable = do
    m <- findExecutable "go"
    return (m /= Nothing)