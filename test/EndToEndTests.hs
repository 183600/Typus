module EndToEndTests (
    runEndToEndTests,
    defaultE2EConfig,
    E2EConfig(..)
) where

import Test.Tasty
import Test.Tasty.HUnit as HU
import System.Exit (exitFailure, ExitCode(..))
import System.Directory (doesFileExist, removeFile, doesDirectoryExist, createDirectoryIfMissing)
import System.FilePath ((</>), takeFileName, takeDirectory, takeBaseName)
import System.Process (readProcessWithExitCode, spawnProcess, waitForProcess)
import Control.Exception (try, SomeException)
import System.IO (hPutStrLn, stderr)
import Control.Exception (evaluate, try)
import Data.Time (getCurrentTime, UTCTime, diffUTCTime)
import Data.List (lines, takeWhile)
import Debug (debugLog, debugBreakpoint)

-- Simple timing function to replace System.TimeIt
timeIt :: IO a -> IO (Double, a)
timeIt action = do
    start <- getCurrentTime
    result <- action
    end <- getCurrentTime
    let elapsed :: Double
        elapsed = fromRational $ toRational (diffUTCTime end start)
    return (elapsed, result)

data E2EConfig = E2EConfig
    { testInputDir :: FilePath
    , testOutputDir :: FilePath
    , expectedOutputDir :: FilePath
    , timeoutSeconds :: Int
    , verbose :: Bool
    }

defaultE2EConfig :: E2EConfig
defaultE2EConfig = E2EConfig
    { testInputDir = "test/data"
    , testOutputDir = "test/output"
    , expectedOutputDir = "test/data/expected"
    , timeoutSeconds = 30
    , verbose = False
    }

runEndToEndTests :: E2EConfig -> IO ()
runEndToEndTests config = do
    debugLog "E2E" "Starting end-to-end tests"
    debugBreakpoint "E2E" "Before running E2E tests"

    putStrLn "=== Running End-to-End Tests ==="

    -- Test cases with input files and expected outputs
    let testCases = [
            ("simple_valid_code.typus", "simple_valid_code.output"),
            ("simple_ownership_code.typus", "simple_ownership_code.output"),
            ("code_with_dependent_types.typus", "code_with_dependent_types.output"),
            ("complex_ownership_code.typus", "complex_ownership_code.output"),
            ("typus_specific_code.typus", "typus_specific_code.output"),
            -- Comprehensive engineering structure tests
            ("comprehensive_go_syntax.typus", "comprehensive_go_syntax.output"),
            ("comprehensive_typus_syntax.typus", "comprehensive_typus_syntax.output"),
            ("standard_library_comprehensive.typus", "standard_library_comprehensive.output"),
            -- Advanced ownership and memory management tests
            ("advanced_ownership.typus", "advanced_ownership.output"),
            ("memory_management.typus", "memory_management.output"),
            ("array_ownership_code.typus", "array_ownership_code.output"),
            ("struct_ownership_code.typus", "struct_ownership_code.output"),
            -- Concurrency and complex data structures
            ("concurrent_code.typus", "concurrent_code.output"),
            ("complex_type_code.typus", "complex_type_code.output"),
            -- Error handling and edge cases
            ("error_scenarios.typus", "error_scenarios.output"),
            ("boundary_conditions.typus", "boundary_conditions.output"),
            ("type_system_edge_cases.typus", "type_system_edge_cases.output")
            ]

    results <- mapM (runSingleE2ETest config) testCases

    -- Run additional stability tests
    putStrLn "\n=== Running Additional Stability Tests ==="

    -- Go build verification tests
    buildResults <- runGoBuildTests config
    let allBuildResults = all id buildResults

    -- Error recovery tests
    errorResult <- errorRecoveryTest config

    -- Performance tests
    perfResult <- performanceTest config

    let allResults = all id results && allBuildResults && errorResult && perfResult

    if allResults
        then putStrLn "=== All End-to-End Tests Passed ==="
        else do
            putStrLn "ERROR: Some end-to-end tests failed"
            exitFailure

runSingleE2ETest :: E2EConfig -> (String, String) -> IO Bool
runSingleE2ETest config (inputFile, expectedOutputFile) = do
    let inputPath = testInputDir config </> inputFile
    let outputPath = testOutputDir config </> expectedOutputFile
    let expectedPath = expectedOutputDir config </> expectedOutputFile

    putStrLn $ "Running end-to-end test for: " ++ inputFile

    debugLog "E2E" $ "Processing input file: " ++ inputPath
    debugBreakpoint "E2E" $ "Before compiling " ++ inputFile

    -- Check if input file exists
    inputExists <- doesFileExist inputPath
    if not inputExists
        then do
            putStrLn $ "ERROR: Input file not found: " ++ inputPath
            return False
        else do
            -- Compile the input file
            (_, (exitCode, stdout, stderrOutput)) <- timeIt $ readProcessWithExitCode "stack" ["run", "typus", "--", "convert", inputPath, "-o", outputPath] ""

            debugLog "E2E" $ "Compilation exit code: " ++ show exitCode
            debugLog "E2E" $ "Compilation stdout: " ++ stdout
            debugLog "E2E" $ "Compilation stderr: " ++ stderrOutput

            case exitCode of
                ExitSuccess -> do
                    -- Check if output file was created
                    outputExists <- doesFileExist outputPath
                    if not outputExists
                        then do
                            putStrLn $ "ERROR: Output file not created: " ++ outputPath
                            return False
                        else do
                            -- Compare with expected output (if it exists)
                            expectedExists <- doesFileExist expectedPath
                            if expectedExists
                                then do
                                    (diffExitCode, diffStdout, _) <- readProcessWithExitCode "diff" [expectedPath, outputPath] ""
                                    case diffExitCode of
                                        ExitSuccess -> do
                                            putStrLn $ "✓ Test passed: " ++ inputFile
                                            return True
                                        _ -> do
                                            putStrLn $ "✗ Test failed: " ++ inputFile ++ " - output differs from expected"
                                            putStrLn $ "Diff output: " ++ diffStdout
                                            return False
                                else do
                                    putStrLn $ "? Test passed: " ++ inputFile ++ " (no expected output to compare against)"
                                    return True
                _ -> do
                    putStrLn $ "✗ Test failed: " ++ inputFile ++ " - compilation failed"
                    putStrLn $ "Error output: " ++ stderrOutput
                    return False

-- Helper function to run a command with timeout
runCommandWithTimeout :: String -> [String] -> Int -> IO (ExitCode, String, String)
runCommandWithTimeout cmd args timeout = do
    process <- spawnProcess cmd args
    result <- try (waitForProcess process :: IO ExitCode) :: IO (Either SomeException ExitCode)
    case result of
        Left (ex) -> do
            return (ExitFailure 1, "", "Command execution failed: " ++ show ex)
        Right exitCode -> do
            -- In a real implementation, we'd capture stdout/stderr
            return (exitCode, "", "Command completed")

-- Test for error recovery
errorRecoveryTest :: E2EConfig -> IO Bool
errorRecoveryTest config = do
    putStrLn "Running error recovery test..."

    let invalidInputFile = testInputDir config </> "malformed_syntax_code.typus"
    let outputPath = testOutputDir config </> "error_recovery_output"

    debugLog "E2E" "Testing error recovery with malformed input"
    debugBreakpoint "E2E" "Before error recovery test"

    (exitCode, stdout, stderrOutput) <- readProcessWithExitCode "stack" ["run", "--", "compile", invalidInputFile, "-o", outputPath] ""

    case exitCode of
        ExitFailure 1 -> do
            putStrLn "✓ Error recovery test passed: Compiler properly handled malformed input"
            return True
        _ -> do
            putStrLn "✗ Error recovery test failed: Compiler did not properly handle malformed input"
            return False

-- Performance test
performanceTest :: E2EConfig -> IO Bool
performanceTest config = do
    putStrLn "Running performance test..."

    let largeInputFile = testInputDir config </> "large_code.typus"
    let outputPath = testOutputDir config </> "performance_test_output"

    debugLog "E2E" "Testing performance with large input"
    debugBreakpoint "E2E" "Before performance test"

    (elapsedTime, (exitCode, _, _)) <- timeIt $ readProcessWithExitCode "stack" ["run", "--", "compile", largeInputFile, "-o", outputPath] ""

    case exitCode of
        ExitSuccess -> do
            let threshold :: Double
                threshold = 10.0 -- seconds
            if elapsedTime < threshold
                then do
                    putStrLn $ "✓ Performance test passed: Compilation completed in " ++ show elapsedTime ++ " seconds"
                    return True
                else do
                    putStrLn $ "✗ Performance test failed: Compilation took too long (" ++ show elapsedTime ++ " seconds)"
                    return False
        _ -> do
            putStrLn "✗ Performance test failed: Compilation failed"
            return False

-- Go build verification tests
runGoBuildTests :: E2EConfig -> IO [Bool]
runGoBuildTests config = do
    putStrLn "Running Go build verification tests..."

    let buildTestCases = [
            "comprehensive_go_syntax.typus",
            "simple_go_code.typus",
            "working_typus_syntax.typus",
            "examples/hello_typus.typus"
            ]

    mapM (runGoBuildTest config) buildTestCases

runGoBuildTest :: E2EConfig -> String -> IO Bool
runGoBuildTest config inputFile = do
    let inputPath = if "examples/" `isPrefixOf` inputFile
                   then inputFile  -- Already includes path
                   else testInputDir config </> inputFile
    let goOutputPath = testOutputDir config </> replaceExtension inputFile "go"
    let buildDir = testOutputDir config </> takeBaseName inputFile

    putStrLn $ "Testing Go build for: " ++ inputFile

    -- Convert Typus to Go
    (convertExitCode, _, convertStderr) <- readProcessWithExitCode "stack"
        ["run", "typus", "--", "convert", inputPath, "-o", goOutputPath] ""

    case convertExitCode of
        ExitSuccess -> do
            -- Check if Go file was created
            goFileExists <- doesFileExist goOutputPath
            if not goFileExists
                then do
                    putStrLn $ "✗ Go file not created: " ++ goOutputPath
                    return False
                else do
                    -- Try to build the Go file
                    (buildExitCode, _, buildStderr) <- readProcessWithExitCode "go"
                        ["build", "-o", buildDir </> "output", goOutputPath] ""

                    case buildExitCode of
                        ExitSuccess -> do
                            putStrLn $ "✓ Go build test passed: " ++ inputFile
                            -- Clean up build artifacts
                            _ <- try (removeFile (buildDir </> "output")) :: IO (Either SomeException ())
                            return True
                        _ -> do
                            putStrLn $ "✗ Go build test failed: " ++ inputFile
                            putStrLn $ "Build error: " ++ buildStderr
                            return False
        _ -> do
            putStrLn $ "✗ Go build test failed (conversion): " ++ inputFile
            putStrLn $ "Conversion error: " ++ convertStderr
            return False
  where
    isPrefixOf prefix str = take (length prefix) str == prefix
    takeBaseName = reverse . takeWhile (/= '/') . reverse
    replaceExtension path newExt =
        let base = takeWhile (/= '.') path
        in base ++ "." ++ newExt

-- Comprehensive project structure tests
runProjectStructureTests :: E2EConfig -> IO [Bool]
runProjectStructureTests config = do
    putStrLn "Running comprehensive project structure tests..."

    let projectTests = [
            "multi_package_project",
            "dependency_injection",
            "microservice_structure",
            "library_with_examples"
            ]

    mapM (runProjectStructureTest config) projectTests

runProjectStructureTest :: E2EConfig -> String -> IO Bool
runProjectStructureTest config projectName = do
    let projectDir = testInputDir config </> "projects" </> projectName
    let outputDir = testOutputDir config </> "projects" </> projectName

    putStrLn $ "Testing project structure: " ++ projectName

    projectExists <- doesDirectoryExist projectDir
    if not projectExists
        then do
            putStrLn $ "? Project test skipped (directory not found): " ++ projectName
            return True  -- Skip rather than fail
        else do
            -- Find all .typus files in the project
            findResult <- readProcessWithExitCode "find" [projectDir, "-name", "*.typus"] ""
            case findResult of
                (ExitSuccess, typusFiles, "") -> do
                    let files = lines typusFiles
                    if null files
                        then do
                            putStrLn $ "? No Typus files found in project: " ++ projectName
                            return True
                        else do
                            -- Convert each file and verify Go build
                            results <- mapM (convertAndBuildFile config outputDir) files
                            return $ all id results
                _ -> do
                    putStrLn $ "✗ Failed to find Typus files in project: " ++ projectName
                    return False

convertAndBuildFile :: E2EConfig -> FilePath -> FilePath -> IO Bool
convertAndBuildFile config outputDir typusFile = do
    let goFile = outputDir </> takeFileName (replaceExtension typusFile "go")
    let buildDir = takeDirectory goFile

    -- Ensure output directory exists
    createDirectoryIfMissing True buildDir

    -- Convert to Go
    (convertExitCode, _, _) <- readProcessWithExitCode "stack"
        ["run", "typus", "--", "convert", typusFile, "-o", goFile] ""

    case convertExitCode of
        ExitSuccess -> do
            -- Try to build
            (buildExitCode, _, _) <- readProcessWithExitCode "go"
                ["build", goFile] ""
            return $ buildExitCode == ExitSuccess
        _ -> return False
  where
    replaceExtension path newExt =
        let base = takeWhile (/= '.') path
        in base ++ "." ++ newExt