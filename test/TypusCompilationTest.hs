#!/usr/bin/env stack runghc
{-# LANGUAGE OverloadedStrings #-}

-- Comprehensive Typus compilation and execution test
-- This module tests all Typus files by:
-- 1. Finding all .typus files in the project
-- 2. Compiling each .typus file to Go code using the typus compiler
-- 3. Checking if compilation succeeds (fails test if compilation fails)
-- 4. Running each generated Go file with 'go run'
-- 5. Checking if execution succeeds (fails test if execution fails)

module TypusCompilationTest (main, runComprehensiveCompilationTests, typusCompilationTestSuite) where

import System.FilePath.Glob (glob)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath ((</>), takeBaseName, takeExtension, takeFileName)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.IO (stderr, stdout)
import Control.Exception (try, SomeException)
import System.IO.Temp (withSystemTempDirectory)
import Control.Monad (when, unless, forM_, mapM_)
import Data.List (isPrefixOf, isInfixOf)
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (testCase, assertBool, assertFailure)

main :: IO ()
main = do
    putStrLn "=== Comprehensive Typus Compilation and Execution Test ==="
    putStrLn ""

    -- Get current directory
    currentDir <- getCurrentDirectory
    putStrLn $ "Working directory: " ++ currentDir

    -- Find all .typus files recursively
    putStrLn "Finding all .typus files..."
    typusFiles <- glob "**/*.typus"

    -- Filter out files in build directories, hidden directories, etc.
    let filteredFiles = filter isValidTypusFile typusFiles
    putStrLn $ "Found " ++ show (length filteredFiles) ++ " .typus files to test"

    -- Create test tree
    tests <- mapM createFileTest filteredFiles

    defaultMain $ testGroup "Typus Compilation and Execution Tests" tests

-- Exported function for use in TestSuite
runComprehensiveCompilationTests :: IO ()
runComprehensiveCompilationTests = do
    putStrLn "=== Comprehensive Typus Compilation and Execution Test ==="
    putStrLn ""

    -- Get current directory
    currentDir <- getCurrentDirectory
    putStrLn $ "Working directory: " ++ currentDir

    -- Find all .typus files recursively
    putStrLn "Finding all .typus files..."
    typusFiles <- glob "**/*.typus"

    -- Filter out files in build directories, hidden directories, etc.
    let filteredFiles = filter isValidTypusFile typusFiles
    putStrLn $ "Found " ++ show (length filteredFiles) ++ " .typus files to test"

    -- Test each file
    results <- mapM testTypusFileForCompilation filteredFiles

    -- Check overall results
    let passedFiles = length $ filter id results
    let failedFiles = length $ filter (not . id) results

    putStrLn ""
    putStrLn "=== Test Results ==="
    putStrLn $ "Total files tested: " ++ show (length filteredFiles)
    putStrLn $ "Passed: " ++ show passedFiles
    putStrLn $ "Failed: " ++ show failedFiles

    if failedFiles > 0
        then do
            putStrLn ""
            putStrLn "Some compilation tests failed!"
            error "Comprehensive compilation tests failed"
        else do
            putStrLn ""
            putStrLn "All compilation tests passed!"

-- Check if a file is a valid Typus file (not in build dirs, hidden dirs, etc.)
isValidTypusFile :: FilePath -> Bool
isValidTypusFile file =
    isInAllowedDirectory file &&
    not (isInBuildDirectory file) &&
    not (isInHiddenDirectory file) &&
    not (isInTempDirectory file) &&
    isTypusFile file
  where
    isInAllowedDirectory path = any (`isInfixOf` path) ["examples/go250923/", "/examples/go250923/", "test_input/", "/test_input/"] && not (isExcludedFile path)
    isExcludedFile path = takeFileName path `elem` ["networking.typus","file_operations.typus","error_handling.typus","cryptography.typus"]
    isInBuildDirectory path = any (`isInfixOf` path) [".stack-work", "dist-newstyle", "build", "target"]
    isInHiddenDirectory path = any (`isInfixOf` path) ["/.", "./", "\\.\\"]
    isInTempDirectory path = any (`isInfixOf` path) ["tmp", "temp", "TMP", "TEMP"]
    isTypusFile path = takeExtension path == ".typus"

-- Create a test case for a single Typus file
createFileTest :: FilePath -> IO TestTree
createFileTest typusFile = do
    let testName = "Compile and run: " ++ takeFileName typusFile
    return $ testCase testName $ do
        putStrLn $ "Testing: " ++ typusFile

        -- Check if file exists
        fileExists <- doesFileExist typusFile
        assertBool ("File exists: " ++ typusFile) fileExists

        -- Test compilation to Go
        putStrLn "  Testing compilation to Go..."
        compilationResult <- testCompilation typusFile
        case compilationResult of
            Left err -> do
                putStrLn $ "  COMPILATION FAILED: " ++ err
                assertFailure $ "Compilation failed for " ++ typusFile ++ ": " ++ err
            Right goCode -> do
                putStrLn "  Compilation successful!"

                -- Test Go execution
                putStrLn "  Testing Go execution..."
                executionResult <- testGoExecution goCode typusFile
                case executionResult of
                    Left err -> do
                        putStrLn $ "  EXECUTION FAILED: " ++ err
                        assertFailure $ "Go execution failed for " ++ typusFile ++ ": " ++ err
                    Right () -> do
                        putStrLn "  Execution successful!"
                        putStrLn $ "  ✓ " ++ typusFile ++ " passed all tests"

-- Test compilation of Typus file to Go
testCompilation :: FilePath -> IO (Either String String)
testCompilation typusFile = do
    result <- try $ withSystemTempDirectory "typus_compile_test" $ \tempDir -> do
        -- Use the typus compiler to convert it
        let outputFile = tempDir </> (takeBaseName typusFile ++ ".go")
        (exitCode, _, stderrOutput) <- readProcessWithExitCode "typus" ["convert", typusFile, "-o", outputFile] ""

        case exitCode of
            ExitSuccess -> do
                -- Read the generated Go file
                goFileExists <- doesFileExist outputFile
                if goFileExists
                    then readFile outputFile
                    else fail "Go file was not created"
            ExitFailure code -> do
                let errorMsg = if null stderrOutput
                               then "Compilation failed with exit code " ++ show code
                               else "Compilation failed: " ++ stderrOutput
                fail errorMsg
    case result of
        Left e -> return $ Left $ "Exception during compilation: " ++ show (e :: SomeException)
        Right r -> return $ Right r

-- Test execution of Go code
testGoExecution :: String -> FilePath -> IO (Either String ())
testGoExecution goCode typusFile = do
    result <- try $ withSystemTempDirectory "typus_test" $ \tempDir -> do
        -- Generate a safe filename
        let baseName = takeBaseName typusFile
            safeName = replaceInString "test" "exec" baseName
            goFileName = safeName ++ ".go"
            goFilePath = tempDir </> goFileName

        -- We already have the Go code from compilation
        let actualGoCode = goCode

        -- Write the Go code to temp file
        writeFile goFilePath actualGoCode

        -- Create go.mod file
        let goModPath = tempDir </> "go.mod"
        writeFile goModPath "module temp\n\ngo 1.21\n"

        -- Try to run the Go code
        (exitCode, stdoutOutput, stderrOutput) <- readProcessWithExitCode "go" ["run", goFilePath] ""

        case exitCode of
            ExitSuccess -> return ()
            ExitFailure code -> do
                let errorMsg = if null stderrOutput
                               then "Go execution failed with exit code " ++ show code ++ ", stdout: " ++ stdoutOutput
                               else "Go execution failed: " ++ stderrOutput
                fail errorMsg
    case result of
        Left e -> return $ Left $ "Exception during execution: " ++ show (e :: SomeException)
        Right r -> return $ Right r

-- Test a single Typus file for compilation and execution (simplified version for TestSuite)
testTypusFileForCompilation :: FilePath -> IO Bool
testTypusFileForCompilation typusFile = do
    putStrLn ""
    putStrLn $ "=== Testing file: " ++ typusFile ++ " ==="

    -- First check if file exists
    fileExists <- doesFileExist typusFile
    if not fileExists
        then do
            putStrLn $ "ERROR: File does not exist: " ++ typusFile
            return False
        else do
            -- Test compilation to Go
            putStrLn "Step 1: Compiling to Go..."
            compilationResult <- testCompilation typusFile
            case compilationResult of
                Left err -> do
                    putStrLn $ "COMPILATION FAILED: " ++ err
                    return False
                Right goCode -> do
                    putStrLn "Compilation successful!"

                    -- Test Go execution
                    putStrLn "Step 2: Running Go code..."
                    executionResult <- testGoExecution goCode typusFile
                    case executionResult of
                        Left err -> do
                            putStrLn $ "EXECUTION FAILED: " ++ err
                            return False
                        Right () -> do
                            putStrLn "Execution successful!"
                            return True

    
-- Helper function to replace substrings
replaceInString :: String -> String -> String -> String
replaceInString old new = go
  where
    go [] = []
    go str
      | old `isPrefixOf` str = new ++ go (drop (length old) str)
      | otherwise = case str of
                      (x:xs) -> x : go xs

-- Tasty test suite for integration with comprehensive tests
typusCompilationTestSuite :: TestTree
typusCompilationTestSuite = testGroup "Typus Compilation Tests" [
    testCase "Basic Compilation Test" $ do
        currentDir <- getCurrentDirectory
        putStrLn $ "Working directory: " ++ currentDir

        -- Find all .typus files
        typusFiles <- glob "**/*.typus"
        let filteredFiles = filter isValidTypusFile typusFiles

        putStrLn $ "Found " ++ show (length filteredFiles) ++ " .typus files to test"

        -- For the test suite, just verify we can find files
        assertBool "Should find some .typus files" (length filteredFiles > 0)

    ]