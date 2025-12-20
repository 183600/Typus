#!/usr/bin/env stack runghc
{-# LANGUAGE OverloadedStrings #-}

-- Standalone Typus compilation and execution test
-- This executable tests all Typus files by:
-- 1. Finding all .typus files in the project
-- 2. Compiling each .typus file to Go code using the typus compiler
-- 3. Checking if compilation succeeds (fails test if compilation fails)
-- 4. Running each generated Go file with 'go run'
-- 5. Checking if execution succeeds (fails test if execution fails)

module Main (main) where

import System.FilePath.Glob (glob)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath ((</>), takeBaseName, takeExtension)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..), exitFailure, exitSuccess)
import Control.Exception (try, SomeException)
import System.IO.Temp (withSystemTempDirectory)
import Data.List (isPrefixOf, isInfixOf)

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

    -- Test each file
    results <- mapM testTypusFile filteredFiles

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
            putStrLn "Some tests failed!"
            exitFailure
        else do
            putStrLn ""
            putStrLn "All tests passed!"
            exitSuccess

-- Check if a file is a valid Typus file (not in build dirs, hidden dirs, etc.)
isValidTypusFile :: FilePath -> Bool
isValidTypusFile file =
    not (isInBuildDirectory file) &&
    not (isInHiddenDirectory file) &&
    not (isInTempDirectory file) &&
    isTypusFile file
  where
    isInBuildDirectory path = any (`isInfixOf` path) [".stack-work", "dist-newstyle", "build", "target"]
    isInHiddenDirectory path = any (`isInfixOf` path) ["/.", "./", "\\.\\"]
    isInTempDirectory path = any (`isInfixOf` path) ["tmp", "temp", "TMP", "TEMP"]
    isTypusFile path = takeExtension path == ".typus"

-- Test a single Typus file
testTypusFile :: FilePath -> IO Bool
testTypusFile typusFile = do
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

-- Helper function to replace substrings
replaceInString :: String -> String -> String -> String
replaceInString old new = go
  where
    go [] = []
    go str
      | old `isPrefixOf` str = new ++ go (drop (length old) str)
      | otherwise = case str of
                      (x:xs) -> x : go xs