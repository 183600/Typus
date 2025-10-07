#!/usr/bin/env stack runghc
{-# LANGUAGE OverloadedStrings #-}

-- Comprehensive test script that compiles all Typus files to Go and tests the output
-- This script will:
-- 1. Find all .typus files in the project
-- 2. Compile each .typus file to Go code
-- 3. Check if compilation succeeds (if not, test fails)
-- 4. Run each generated Go file with 'go run'
-- 5. Check if execution succeeds (if not, test fails)

module Main where

import System.FilePath.Glob (glob)
import System.Directory (doesFileExist, doesDirectoryExist, createDirectoryIfMissing, getCurrentDirectory)
import System.FilePath ((</>), takeBaseName, takeExtension)
import System.Process (readProcessWithExitCode, CreateProcess(..), StdStream(..))
import System.Exit (ExitCode(..), exitFailure, exitSuccess)
import System.IO (hPutStr, hPutStrLn, stderr, stdout)
import Control.Exception (try, SomeException, bracket)
import System.IO.Temp (withSystemTempDirectory)
import System.IO (hClose, openTempFile)
import Control.Monad (when, unless, forM_, filterM)
import Data.List (isSuffixOf, isPrefixOf)
import Data.Maybe (isJust, fromJust)

main :: IO ()
main = do
    putStrLn "=== Comprehensive Typus Compilation Test ==="
    putStrLn ""

    -- Get current directory
    currentDir <- getCurrentDirectory
    putStrLn $ "Working directory: " ++ currentDir

    -- Find all .typus files
    putStrLn "Finding all .typus files..."
    typusFiles <- glob "../**/*.typus"
    putStrLn $ "Found " ++ show (length typusFiles) ++ " .typus files"

    -- Filter out any files that are in hidden directories or build directories
    let filteredFiles = filter isValidTypusFile typusFiles
    putStrLn $ "After filtering: " ++ show (length filteredFiles) ++ " files to test"

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
    not (isInTempDirectory file)
  where
    isInBuildDirectory path = any (`isInfixOf` path) [".stack-work", "dist-newstyle", "build", "target"]
    isInHiddenDirectory path = any (`isInfixOf` path) ["/.", "./", "\\.\\"]  -- Hidden directories
    isInTempDirectory path = any (`isInfixOf` path) ["tmp", "temp", "TMP", "TEMP"]

-- Test a single Typus file
testTypusFile :: FilePath -> IO Bool
testTypusFile typusFile = do
    putStrLn ""
    putStrLn $ "=== Testing file: " ++ typusFile ++ " ==="

    -- First check if file exists
    fileExists <- doesFileExist typusFile
    unless fileExists $ do
        putStrLn $ "ERROR: File does not exist: " ++ typusFile
        return False

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
    try $ do
        -- Read the Typus file
        content <- readFile typusFile

        -- Use the typus compiler to convert it
        (exitCode, _, err) <- readProcessWithExitCode "typus" ["convert", typusFile, "/dev/stdout"] ""

        case exitCode of
            ExitSuccess -> do
                -- For some reason, the output might be in stderr, so check both
                if null err
                    then return ""  -- typus command doesn't output to stdout as expected
                    else return err
            ExitFailure code -> do
                let errorMsg = if null err
                               then "Compilation failed with exit code " ++ show code
                               else "Compilation failed: " ++ err
                fail errorMsg
    `catch` (\(e :: SomeException) -> return $ Left $ "Exception during compilation: " ++ show e)

-- Test execution of Go code
testGoExecution :: String -> FilePath -> IO (Either String ())
testGoExecution goCode typusFile = do
    try $ withSystemTempDirectory "typus_test" $ \tempDir -> do
        -- Generate a safe filename (avoid "test" in filename to prevent Go build issues)
        let baseName = takeBaseName typusFile
            safeName = replaceInString "test" "exec" baseName
            goFileName = safeName ++ ".go"
            goFilePath = tempDir </> goFileName

        -- Write the Go code to temp file
        -- If goCode is empty (typus doesn't output to stdout), we need to run the convert command again
        actualGoCode <- if null goCode
            then do
                -- Run typus convert to output to file
                let tempOutput = tempDir </> "output.go"
                (exitCode, _, err) <- readProcessWithExitCode "typus" ["convert", typusFile, tempOutput] ""
                case exitCode of
                    ExitSuccess -> readFile tempOutput
                    ExitFailure _ -> fail $ "Failed to convert file: " ++ err
            else return goCode

        writeFile goFilePath actualGoCode

        -- Create go.mod file
        let goModPath = tempDir </> "go.mod"
        writeFile goModPath "module temp\n\ngo 1.21\n"

        -- Try to run the Go code
        (exitCode, stdout, stderr) <- readProcessWithExitCode "go" ["run", goFilePath] ""

        case exitCode of
            ExitSuccess -> return ()
            ExitFailure code -> do
                let errorMsg = if null stderr
                               then "Go execution failed with exit code " ++ show code ++ ", stdout: " ++ stdout
                               else "Go execution failed: " ++ stderr
                fail errorMsg
    `catch` (\(e :: SomeException) -> return $ Left $ "Exception during execution: " ++ show e)

-- Helper function to replace substrings
replaceInString :: String -> String -> String -> String
replaceInString old new = go
  where
    go [] = []
    go str
      | old `isPrefixOf` str = new ++ go (drop (length old) str)
      | (x:xs) <- str = x : go xs