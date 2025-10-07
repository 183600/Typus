{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.Exit (ExitCode(..), exitFailure, exitSuccess)
import System.Directory (doesFileExist, getCurrentDirectory, createDirectoryIfMissing)
import System.FilePath ((</>), takeBaseName, takeExtension)
import System.Process (readProcessWithExitCode)
import Control.Exception (try, SomeException, catch)
import Control.Monad (unless)
import System.IO.Temp (withSystemTempDirectory)
import System.IO (hPutStrLn, stdout, stderr)
import Control.Monad (when, forM_, filterM)
import Data.List (isInfixOf, isSuffixOf, isPrefixOf)
import System.FilePath.Glob (glob)

main :: IO ()
main = do
    putStrLn "=== Stack Typus Compiler Test ==="
    putStrLn ""

    -- Get current directory
    currentDir <- getCurrentDirectory
    putStrLn $ "Working directory: " ++ currentDir

    -- Find all .typus files in the project
    putStrLn "Finding all .typus files..."
    typusFiles <- glob "../**/*.typus"

    -- Filter out files in build directories and hidden directories
    let filteredFiles = filter isValidTypusFile typusFiles
    putStrLn $ "Found " ++ show (length filteredFiles) ++ " .typus files to test"

    -- Test each file
    results <- mapM testTypusFile filteredFiles

    -- Report results
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

-- Check if a file is valid for testing (not in build dirs, etc.)
isValidTypusFile :: FilePath -> Bool
isValidTypusFile file =
    not (isInBuildDirectory file) &&
    not (isInHiddenDirectory file) &&
    not (isInTempDirectory file)
  where
    isInBuildDirectory path = any (`isInfixOf` path) [".stack-work", "dist-newstyle", "build", "target"]
    isInHiddenDirectory path = any (`isInfixOf` path) ["/.", "./", "\\.\\", ".git", ".hpc"]
    isInTempDirectory path = any (`isInfixOf` path) ["tmp", "temp", "TMP", "TEMP"]

-- Test a single Typus file
testTypusFile :: FilePath -> IO Bool
testTypusFile typusFile = do
    putStrLn ""
    putStrLn $ "=== Testing: " ++ typusFile ++ " ==="

    -- Check if file exists
    fileExists <- doesFileExist typusFile
    unless fileExists $ do
        putStrLn "ERROR: File does not exist"
        return False

    -- Step 1: Test compilation to Go
    compilationResult <- testCompilation typusFile
    case compilationResult of
        Left err -> do
            putStrLn $ "❌ COMPILATION FAILED: " ++ err
            return False
        Right goCode -> do
            putStrLn "✅ Compilation successful!"

            -- Step 2: Test Go execution
            executionResult <- testGoExecution goCode typusFile
            case executionResult of
                Left err -> do
                    putStrLn $ "❌ EXECUTION FAILED: " ++ err
                    return False
                Right () -> do
                    putStrLn "✅ Execution successful!"
                    return True

-- Test compilation of Typus file to Go
testCompilation :: FilePath -> IO (Either String String)
testCompilation typusFile = try $ do
    -- Use the typus compiler to convert the file
    (exitCode, stdout, stderr) <- readProcessWithExitCode "typus" ["convert", typusFile, "-"] ""

    case exitCode of
        ExitSuccess -> do
            -- Check if there's any output
            if null stdout && null stderr
                then return ""  -- Some compilers might not output to stdout
                else return $ if null stdout then stderr else stdout
        ExitFailure code -> do
            let errorMsg = if null stderr
                           then "Compilation failed with exit code " ++ show code
                           else "Compilation failed: " ++ stderr
            Left errorMsg
    `catch` (\(e :: SomeException) -> return $ Left $ "Exception during compilation: " ++ show e)

-- Test execution of Go code
testGoExecution :: String -> FilePath -> IO (Either String ())
testGoExecution goCode typusFile = try $
    withSystemTempDirectory "typus_test" $ \tempDir -> do
        -- Generate a safe filename
        let baseName = takeBaseName typusFile
            safeName = replaceString "test" "exec" baseName
            goFileName = safeName ++ ".go"
            goFilePath = tempDir </> goFileName

        -- If goCode is empty, try to convert to file directly
        actualGoCode <- if null goCode
            then do
                let tempOutput = tempDir </> "output.go"
                (exitCode, _, err) <- readProcessWithExitCode "typus" ["convert", typusFile, tempOutput] ""
                case exitCode of
                    ExitSuccess -> readFile tempOutput
                    ExitFailure _ -> fail $ "Failed to convert to file: " ++ err
            else return goCode

        -- Write the Go code to file
        writeFile goFilePath actualGoCode

        -- Create go.mod file for proper module support
        let goModPath = tempDir </> "go.mod"
        writeFile goModPath "module temp\n\ngo 1.21\n"

        -- Try to run the Go code
        (exitCode, stdout, stderr) <- readProcessWithExitCode "go" ["run", goFilePath] ""

        case exitCode of
            ExitSuccess -> return ()
            ExitFailure code -> do
                let errorMsg = if null stderr
                               then "Go execution failed with exit code " ++ show code ++ "\nstdout: " ++ stdout
                               else "Go execution failed: " ++ stderr
                fail errorMsg
    `catch` (\(e :: SomeException) -> return $ Left $ "Exception during execution: " ++ show e)

-- Helper function to replace substrings
replaceString :: String -> String -> String -> String
replaceString old new = go
  where
    go [] = []
    go str
      | old `isPrefixOf` str = new ++ go (drop (length old) str)
      | (x:xs) <- str = x : go xs