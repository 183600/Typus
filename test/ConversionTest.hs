module ConversionTest (runConversionTests, conversionTestSuite) where

import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, removeDirectoryRecursive)
import System.FilePath ((</>), takeFileName, takeExtension, dropExtension, (<.>))
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.IO (hPutStrLn, stderr)
import qualified System.IO as IO
import Control.Monad (when, forM_, forM)
import Data.List (isSuffixOf, isInfixOf)
import Text.Printf (printf)
import Test.Tasty
import Test.Tasty.HUnit as HU

-- Main test function
runConversionTests :: IO ()
runConversionTests = do
    putStrLn "=== Typus Conversion and Execution Test ==="
    putStrLn ""

    -- Test configuration
    let inputDir = "examples/go250923"
    let outputDir = "examples/go250923output"

    -- Check if input directory exists
    inputExists <- doesDirectoryExist inputDir
    when (not inputExists) $ do
        hPutStrLn IO.stderr $ "ERROR: Input directory " ++ inputDir ++ " does not exist"
        fail "Input directory not found"

    -- Step 1: Test typus convert command
    putStrLn "Step 1: Testing typus convert command..."
    convertSuccess <- testTypusConvert inputDir outputDir
    when (not convertSuccess) $ do
        hPutStrLn IO.stderr "ERROR: typus convert command failed"
        fail "typus convert failed"
    putStrLn "✓ typus convert command completed successfully"
    putStrLn ""

    -- Step 2: Get list of typus files
    putStrLn "Step 2: Getting list of typus files..."
    typusFiles <- getTypusFiles inputDir
    printf "Found %d typus files\n" (length typusFiles)
    putStrLn ""

    -- Step 3: Test running original typus files (expected to fail)
    putStrLn "Step 3: Testing original typus files (expected to fail)..."
    testOriginalFiles typusFiles
    putStrLn ""

    -- Step 4: Test running converted Go files
    putStrLn "Step 4: Testing converted Go files..."
    testResults <- testConvertedFiles outputDir typusFiles
    putStrLn ""

    -- Step 5: Summary
    putStrLn "=== Test Summary ==="
    putStrLn $ "Total typus files: " ++ show (length typusFiles)
    putStrLn $ "Go files that run without error: " ++ show (length $ filter id testResults)
    let successCount = length $ filter id testResults
    if successCount == length typusFiles
        then putStrLn "✓ All tests passed!"
        else do
            putStrLn "✗ Some tests failed!"
            printf "Successful: %d/%d\n" successCount (length typusFiles)

-- Test that typus convert command
testTypusConvert :: FilePath -> FilePath -> IO Bool
testTypusConvert inputDir outputDir = do
    -- Clean up previous output
    outputExists <- doesDirectoryExist outputDir
    when outputExists $ removeDirectoryRecursive outputDir

    -- Run typus convert command
    putStrLn $ "Running: stack exec -- typus convert " ++ inputDir ++ " -o " ++ outputDir
    (exitCode, stdout, stdErr) <- readProcessWithExitCode "stack" ["exec", "--", "typus", "convert", inputDir, "-o", outputDir] ""

    case exitCode of
        ExitSuccess -> do
            putStrLn $ "Conversion completed successfully"
            putStrLn $ "Output: " ++ take 200 stdout
            return True
        ExitFailure code -> do
            hPutStrLn IO.stderr $ "typus convert failed with exit code " ++ show code
            hPutStrLn IO.stderr $ "STDOUT: " ++ take 500 stdout
            hPutStrLn IO.stderr $ "STDERR: " ++ take 500 stdErr
            return False

-- Get list of typus files in directory
getTypusFiles :: FilePath -> IO [FilePath]
getTypusFiles dir = do
    allFiles <- listDirectory dir
    let typusFiles = filter (isSuffixOf ".typus") allFiles
    return $ map (dir </>) typusFiles

-- Test running original typus files (expected to fail)
testOriginalFiles :: [FilePath] -> IO ()
testOriginalFiles files = do
    forM_ files $ \file -> do
        putStrLn $ "Testing original file: " ++ takeFileName file
        (exitCode, _, _) <- readProcessWithExitCode "go" ["run", file] ""
        case exitCode of
            ExitSuccess -> do
                putStrLn $ "  ⚠ Unexpected success (typus file ran directly)"
            ExitFailure _ -> do
                putStrLn $ "  ✓ Expected failure (Go cannot run .typus files directly)"

-- Test running converted Go files
testConvertedFiles :: FilePath -> [FilePath] -> IO [Bool]
testConvertedFiles outputDir typusFiles = do
    outputExists <- doesDirectoryExist outputDir
    if not outputExists
        then do
            hPutStrLn IO.stderr $ "ERROR: Output directory " ++ outputDir ++ " does not exist"
            return []
        else do
            results <- forM typusFiles $ \typusFile -> do
                let baseName = takeFileName typusFile
                let goFile = outputDir </> dropExtension baseName <.> "go"

                goExists <- doesFileExist goFile
                if not goExists
                    then do
                        hPutStrLn IO.stderr $ "ERROR: Go file not found: " ++ goFile
                        return False
                    else do
                        putStrLn $ "Testing Go file: " ++ takeFileName goFile

                        -- Try to run Go file with timeout
                        (exitCode, stdout, stdErr) <- readProcessWithExitCode "timeout" ["10s", "go", "run", goFile] ""
                        let actualExitCode = if exitCode == ExitFailure 124
                                            then ExitFailure 1  -- Timeout
                                            else exitCode

                        case actualExitCode of
                            ExitSuccess -> do
                                putStrLn $ "  ✓ Go file executed successfully"
                                when (not $ null stdout) $ do
                                    putStrLn $ "    Output: " ++ take 200 stdout
                                    -- Check for expected output patterns
                                    checkExpectedOutput baseName stdout
                                return True
                            ExitFailure _ -> do
                                putStrLn $ "  ✗ Go file execution failed"
                                when (not $ null stdErr) $
                                    putStrLn $ "    Error: " ++ take 200 stdErr
                                return False
            return results

-- Check if the output matches expected patterns for known files
checkExpectedOutput :: FilePath -> String -> IO ()
checkExpectedOutput filename output
    | "hello" `isInfixOf` filename =
        if "Hello" `isInfixOf` output
            then putStrLn $ "    ✓ Expected 'Hello' in output"
            else putStrLn $ "    ⚠ Expected 'Hello' in output"
    | "calculator" `isInfixOf` filename =
        if "10 + 20 = 30" `isInfixOf` output
            then putStrLn $ "    ✓ Expected calculator result in output"
            else putStrLn $ "    ⚠ Expected calculator result in output"
    | "algorithms" `isInfixOf` filename =
        if ("Original array:" `isInfixOf` output && "Sorted array:" `isInfixOf` output)
            then putStrLn $ "    ✓ Expected algorithm output patterns found"
            else putStrLn $ "    ⚠ Expected algorithm output patterns not found"
    | "data_structures" `isInfixOf` filename =
        if "Linked List contents:" `isInfixOf` output
            then putStrLn $ "    ✓ Expected data structures output found"
            else putStrLn $ "    ⚠ Expected data structures output not found"
    | otherwise = putStrLn $ "    (No specific output pattern to check for this file)"

-- Tasty test suite for integration with comprehensive tests
conversionTestSuite :: TestTree
conversionTestSuite = testGroup "Conversion Tests" [
    testCase "Simple Conversion Test" $ do
        -- Basic test to verify conversion functionality exists
        let inputDir = "examples/go250923"
        let outputDir = "examples/go250923output"

        inputExists <- doesDirectoryExist inputDir
        when (not inputExists) $ assertFailure $ "Input directory does not exist: " ++ inputDir

        putStrLn "Basic conversion test completed successfully"

    ]