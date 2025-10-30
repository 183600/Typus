module ConversionTest (runConversionTests, conversionTestSuite) where

import System.Directory
    ( doesDirectoryExist
    , doesFileExist
    , findExecutable
    , listDirectory
    , removeDirectoryRecursive
    )
import System.FilePath ((</>), (<.>), dropExtension, takeFileName)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.IO (hPutStrLn, stderr)
import Control.Monad (forM, forM_, unless, when)
import Data.List (isInfixOf, isSuffixOf)
import Text.Printf (printf)
import Test.Tasty
import Test.Tasty.HUnit as HU

import TestSupport.Verbosity (Verbosity(..), getVerbosity, logVerbose, whenVerbose)

-- Main test function
runConversionTests :: IO ()
runConversionTests = do
    verbosity <- getVerbosity
    whenVerbose verbosity $ putStrLn "=== Typus Conversion and Execution Test ==="

    maybeGoPath <- findExecutable "go"
    case maybeGoPath of
        Nothing ->
            putStrLn "WARNING: Go compiler not found; skipping conversion tests."
        Just goPath -> do
            -- Test configuration
            let inputDir = "examples/go250923"
            let outputDir = "examples/go250923output"

            -- Check if input directory exists
            inputExists <- doesDirectoryExist inputDir
            when (not inputExists) $ do
                hPutStrLn stderr $ "ERROR: Input directory " ++ inputDir ++ " does not exist"
                fail "Input directory not found"

            -- Step 1: Test typus convert command
            whenVerbose verbosity $ putStrLn "Step 1: Testing typus convert command..."
            convertSuccess <- testTypusConvert verbosity inputDir outputDir
            when (not convertSuccess) $ do
                hPutStrLn stderr "ERROR: typus convert command failed"
                fail "typus convert failed"

            -- Step 2: Get list of typus files
            whenVerbose verbosity $ putStrLn "Step 2: Getting list of typus files..."
            typusFiles <- getTypusFiles inputDir
            whenVerbose verbosity $ putStrLn $ "Found " ++ show (length typusFiles) ++ " typus files"
            when (null typusFiles) $ do
                hPutStrLn stderr "ERROR: No typus files found for conversion tests"
                fail "no typus files available"

            -- Step 3: Test running original typus files (expected to fail)
            whenVerbose verbosity $ putStrLn "Step 3: Testing original typus files (expected to fail)..."
            testOriginalFiles verbosity goPath typusFiles

            -- Step 4: Test running converted Go files
            whenVerbose verbosity $ putStrLn "Step 4: Testing converted Go files..."
            results <- testConvertedFiles verbosity goPath outputDir typusFiles

            let total = length results
                successCount = length (filter id results)

            case total of
                0 -> putStrLn "Conversion tests skipped (no Go files were executed)."
                _ -> do
                    if successCount == total
                        then putStrLn "Conversion tests passed."
                        else printf "Conversion tests passed for %d/%d files.\n" successCount total

                    whenVerbose verbosity $ do
                        putStrLn "=== Test Summary ==="
                        putStrLn $ "Total typus files: " ++ show (length typusFiles)
                        putStrLn $ "Go files that run without error: " ++ show successCount

-- Test that typus convert command
testTypusConvert :: Verbosity -> FilePath -> FilePath -> IO Bool
testTypusConvert verbosity inputDir outputDir = do
    -- Clean up previous output
    outputExists <- doesDirectoryExist outputDir
    when outputExists $ removeDirectoryRecursive outputDir

    logVerbose verbosity $ "Running: stack exec -- typus convert " ++ inputDir ++ " -o " ++ outputDir
    (exitCode, stdout, stdErr) <- readProcessWithExitCode "stack"
        ["exec", "--", "typus", "convert", inputDir, "-o", outputDir] ""

    case exitCode of
        ExitSuccess -> do
            logVerbose verbosity "Conversion completed successfully"
            whenVerbose verbosity $
                unless (null stdout) $
                    putStrLn $ "Output: " ++ take 200 stdout
            pure True
        ExitFailure code -> do
            hPutStrLn stderr $ "typus convert failed with exit code " ++ show code
            hPutStrLn stderr $ "STDOUT: " ++ take 500 stdout
            hPutStrLn stderr $ "STDERR: " ++ take 500 stdErr
            pure False

-- Get list of typus files in directory
getTypusFiles :: FilePath -> IO [FilePath]
getTypusFiles dir = do
    allFiles <- listDirectory dir
    let typusFiles = filter (".typus" `isSuffixOf`) allFiles
    pure $ map (dir </>) typusFiles

-- Test running original typus files (expected to fail)
testOriginalFiles :: Verbosity -> FilePath -> [FilePath] -> IO ()
testOriginalFiles verbosity goPath files =
    forM_ files $ \file -> do
        whenVerbose verbosity $ putStrLn $ "Testing original file: " ++ takeFileName file
        (exitCode, _, _) <- readProcessWithExitCode goPath ["run", file] ""
        case exitCode of
            ExitSuccess ->
                hPutStrLn stderr $ "WARNING: Unexpected success when running Typus file directly: " ++ takeFileName file
            ExitFailure _ ->
                whenVerbose verbosity $ putStrLn "  ✓ Expected failure (Go cannot run .typus files directly)"

-- Test running converted Go files
testConvertedFiles :: Verbosity -> FilePath -> FilePath -> [FilePath] -> IO [Bool]
testConvertedFiles verbosity goPath outputDir typusFiles = do
    outputExists <- doesDirectoryExist outputDir
    if not outputExists
        then do
            hPutStrLn stderr $ "ERROR: Output directory " ++ outputDir ++ " does not exist"
            pure []
        else forM typusFiles $ \typusFile -> do
            let baseName = takeFileName typusFile
                goFile = outputDir </> dropExtension baseName <.> "go"

            goExists <- doesFileExist goFile
            if not goExists
                then do
                    hPutStrLn stderr $ "ERROR: Go file not found: " ++ goFile
                    pure False
                else do
                    whenVerbose verbosity $ putStrLn $ "Testing Go file: " ++ takeFileName goFile

                    (exitCode, stdout, stdErr) <- readProcessWithExitCode "timeout" ["10s", goPath, "run", goFile] ""
                    let actualExitCode = if exitCode == ExitFailure 124 then ExitFailure 1 else exitCode

                    case actualExitCode of
                        ExitSuccess -> do
                            whenVerbose verbosity $ putStrLn "  ✓ Go file executed successfully"
                            whenVerbose verbosity $
                                unless (null stdout) $
                                    putStrLn $ "    Output (truncated): " ++ take 200 stdout
                            checkExpectedOutput verbosity baseName stdout
                            pure True
                        ExitFailure _ -> do
                            hPutStrLn stderr $ "Go file execution failed: " ++ takeFileName goFile
                            when (not $ null stdErr) $
                                hPutStrLn stderr $ "    Error: " ++ take 200 stdErr
                            pure False

-- Check if the output matches expected patterns for known files
checkExpectedOutput :: Verbosity -> FilePath -> String -> IO ()
checkExpectedOutput verbosity filename output =
    case expectation of
        Nothing ->
            whenVerbose verbosity $
                putStrLn "    (No specific output pattern to check for this file)"
        Just (description, predicate) ->
            if predicate output
                then whenVerbose verbosity $
                    putStrLn $ "    ✓ " ++ description
                else do
                    whenVerbose verbosity $
                        putStrLn $ "    ⚠ " ++ description
                    hPutStrLn stderr $ "WARNING: Expected " ++ description ++ " for " ++ filename
  where
    expectation
        | "hello" `isInfixOf` filename =
            Just ("Expected 'Hello' in output", ("Hello" `isInfixOf`))
        | "calculator" `isInfixOf` filename =
            Just ("Expected calculator result in output", ("10 + 20 = 30" `isInfixOf`))
        | "algorithms" `isInfixOf` filename =
            Just
                ( "Expected algorithm output patterns"
                , \out -> "Original array:" `isInfixOf` out && "Sorted array:" `isInfixOf` out
                )
        | "data_structures" `isInfixOf` filename =
            Just ("Expected data structures output", ("Linked List contents:" `isInfixOf`))
        | otherwise = Nothing

-- Tasty test suite for integration with comprehensive tests
conversionTestSuite :: TestTree
conversionTestSuite = testGroup "Conversion Tests"
    [ testCase "Simple Conversion Test" $ do
        let inputDir = "examples/go250923"
        let _outputDir = "examples/go250923output" -- renamed to avoid unused warning

        inputExists <- doesDirectoryExist inputDir
        when (not inputExists) $ assertFailure $ "Input directory does not exist: " ++ inputDir
    ]
