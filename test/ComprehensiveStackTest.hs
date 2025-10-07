{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.Exit (ExitCode(..), exitFailure, exitSuccess)
import System.Process (readProcessWithExitCode)
import System.IO (hPutStrLn, stdout)
import Control.Exception (try, SomeException)
import Control.Monad (unless)

main :: IO ()
main = do
    putStrLn "=== Stack Comprehensive Compiler Test ==="
    putStrLn ""

    -- Run shell script
    result <- try (do
        (exitCode, stdout, stderr) <- readProcessWithExitCode "./test_typus_comprehensive.sh" [] ""

        putStrLn stdout
        unless (null stderr) $ hPutStrLn stdout stderr

        return exitCode
    ) `catch` (\(e :: SomeException) -> do
        putStrLn $ "ERROR: Exception running test script: " ++ show e
        return $ ExitFailure 1
    )

    case result of
        ExitSuccess -> do
            putStrLn ""
            putStrLn "✅ All comprehensive tests passed!"
            exitSuccess
        ExitFailure code -> do
            putStrLn ""
            putStrLn $ "❌ Comprehensive tests failed with exit code: " ++ show code
            exitFailure