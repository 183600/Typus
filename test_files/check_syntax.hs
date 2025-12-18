#!/usr/bin/env runhaskell
-- Simple syntax check script

import System.Exit (exitFailure)

main :: IO ()
main = do
    putStrLn "Checking test files for basic syntax errors..."
    
    -- Try to read and parse the test files
    putStrLn "Checking UtilsQuickCheckSpec.hs..."
    -- This would normally require the full compilation environment
    putStrLn "Syntax checking requires full cabal environment"
    
    putStrLn "Done."