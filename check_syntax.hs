#!/usr/bin/env runhaskell
-- Simple syntax checker for our test module

import System.Process (readProcess)
import System.Exit (ExitCode(..))

main :: IO ()
main = do
  putStrLn "Checking syntax of EnhancedCabalQuickCheckTestSuite.hs..."
  
  -- Try to parse the file using GHC's parser only
  result <- readProcess "ghc" ["-fno-code", "-c", "test/Test/Unit/EnhancedCabalQuickCheckTestSuite.hs", "-itest", "-isrc"] ""
  
  putStrLn "GHC Output:"
  putStrLn result
  
  putStrLn "Syntax check completed. If there are no syntax errors above, the module is syntactically correct."