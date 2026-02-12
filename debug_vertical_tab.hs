#!/usr/bin/env runhaskell

import qualified Utils as U

main :: IO ()
main = do
  putStrLn "Testing normalizeIndentation with vertical tab..."
  let testInput = "\v"
  let mixed = "\t  \t  " ++ testInput ++ "  \t  "
  putStrLn $ "Input: " ++ show testInput
  putStrLn $ "Mixed: " ++ show mixed
  let normalized = U.normalizeIndentation mixed
  putStrLn $ "Normalized: " ++ show normalized
  putStrLn $ "Expected: " ++ show mixed
  putStrLn $ "Test " ++ if normalized == mixed then "PASSED" else "FAILED"