#!/usr/bin/env runhaskell

import qualified Utils as U
import Data.Char (isPrint)

main :: IO ()
main = do
  putStrLn "Testing normalizeIndentation with newline..."
  let testInput = "\n"
  let mixed = "\t  \t  " ++ testInput ++ "  \t  "
  putStrLn $ "Input: " ++ show testInput
  putStrLn $ "Mixed: " ++ show mixed
  let normalized = U.normalizeIndentation mixed
  putStrLn $ "Normalized: " ++ show normalized
  putStrLn $ "Expected: " ++ show mixed
  putStrLn $ "Test " ++ if normalized == mixed then "PASSED" else "FAILED"
  
  putStrLn "\nTesting normalizeIndentation relative with a\t..."
  let testInput2 = "a\t"
  putStrLn $ "Input: " ++ show testInput2
  let normalized2 = U.normalizeIndentation testInput2
  putStrLn $ "Normalized: " ++ show normalized2
  putStrLn $ "Expected: " ++ show "a "
  putStrLn $ "Test " ++ if normalized2 == "a " then "PASSED" else "FAILED"