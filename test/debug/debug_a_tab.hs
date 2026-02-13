#!/usr/bin/env runhaskell

import qualified Utils as U
import Data.Char (isPrint, isSpace)

main :: IO ()
main = do
  putStrLn "Testing normalizeIndentation with a\t..."
  let testInput = "a\t"
  putStrLn $ "Input: " ++ show testInput
  putStrLn $ "Length: " ++ show (length testInput)
  putStrLn $ "All isSpace: " ++ show (all isSpace testInput)
  putStrLn $ "Any isPrint: " ++ show (any isPrint testInput)
  putStrLn $ "Any not isPrint: " ++ show (any (not . isPrint) testInput)
  putStrLn $ "Contains tab: " ++ show ('\t' `elem` testInput)
  putStrLn $ "Contains space: " ++ show (' ' `elem` testInput)
  
  let normalized = U.normalizeIndentation testInput
  putStrLn $ "Normalized: " ++ show normalized
  putStrLn $ "Expected: " ++ show "a "
  putStrLn $ "Test " ++ if normalized == "a " then "PASSED" else "FAILED"