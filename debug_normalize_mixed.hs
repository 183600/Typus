#!/usr/bin/env runhaskell

import qualified Utils

main :: IO ()
main = do
-- Test prop_normalize_indentation_mixed failure case: "\n"
  putStrLn "Testing prop_normalize_indentation_mixed with \"\\n\":"
  let input = "\n"
  let result = Utils.normalizeIndentation input
  let resultLines = lines result
  putStrLn $ "Input: " ++ show input
  putStrLn $ "Result: " ++ show result
  putStrLn $ "Result lines: " ++ show resultLines
  putStrLn $ "Length of result lines: " ++ show (length resultLines)
  putStrLn $ "Expected length: 1"
  putStrLn $ "Test passes: " ++ show (length resultLines == 1)
  putStrLn ""