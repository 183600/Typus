#!/usr/bin/env runhaskell

import qualified Utils

main :: IO ()
main = do
  -- Test prop_normalize_indentation_mixed failure case: "\n"
  putStrLn "Testing prop_normalize_indentation_mixed with newline:"
  let s = "\n"
  let mixed = "\t  \t  " ++ s ++ "  \t  "
  let normalized = Utils.normalizeIndentation mixed
  putStrLn $ "s: " ++ show s
  putStrLn $ "mixed: " ++ show mixed
  putStrLn $ "normalized: " ++ show normalized
  putStrLn $ "normalized == mixed: " ++ show (normalized == mixed)
  putStrLn $ "Test passes: " ++ show (normalized == mixed)
  putStrLn ""