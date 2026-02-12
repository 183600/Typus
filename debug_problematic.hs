#!/usr/bin/env runhaskell

import qualified Utils

main :: IO ()
main = do
  -- Test isProblematicUnclosedString with "\"a\""
  putStrLn "Testing isProblematicUnclosedString with \"\\\"a\\\"\":"
  let input = "\"a\""
  let result = Utils.isProblematicUnclosedString input
  putStrLn $ "Input: " ++ show input
  putStrLn $ "Output: " ++ show result
  putStrLn $ "Expected: True"
  putStrLn ""