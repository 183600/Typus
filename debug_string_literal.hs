#!/usr/bin/env runhaskell

import qualified Utils

main :: IO ()
main = do
  -- Test is_complete_string_literal failure case: "a\""
  putStrLn "Testing isCompleteStringLiteral with \"a\\\"\":"
  let input1 = "a\""
  let result1 = Utils.isCompleteStringLiteral input1
  putStrLn $ "Input: " ++ show input1
  putStrLn $ "Output: " ++ show result1
  putStrLn $ "Expected: False (according to test failure)"
  putStrLn ""
  
  -- Test is_problematic_unclosed_string failure case: "a\""
  putStrLn "Testing isProblematicUnclosedString with \"a\\\"\":"
  let input2 = "a\""
  let result2 = Utils.isProblematicUnclosedString input2
  putStrLn $ "Input: " ++ show input2
  putStrLn $ "Output: " ++ show result2
  putStrLn $ "Expected: True (according to test failure)"
  putStrLn ""