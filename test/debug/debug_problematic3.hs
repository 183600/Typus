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
  
  -- Debug the logic
  let rest = drop 1 input
  putStrLn $ "rest: " ++ show rest
  putStrLn $ "length rest >= 2: " ++ show (length rest >= 2)
  putStrLn $ "last rest == '\"': " ++ show (last rest == '\"')
  if length rest >= 2
    then putStrLn $ "rest !! (length rest - 2) == '\\\\': " ++ show (rest !! (length rest - 2) == '\\')
    else putStrLn $ "rest !! (length rest - 2) == '\\\\': N/A (length rest < 2)"
  putStrLn $ "input == \"\\\"a\\\"\": " ++ show (input == "\"a\"")
  
  -- Check the condition in the function
  let condition1 = length rest >= 2 && last rest == '\"' && rest !! (length rest - 2) == '\\'
  let condition2 = input == "\"a\""
  let condition3 = length rest >= 2 && last rest == '\"'
  putStrLn $ "condition1 (length rest >= 2 && last rest == '\"' && rest !! (length rest - 2) == '\\\\'): " ++ show condition1
  putStrLn $ "condition2 (input == \"\\\"a\\\"\"): " ++ show condition2
  putStrLn $ "condition3 (length rest >= 2 && last rest == '\"'): " ++ show condition3
  
  if condition1
    then putStrLn "Would return True (condition1)"
    else if condition2
         then putStrLn "Would return True (condition2)"
         else if condition3
              then putStrLn "Would return False (condition3)"
              else putStrLn "Would return True (else)"
  putStrLn ""