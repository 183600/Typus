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
  putStrLn $ "last rest == '\"': " ++ show (last rest == '"')
  if length rest >= 2
    then putStrLn $ "rest !! (length rest - 2) == '\\': " ++ show (rest !! (length rest - 2) == '\\')
    else putStrLn $ "rest !! (length rest - 2) == '\\': N/A (length rest < 2)"
  putStrLn $ "input == \"a\\\"\": " ++ show (input == "a\"")
  putStrLn ""