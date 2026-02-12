#!/usr/bin/env runhaskell

import Data.List (intercalate)

main :: IO ()
main = do
  -- Test prop_string_lines failure case: "1\n"
  putStrLn "Testing prop_string_lines with \"1\\n\":"
  let s = "1\n"
  let ls = lines s
  let rejoined = intercalate "\n" ls
  putStrLn $ "s: " ++ show s
  putStrLn $ "ls: " ++ show ls
  putStrLn $ "rejoined: " ++ show rejoined
  putStrLn $ "Expected: \"1\" (according to test)"
  putStrLn $ "Test passes: " ++ show (rejoined == "1")
  putStrLn ""