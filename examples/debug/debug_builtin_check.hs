#!/usr/bin/env stack
-- stack --resolver lts-20.0 script

{-# LANGUAGE OverloadedStrings #-}

import Ownership
import System.IO

main :: IO ()
main = do
  putStrLn "Testing built-in functions list:"
  
  -- Test if int and fmt are in the built-in list
  putStrLn $ "int in builtInFunctions: " ++ show ("int" `elem` builtInFunctions)
  putStrLn $ "fmt in builtInFunctions: " ++ show ("fmt" `elem` builtInFunctions)
  putStrLn $ "Println in builtInFunctions: " ++ show ("Println" `elem` builtInFunctions)
  putStrLn $ "fmt.Println in builtInFunctions: " ++ show ("fmt.Println" `elem` builtInFunctions)
  
  putStrLn $ "\nFirst 10 built-in functions: " ++ show (take 10 builtInFunctions)