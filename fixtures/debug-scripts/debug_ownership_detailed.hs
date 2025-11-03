#!/usr/bin/env stack
-- stack --resolver lts-20.0 script

{-# LANGUAGE OverloadedStrings #-}

import Ownership
import System.IO

main :: IO ()
main = do
  putStrLn "Testing ownership analysis with detailed debugging:"
  
  let testCases =
        [ ("x := 42", "Simple assignment")
        , ("y := \"hello\"", "String assignment")
        , ("z := []int{1, 2, 3}", "Slice with int type")
        , ("a := z", "Variable assignment")
        , ("fmt.Println(a)", "Function call with fmt.Println")
        ]
  
  mapM_ (uncurry testCode) testCases
  
  putStrLn "\nTesting the property-based test logic:"
  let validCodes = map fst testCases
      results = map analyzeOwnership validCodes
  putStrLn $ "Results: " ++ show results
  putStrLn $ "All null? " ++ show (all null results)
  
  putStrLn "\nTesting with debug mode:"
  let (errors, debugLog) = analyzeOwnershipDebug True "z := []int{1, 2, 3}"
  putStrLn $ "Errors: " ++ show errors
  putStrLn $ "Debug log:"
  mapM_ putStrLn debugLog

testCode :: String -> String -> IO ()
testCode code description = do
  putStrLn $ "\n=== " ++ description ++ " ==="
  putStrLn $ "Code: " ++ code
  let (errors, debugLog) = analyzeOwnershipDebug True code
  putStrLn $ "Errors: " ++ show errors
  putStrLn $ "Debug log:"
  mapM_ putStrLn (take 10 debugLog)  -- Limit debug output