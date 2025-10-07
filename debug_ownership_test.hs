#!/usr/bin/env stack
-- stack --resolver lts-20.0 script

{-# LANGUAGE OverloadedStrings #-}

import Ownership
import System.IO

main :: IO ()
main = do
  putStrLn "Testing ownership analysis on valid codes:"
  
  let validCodes =
        [ "x := 42"
        , "y := \"hello\""
        , "z := []int{1, 2, 3}"
        , "a := z"
        , "fmt.Println(a)"
        ]
  
  mapM_ testCode validCodes
  
  putStrLn "\nTesting the property-based test logic:"
  let results = map analyzeOwnership validCodes
  putStrLn $ "Results: " ++ show results
  putStrLn $ "All null? " ++ show (all null results)

testCode :: String -> IO ()
testCode code = do
  putStrLn $ "\nCode: " ++ code
  let errors = analyzeOwnership code
  putStrLn $ "Errors: " ++ show errors