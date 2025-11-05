#!/usr/bin/env stack
{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Ownership as Own

main :: IO ()
main = do
    putStrLn "Testing double move detection..."

    -- Test case from the failing property
    let testCode1 = "x := []int{1}; y := x; z := x"
    let testCode2 = "data := []int{2}; copy1 := data; copy2 := data"

    let result1 = Own.analyzeOwnership testCode1
    let result2 = Own.analyzeOwnership testCode2

    putStrLn $ "Test 1: " ++ testCode1
    putStrLn $ "Result 1: " ++ show result1
    putStrLn $ "Should have errors: " ++ show (not (null result1))
    putStrLn ""

    putStrLn $ "Test 2: " ++ testCode2
    putStrLn $ "Result 2: " ++ show result2
    putStrLn $ "Should have errors: " ++ show (not (null result2))
    putStrLn ""

    -- Check if property holds
    let bothHaveErrors = not (null result1) && not (null result2)
    putStrLn $ "Property 'Double move always detected' holds: " ++ show bothHaveErrors

    if bothHaveErrors
        then putStrLn "✓ Test passed"
        else putStrLn "✗ Test failed"