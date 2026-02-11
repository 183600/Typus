#!/usr/bin/env runhaskell

import Utils

main :: IO ()
main = do
    putStrLn "=== Testing isCompleteStringLiteral with escaped quotes ==="
    
    -- The failing test case: "c\""
    let testStr = "\"c\\\"\""
    putStrLn $ "Test string: " ++ show testStr
    putStrLn $ "isCompleteStringLiteral result: " ++ show (isCompleteStringLiteral testStr)
    
    -- Test other similar cases
    putStrLn "\n=== Testing similar cases ==="
    
    putStrLn $ "a with escaped quote: " ++ show ("\"a\\\"\"") ++ " -> " ++ show (isCompleteStringLiteral "\"a\\\"\"")
    putStrLn $ "b with escaped quote: " ++ show ("\"b\\\"\"") ++ " -> " ++ show (isCompleteStringLiteral "\"b\\\"\"")
    putStrLn $ "c with escaped quote: " ++ show ("\"c\\\"\"") ++ " -> " ++ show (isCompleteStringLiteral "\"c\\\"\"")
    putStrLn $ "empty with escaped quote: " ++ show ("\"\\\"\\\"\"") ++ " -> " ++ show (isCompleteStringLiteral "\"\\\"\\\"\"")
    putStrLn $ "a with escaped backslash: " ++ show ("\"a\\\\\"\"") ++ " -> " ++ show (isCompleteStringLiteral "\"a\\\\\"\"")
    putStrLn $ "empty with escaped backslash: " ++ show ("\"\\\\\"\"") ++ " -> " ++ show (isCompleteStringLiteral "\"\\\\\"\"")
    
    -- Let's also check what the test is actually doing
    putStrLn "\n=== Simulating the test ==="
    let s = "c"
    let escaped = "\"" ++ s ++ "\\\"\""
    putStrLn $ "Input s: " ++ show s
    putStrLn $ "Constructed escaped: " ++ show escaped
    putStrLn $ "isCompleteStringLiteral escaped: " ++ show (isCompleteStringLiteral escaped)
