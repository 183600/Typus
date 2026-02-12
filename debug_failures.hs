#!/usr/bin/env runhaskell
import Utils

-- Test failing case 1: normalizeIndentation with "\r"
main :: IO ()
main = do
    putStrLn "Testing normalizeIndentation with \"\\r\":"
    let input1 = "\r"
    let result1 = normalizeIndentation input1
    putStrLn $ "Input: " ++ show input1
    putStrLn $ "Result: " ++ show result1
    putStrLn ""
    
    -- Test failing case 2: removeComments with "b\n"
    putStrLn "Testing removeComments with \"b\\n\":"
    let input2 = "//b\n"
    let result2 = removeComments input2
    putStrLn $ "Input: " ++ show input2
    putStrLn $ "Result: " ++ show result2
    putStrLn ""