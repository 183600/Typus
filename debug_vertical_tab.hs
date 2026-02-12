#!/usr/bin/env runhaskell
import Utils

main :: IO ()
main = do
    putStrLn "Testing normalizeIndentation with \"\\v\":"
    let input = "\v"
    let result = normalizeIndentation input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Expected: " ++ show input