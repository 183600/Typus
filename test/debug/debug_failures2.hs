#!/usr/bin/env runhaskell
import Utils
import Data.List (isPrefixOf)

-- Test failing case 1: normalizeIndentation with "\t\t\r\t"
main :: IO ()
main = do
    putStrLn "Testing normalizeIndentation with \"\\t\\t\\r\\t\":"
    let input = "\t\t\r\t"
    let result = normalizeIndentation input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Starts with \\t\\t: " ++ show ("\t\t" `isPrefixOf` result)
    putStrLn ""
