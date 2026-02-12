#!/usr/bin/env runhaskell
import Utils

main :: IO ()
main = do
    putStrLn "Testing removeComments with \"//b\\n\":"
    let input = "//b\n"
    let result = removeComments input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Expected: \"b\\n\""