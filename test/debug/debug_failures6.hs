#!/usr/bin/env runhaskell
import Utils

main :: IO ()
main = do
    let input = "\t\t \n\t"
    putStrLn $ "Testing normalizeIndentation with: " ++ show input
    let result = normalizeIndentation input
    putStrLn $ "Result: " ++ show result