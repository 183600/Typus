module Main where

import Parser

main :: IO ()
main = do
    let testInput = unlines [
            "//! ownership: on",
            "//! dependent_types: off",
            "",
            "package main",
            "",
            "func main() {",
            "    println(\"Hello\")",
            "}"
            ]
    
    case parse testInput of
        Left err -> putStrLn $ "Parse error: " ++ err
        Right ast -> do
            putStrLn "Parsed successfully!"
            print ast
            putStrLn $ "Ownership directive: " ++ show (fdOwnership (tfDirectives ast))
            putStrLn $ "Dependent types directive: " ++ show (fdDependentTypes (tfDirectives ast))