module Main where

import Parser
import SourceLocation (Located, locatedValue)

directiveValue :: Maybe (Located a) -> Maybe a
directiveValue = fmap locatedValue

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
    
    case parseTypus testInput of
        Left err -> putStrLn $ "Error: " ++ err
        Right ast -> do
            putStrLn "Parsed successfully!"
            print ast
            let dirs = tfDirectives ast
            putStrLn $ "Ownership directive: " ++ show (directiveValue (fdOwnership dirs))
