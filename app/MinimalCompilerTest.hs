module Main (main) where

import System.Exit (exitFailure)
import Parser (parseTypus)
import Compiler (compile)

main :: IO ()
main = do
    putStrLn "Testing compiler with minimal input..."
    let minimalContent = "package main\n\nfunc main() {\n}"
    putStrLn $ "Parsing content: " ++ minimalContent
    
    case parseTypus minimalContent of
        Left err -> do
            putStrLn $ "Parse error: " ++ err
            exitFailure
        Right typusFile -> do
            putStrLn "Parse successful!"
            putStrLn "Testing compilation..."
            
            case compile typusFile of
                Left err -> do
                    putStrLn $ "Compilation error: " ++ err
                    exitFailure
                Right goCode -> do
                    putStrLn "Compilation successful!"
                    putStrLn $ "Generated code: " ++ goCode