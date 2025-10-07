module Main (main) where

import System.Exit (exitFailure)
import Parser (parseTypus)
import Compiler (compile)

main :: IO ()
main = do
    putStrLn "Testing compiler..."
    content <- readFile "simple_test.typus"
    putStrLn $ "Parsing content of length: " ++ show (length content)
    
    case parseTypus content of
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
                    putStrLn $ "Generated code length: " ++ show (length goCode)
                    putStrLn $ "Generated code:\n" ++ goCode