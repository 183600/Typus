module Main (main) where

import System.Exit (exitFailure)
import qualified Data.List as L
import Parser (parseTypus)
import Compiler (compile, renderCompilationError)

main :: IO ()
main = do
    putStrLn "Testing compiler..."
    content <- readFile "fixtures/reference/simple_test.typus"
    putStrLn $ "Parsing content of L.length: " ++ show (L.length content)
    
    case parseTypus content of
        Left err -> do
            putStrLn $ "Parse error: " ++ err
            exitFailure
        Right typusFile -> do
            putStrLn "Parse successful!"
            putStrLn "Testing compilation..."
            
            case compile typusFile of
                Left err -> do
                    putStrLn $ "Compilation error: " ++ renderCompilationError err
                    exitFailure
                Right goCode -> do
                    putStrLn "Compilation successful!"
                    putStrLn $ "Generated code L.length: " ++ show (L.length goCode)
                    putStrLn $ "Generated code:\n" ++ goCode