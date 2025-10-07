module Main (main) where

import System.Exit (exitFailure)
import Parser (parseTypus)

main :: IO ()
main = do
    putStrLn "Testing parser..."
    content <- readFile "simple_test.typus"
    putStrLn $ "Parsing content of length: " ++ show (length content)
    
    case parseTypus content of
        Left err -> do
            putStrLn $ "Parse error: " ++ err
            exitFailure
        Right result -> do
            putStrLn "Parse successful!"
            putStrLn $ "Result: " ++ show result