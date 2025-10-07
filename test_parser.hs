module Main where
import Parser
import System.IO

main :: IO ()
main = do
    content <- readFile "250921.typus"
    case parseTypus content of
        Left err -> putStrLn $ "Parse error: " ++ err
        Right typusFile -> do
            putStrLn $ "Parse successful!"
            putStrLn $ "Number of blocks: " ++ show (length (tfBlocks typusFile))
            putStrLn $ "File directives: " ++ show (tfDirectives typusFile)
            putStrLn $ "First block content: " ++ take 100 (cbContent (head (tfBlocks typusFile)))