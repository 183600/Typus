module Main where
import Parser

main :: IO ()
main = do
    content <- readFile "fixtures/reference/250921.typus"
    case parseTypus content of
        Left err -> putStrLn $ "Parse error: " ++ err
        Right typusFile -> do
            putStrLn $ "Parse successful!"
            putStrLn $ "Number of blocks: " ++ show (length (tfBlocks typusFile))
            putStrLn $ "File directives: " ++ show (tfDirectives typusFile)
            case tfBlocks typusFile of
                [] -> putStrLn "No blocks found"
                (firstBlock:_) -> putStrLn $ "First block content: " ++ take 100 (cbContent firstBlock)