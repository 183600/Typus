import Parser

main :: IO ()
main = do
    let source = "package main\nfunc main() {\n    if true {\n"
    case Parser.parseTypus source of
        Left err -> putStrLn $ "Parse error: " ++ err
        Right result -> putStrLn $ "Parse succeeded: " ++ show result