import Compiler.GoAst

main :: IO ()
main = do
    let source = "package main\nfunc a() { b() }\nfunc b() { a() }\nfunc main() { a() }"
    case parseGoModule (lines source) of
        Left err -> putStrLn $ "Parse error: " ++ err
        Right goModule -> do
            putStrLn $ "Package: " ++ show (gmPackage goModule)
            putStrLn $ "Decls: " ++ show (gmDecls goModule)