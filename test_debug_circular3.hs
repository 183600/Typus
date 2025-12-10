import Compiler.TypeChecker
import Compiler.GoAst
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
    let source = "package main\nfunc a() { b() }\nfunc b() { a() }\nfunc main() { a() }"
    case parseGoModule (lines source) of
        Left err -> putStrLn $ "Parse error: " ++ err
        Right goModule -> do
            putStrLn $ "Decls: " ++ show (length (gmDecls goModule))
            let functionInfos = mapMaybe parseFunctionInfoFromDecl (gmDecls goModule)
            putStrLn $ "Function infos: " ++ show (length functionInfos)
            mapM_ (\FunctionInfo{..} -> do
                putStrLn $ "Function: " ++ fiName
                putStrLn $ "Body: " ++ show fiBody
                let calls = extractCallExpressions fiBody
                putStrLn $ "Calls: " ++ show calls
                ) functionInfos