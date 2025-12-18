import Compiler.GoAst
import Compiler.TypeChecker
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
    let source = "package main\nfunc a() { b() }\nfunc b() { a() }\nfunc main() { a() }"
    case parseGoModule (lines source) of
        Left err -> putStrLn $ "Parse error: " ++ err
        Right goModule -> do
            putStrLn $ "Decls: " ++ show (gmDecls goModule)
            let functionInfos = mapMaybe parseFunctionInfoFromDecl (gmDecls goModule)
            putStrLn $ "Function infos: " ++ show functionInfos
            let callGraph = Map.fromList $ map (\FunctionInfo{..} -> (fiName, map callName (extractCallExpressions fiBody))) functionInfos
            putStrLn $ "Call graph: " ++ show callGraph