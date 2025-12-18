import Compiler.TypeChecker
import Compiler.GoAst
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
    let source = "package main\nfunc main() {\n    println(undefinedVar)\n}"
    case parseGoModule (lines source) of
        Left err -> putStrLn $ "Parse error: " ++ err
        Right goModule -> do
            let env = buildTypeEnv goModule
            putStrLn $ "Environment: " ++ show env
            let errors = gatherTypeErrors env goModule
            putStrLn $ "Errors: " ++ show errors