import Compiler.TypeChecker (parseFunctionInfo, FunctionInfo(..))
import Compiler.GoAst (FuncDecl(..))

main :: IO ()
main = do
    let funcA = FuncDecl ["func a() { b() }"]
    let funcB = FuncDecl ["func b() { a() }"]
    let infoA = parseFunctionInfo funcA
    let infoB = parseFunctionInfo funcB
    putStrLn $ "Function A: " ++ show infoA
    putStrLn $ "Function B: " ++ show infoB
    
    case infoA of
        Just FunctionInfo{..} -> do
            putStrLn $ "Function A name: " ++ fiName
            putStrLn $ "Function A body: " ++ show fiBody
        Nothing -> putStrLn $ "Failed to parse function A"
    
    case infoB of
        Just FunctionInfo{..} -> do
            putStrLn $ "Function B name: " ++ fiName
            putStrLn $ "Function B body: " ++ show fiBody
        Nothing -> putStrLn $ "Failed to parse function B"