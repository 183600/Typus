import System.IO
import Compiler.TypeChecker
import Compiler.IR
import Compiler.GoAst

main :: IO ()
main = do
    -- 读取util.typus文件
    utilContent <- readFile "test/fixtures/full_project/util.typus"
    mainContent <- readFile "test/fixtures/full_project/main.typus"
    
    -- 解析为GoModule
    let utilModule = parseGoModule (lines utilContent)
    let mainModule = parseGoModule (lines mainContent)
    
    putStrLn "=== util.typus解析结果 ==="
    print utilModule
    
    putStrLn "\n=== main.typus解析结果 ==="
    print mainModule
    
    case (utilModule, mainModule) of
        (Right utilGoMod, Right mainGoMod) -> do
            let combinedModule = GoModule
                    { gmPackage = gmPackage mainGoMod
                    , gmImports = gmImports mainGoMod ++ gmImports utilGoMod
                    , gmDecls = gmDecls mainGoMod ++ gmDecls utilGoMod
                    , gmBuildTags = []
                    }
            
            putStrLn "\n=== 合并后的模块 ==="
            print combinedModule
            
            let env = buildTypeEnv combinedModule
            putStrLn "\n=== 类型环境中的函数 ==="
            print (Map.keys (functionTypes env))
            
            let errors = gatherTypeErrors env combinedModule
            putStrLn "\n=== 类型错误 ==="
            print errors
            
        (Left err, _) -> putStrLn $ "util.typus解析错误: " ++ err
        (_, Left err) -> putStrLn $ "main.typus解析错误: " ++ err