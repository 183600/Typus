-- 简单的调试脚本
module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map as Map
import Compiler.GoAst
import Compiler.TypeChecker
import Compiler.IR

main :: IO ()
main = do
    -- 读取文件内容
    utilContent <- TIO.readFile "test/fixtures/full_project/util.typus"
    mainContent <- TIO.readFile "test/fixtures/full_project/main.typus"
    
    -- 转换为String
    let utilStr = T.unpack utilContent
    let mainStr = T.unpack mainContent
    
    -- 解析为GoModule
    let utilModule = parseGoModule (lines utilStr)
    let mainModule = parseGoModule (lines mainStr)
    
    putStrLn "=== util.typus解析结果 ==="
    case utilModule of
        Left err -> putStrLn $ "错误: " ++ err
        Right mod -> do
            putStrLn $ "包名: " ++ show (gmPackage mod)
            putStrLn $ "导入: " ++ show (gmImports mod)
            putStrLn $ "声明数量: " ++ show (length (gmDecls mod))
            mapM_ print (gmDecls mod)
    
    putStrLn "\n=== main.typus解析结果 ==="
    case mainModule of
        Left err -> putStrLn $ "错误: " ++ err
        Right mod -> do
            putStrLn $ "包名: " ++ show (gmPackage mod)
            putStrLn $ "导入: " ++ show (gmImports mod)
            putStrLn $ "声明数量: " ++ show (length (gmDecls mod))
            mapM_ print (gmDecls mod)
    
    -- 测试合并
    case (utilModule, mainModule) of
        (Right utilGoMod, Right mainGoMod) -> do
            let combinedModule = GoModule
                    { gmPackage = gmPackage mainGoMod
                    , gmImports = gmImports mainGoMod ++ gmImports utilGoMod
                    , gmDecls = gmDecls mainGoMod ++ gmDecls utilGoMod
                    , gmBuildTags = []
                    }
            
            putStrLn "\n=== 合并后的模块 ==="
            putStrLn $ "包名: " ++ show (gmPackage combinedModule)
            putStrLn $ "导入: " ++ show (gmImports combinedModule)
            putStrLn $ "声明数量: " ++ show (length (gmDecls combinedModule))
            
            -- 构建类型环境
            let env = buildTypeEnv combinedModule
            putStrLn "\n=== 类型环境中的函数 ==="
            putStrLn $ "函数数量: " ++ show (Map.size (functionTypes env))
            mapM_ print (Map.toList (functionTypes env))
            
        (Left err, _) -> putStrLn $ "util.typus解析错误: " ++ err
        (_, Left err) -> putStrLn $ "main.typus解析错误: " ++ err