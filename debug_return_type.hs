-- 测试返回类型解析
module Main where

import Compiler.TypeChecker (parseFunctionSignature, FunctionSignature(..))

main :: IO ()
main = do
    let header = "func Add(a int, b int) int"
    case parseFunctionSignature header of
        Nothing -> putStrLn "解析失败"
        Just sig -> do
            putStrLn $ "参数: " ++ show (fsParams sig)
            putStrLn $ "返回类型: " ++ show (fsReturns sig)