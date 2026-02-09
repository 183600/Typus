#!/usr/bin/env runhaskell

-- 导入Utils模块
import Utils

-- 测试失败的具体案例
main :: IO ()
main = do
    putStrLn "=== 测试失败案例 ==="
    
    -- 案例1: IsCompleteStringLiteral invalid - "\"a
    let test1 = "\"\\\"a"
    putStrLn $ "测试1: isCompleteStringLiteral \"" ++ test1 ++ "\""
    putStrLn $ "结果: " ++ show (isCompleteStringLiteral test1)
    putStrLn $ "期望: False"
    putStrLn ""
    
    -- 案例2: IsCompleteStringLiteral escaped quotes - \"\"\"
    let test2 = "\"\\\"\\\"\""
    putStrLn $ "测试2: isCompleteStringLiteral \"" ++ test2 ++ "\""
    putStrLn $ "结果: " ++ show (isCompleteStringLiteral test2)
    putStrLn $ "期望: True"
    putStrLn ""
    
    -- 案例3: IsProblematicUnclosedString - '
    let test3 = "'\\\""
    putStrLn $ "测试3: isProblematicUnclosedString \"" ++ test3 ++ "\""
    putStrLn $ "结果: " ++ show (isProblematicUnclosedString test3)
    putStrLn $ "期望: True"
    putStrLn ""
    
    -- 额外测试：原始失败案例
    putStrLn "=== 原始失败案例 ==="
    putStrLn $ "isCompleteStringLiteral \"\\\"a: " ++ show (isCompleteStringLiteral "\"\\\"a")
    putStrLn $ "isCompleteStringLiteral \"\\\"\\\": " ++ show (isCompleteStringLiteral "\"\\\"\\\"")
    putStrLn $ "isProblematicUnclosedString \"': " ++ show (isProblematicUnclosedString "'\\\"")