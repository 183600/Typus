#!/usr/bin/env runhaskell

-- 导入Utils模块
import Utils

-- 测试新的失败案例
main :: IO ()
main = do
    putStrLn "=== 测试新的失败案例 ==="
    
    -- IsCompleteStringLiteral invalid 的失败案例
    let test1 = "\"a"
    putStrLn $ "测试1: IsCompleteStringLiteral invalid"
    putStrLn $ "  字符串: " ++ show test1
    putStrLn $ "  isCompleteStringLiteral: " ++ show (isCompleteStringLiteral test1)
    putStrLn $ "  期望: False (根据测试名称'invalid')"
    putStrLn ""
    
    -- IsCompleteStringLiteral escaped quotes 的失败案例
    let test2 = "'"
    putStrLn $ "测试2: IsCompleteStringLiteral escaped quotes"
    putStrLn $ "  字符串: " ++ show test2
    putStrLn $ "  isCompleteStringLiteral: " ++ show (isCompleteStringLiteral test2)
    putStrLn $ "  期望: False (单个单引号不是完整的字符串字面量)"
    putStrLn ""
    
    -- IsProblematicUnclosedString 的失败案例
    let test3 = ""
    putStrLn $ "测试3: IsProblematicUnclosedString"
    putStrLn $ "  字符串: " ++ show test3
    putStrLn $ "  isProblematicUnclosedString: " ++ show (isProblematicUnclosedString test3)
    putStrLn $ "  期望: False (空字符串不是问题性的未闭合字符串)"
    putStrLn ""
    
    -- 分析这些结果
    putStrLn "=== 分析 ==="
    putStrLn "测试1: \"\\\"a\" - 双引号+a，应该是不完整的，返回 False 是正确的"
    putStrLn "测试2: \"'\" - 单个单引号，应该是不完整的，返回 False 是正确的"
    putStrLn "测试3: \"\"\" - 空字符串，应该返回 False，这是正确的"
    putStrLn ""
    putStrLn "问题可能在于测试属性的逻辑，而不是函数实现"