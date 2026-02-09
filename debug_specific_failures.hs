#!/usr/bin/env runhaskell

-- 导入Utils模块
import Utils

-- 测试特定的失败案例
main :: IO ()
main = do
    putStrLn "=== 测试特定的失败案例 ==="
    
    -- 从测试输出看到的实际失败案例
    let test1 = "\"\\\"a"  -- IsCompleteStringLiteral invalid 的失败案例
    putStrLn $ "测试1: isCompleteStringLiteral \"" ++ test1 ++ "\""
    putStrLn $ "结果: " ++ show (isCompleteStringLiteral test1)
    putStrLn $ "期望: False (根据测试名称'invalid')"
    putStrLn ""
    
    let test2 = "\"\\\"\""  -- IsCompleteStringLiteral escaped quotes 的失败案例
    putStrLn $ "测试2: isCompleteStringLiteral \"" ++ test2 ++ "\""
    putStrLn $ "结果: " ++ show (isCompleteStringLiteral test2)
    putStrLn $ "期望: True (根据测试名称'escaped quotes')"
    putStrLn ""
    
    let test3 = "'\\"  -- IsProblematicUnclosedString 的失败案例
    putStrLn $ "测试3: isProblematicUnclosedString \"" ++ test3 ++ "\""
    putStrLn $ "结果: " ++ show (isProblematicUnclosedString test3)
    putStrLn $ "期望: True (根据测试名称)"
    putStrLn ""
    
    -- 测试属性中定义的案例
    putStrLn "=== 测试属性中定义的案例 ==="
    
    -- prop_is_complete_string_literal_invalid 中的案例
    putStrLn $ "测试4: isCompleteStringLiteral \"\\\\\": " ++ show (isCompleteStringLiteral "\"\\")
    putStrLn $ "期望: False"
    
    putStrLn $ "测试5: isCompleteStringLiteral '\\\\\\': " ++ show (isCompleteStringLiteral "'\\")
    putStrLn $ "期望: False"
    
    -- prop_is_complete_string_literal_escaped_quotes 中的案例
    putStrLn $ "测试6: isCompleteStringLiteral \"\\\\\\\"\\\": " ++ show (isCompleteStringLiteral "\"\\\"\"")
    putStrLn $ "期望: True"
    
    -- prop_is_problematic_unclosed_string 中的案例
    putStrLn $ "测试7: isProblematicUnclosedString \"\\\\\\\"\\\": " ++ show (isProblematicUnclosedString "\"\\\"\"")
    putStrLn $ "期望: True"
