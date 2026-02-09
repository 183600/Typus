#!/usr/bin/env runhaskell

import qualified Utils

-- 测试 "\""
testQuotedBackslash :: IO ()
testQuotedBackslash = do
    let input = "\""
    putStrLn $ "=== 测试输入: " ++ show input ++ " ==="
    
    putStrLn $ "isCompleteStringLiteral " ++ show input ++ " = " ++ show (Utils.isCompleteStringLiteral input)
    putStrLn $ "isProblematicUnclosedString " ++ show input ++ " = " ++ show (Utils.isProblematicUnclosedString input)
    
    -- 检查字符编码
    putStrLn $ "字符编码: " ++ show (map (\c -> (c, fromEnum c)) input)
    
    -- 检查测试期望
    putStrLn "\n=== 测试期望 ==="
    putStrLn "isCompleteStringLiteral invalid 测试期望: False"
    putStrLn "isProblematicUnclosedString 测试期望: True"
    
    putStrLn "\n=== 实际结果 ==="
    putStrLn $ "isCompleteStringLiteral 结果: " ++ show (Utils.isCompleteStringLiteral input) ++ " (期望: False)"
    putStrLn $ "isProblematicUnclosedString 结果: " ++ show (Utils.isProblematicUnclosedString input) ++ " (期望: True)"
    
    putStrLn "\n=== 结论 ==="
    putStrLn $ "isCompleteStringLiteral 符合期望: " ++ show (not (Utils.isCompleteStringLiteral input))
    putStrLn $ "isProblematicUnclosedString 符合期望: " ++ show (Utils.isProblematicUnclosedString input)

main :: IO ()
main = testQuotedBackslash