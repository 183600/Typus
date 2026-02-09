#!/usr/bin/env runhaskell

import Test.QuickCheck
import qualified Utils

-- 手动重现 QuickCheck 测试
main :: IO ()
main = do
    putStrLn "=== 手动重现 QuickCheck 测试 ==="
    
    -- 测试 isCompleteStringLiteral invalid
    putStrLn "\n--- isCompleteStringLiteral invalid ---"
    
    -- 模拟测试用例生成
    let testCases = ["\"", "'", "\\", "\"\\", "\\\"", "'\\"]
    mapM_ (\input -> do
        let result = Utils.isCompleteStringLiteral input
        let expected = False
        putStrLn $ "输入: " ++ show input ++ ", 结果: " ++ show result ++ ", 期望: " ++ show expected ++ ", 符合: " ++ show (result == expected)
        ) testCases
    
    -- 测试 isProblematicUnclosedString
    putStrLn "\n--- isProblematicUnclosedString ---"
    
    mapM_ (\input -> do
        let result = Utils.isProblematicUnclosedString input
        let expected = input `elem` ["\"", "'", "\"\\\"", "'\\"]
        putStrLn $ "输入: " ++ show input ++ ", 结果: " ++ show result ++ ", 期望: " ++ show expected ++ ", 符合: " ++ show (result == expected)
        ) testCases
    
    -- 运行实际的 QuickCheck 测试
    putStrLn "\n--- 运行实际的 QuickCheck 测试 ---"
    
    -- isCompleteStringLiteral invalid
    putStrLn "测试 isCompleteStringLiteral invalid:"
    quickCheck $ \s ->
        let validS = take 50 s
            stringWithoutEndQuote = "\"" ++ validS ++ "\\"  -- 添加反斜杠确保字符串不完整
            stringWithoutEndQuoteSingle = "'" ++ validS ++ "\\"  -- 单引号版本
        in if null validS
           then not (Utils.isCompleteStringLiteral "\"\\") &&  -- 反斜杠后没有引号，应该是不完整的
               not (Utils.isCompleteStringLiteral "'\\")      -- 单引号版本同理
           else not (Utils.isCompleteStringLiteral stringWithoutEndQuote) &&
               not (Utils.isCompleteStringLiteral stringWithoutEndQuoteSingle)
    
    -- isProblematicUnclosedString
    putStrLn "测试 isProblematicUnclosedString:"
    quickCheck $ \s ->
        let validS = take 30 s
            -- 确保字符串以引号开头，后跟反斜杠，并且不是完整的字符串字面量
            problematicString = "\"\\\"" ++ validS  -- 不添加结尾引号，确保不完整
        in if null validS
           then Utils.isProblematicUnclosedString "\"\\\""  -- 包含转义引号但不完整的字符串
           else Utils.isProblematicUnclosedString problematicString