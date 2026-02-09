#!/usr/bin/env runhaskell

import qualified Utils

-- 手动重现 QuickCheck 测试的逻辑
main :: IO ()
main = do
    putStrLn "=== 手动重现 QuickCheck 测试 ==="
    
    -- 测试 isCompleteStringLiteral invalid 的逻辑
    putStrLn "\n--- isCompleteStringLiteral invalid 逻辑 ---"
    
    -- 测试 null validS 的情况
    let validS = ""
    let stringWithoutEndQuote = "\"" ++ validS ++ "\\"  -- 添加反斜杠确保字符串不完整
    let stringWithoutEndQuoteSingle = "'" ++ validS ++ "\\"  -- 单引号版本
    
    putStrLn $ "validS = " ++ show validS
    putStrLn $ "stringWithoutEndQuote = " ++ show stringWithoutEndQuote
    putStrLn $ "stringWithoutEndQuoteSingle = " ++ show stringWithoutEndQuoteSingle
    
    putStrLn $ "isCompleteStringLiteral \"\\\\\" = " ++ show (Utils.isCompleteStringLiteral "\\")
    putStrLn $ "isCompleteStringLiteral \"'\\\\\" = " ++ show (Utils.isCompleteStringLiteral "'\\")
    
    let nullCaseResult = not (Utils.isCompleteStringLiteral "\\") && 
                        not (Utils.isCompleteStringLiteral "'\\")
    putStrLn $ "null validS 情况结果: " ++ show nullCaseResult
    
    -- 测试 isProblematicUnclosedString 的逻辑
    putStrLn "\n--- isProblematicUnclosedString 逻辑 ---"
    
    -- 测试 null validS 的情况
    let validS3 = ""
    let problematicString = "\"\\\\\"" ++ validS3  -- 不添加结尾引号，确保不完整
    
    putStrLn $ "validS = " ++ show validS3
    putStrLn $ "problematicString = " ++ show problematicString
    putStrLn $ "isProblematicUnclosedString problematicString = " ++ show (Utils.isProblematicUnclosedString problematicString)
    
    let nullCaseResult3 = Utils.isProblematicUnclosedString problematicString
    putStrLn $ "null validS 情况结果: " ++ show nullCaseResult3