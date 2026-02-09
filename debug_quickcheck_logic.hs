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
    
    -- 测试非空 validS 的情况
    let validS2 = "test"
    let stringWithoutEndQuote2 = "\"" ++ validS2 ++ "\\"  -- 添加反斜杠确保字符串不完整
    let stringWithoutEndQuoteSingle2 = "'" ++ validS2 ++ "\\"  -- 单引号版本
    
    putStrLn $ "\nvalidS = " ++ show validS2
    putStrLn $ "stringWithoutEndQuote = " ++ show stringWithoutEndQuote2
    putStrLn $ "stringWithoutEndQuoteSingle = " ++ show stringWithoutEndQuoteSingle2
    
    putStrLn $ "isCompleteStringLiteral \"" ++ show stringWithoutEndQuote2 ++ "\" = " ++ show (Utils.isCompleteStringLiteral stringWithoutEndQuote2)
    putStrLn $ "isCompleteStringLiteral \"" ++ show stringWithoutEndQuoteSingle2 ++ "\" = " ++ show (Utils.isCompleteStringLiteral stringWithoutEndQuoteSingle2)
    
    let nonNullCaseResult = not (Utils.isCompleteStringLiteral stringWithoutEndQuote2) && 
                           not (Utils.isCompleteStringLiteral stringWithoutEndQuoteSingle2)
    putStrLn $ "非空 validS 情况结果: " ++ show nonNullCaseResult
    
    -- 测试 isProblematicUnclosedString 的逻辑
    putStrLn "\n--- isProblematicUnclosedString 逻辑 ---"
    
    -- 测试 null validS 的情况
    let validS3 = ""
    let problematicString = "\"\\\\\"" ++ validS3  -- 不添加结尾引号，确保不完整
    
    putStrLn $ "validS = " ++ show validS3
    putStrLn $ "problematicString = " ++ show problematicString
    putStrLn $ "isProblematicUnclosedString \"\\\\\\\\\"\" = " ++ show (Utils.isProblematicUnclosedString "\"\\\\\"")
    
    let nullCaseResult3 = Utils.isProblematicUnclosedString "\"\\\\\""
    putStrLn $ "null validS 情况结果: " ++ show nullCaseResult3
    
    -- 测试非空 validS 的情况
    let validS4 = "test"
    let problematicString2 = "\"\\\\\"" ++ validS4  -- 不添加结尾引号，确保不完整
    
    putStrLn $ "\nvalidS = " ++ show validS4
    putStrLn $ "problematicString = " ++ show problematicString2
    putStrLn $ "isProblematicUnclosedString \"" ++ show problematicString2 ++ "\" = " ++ show (Utils.isProblematicUnclosedString problematicString2)
    
    let nonNullCaseResult3 = Utils.isProblematicUnclosedString problematicString2
    putStrLn $ "非空 validS 情况结果: " ++ show nonNullCaseResult3