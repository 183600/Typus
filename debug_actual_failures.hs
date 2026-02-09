#!/usr/bin/env runhaskell

-- 导入Utils模块
import Utils

-- 测试实际的失败案例
main :: IO ()
main = do
    putStrLn "=== 测试实际的失败案例 ==="
    
    -- RemoveComments strings with comments 的失败案例
    let test1 = "\""  -- 第一个参数
    let test2 = ""    -- 第二个参数
    putStrLn $ "测试1: RemoveComments strings with comments"
    putStrLn $ "  参数1: " ++ show test1
    putStrLn $ "  参数2: " ++ show test2
    putStrLn $ "  removeComments test1: " ++ show (removeComments test1)
    putStrLn $ "  removeComments test2: " ++ show (removeComments test2)
    putStrLn ""
    
    -- IsCompleteStringLiteral invalid 的失败案例
    let test3 = "'a"
    putStrLn $ "测试2: IsCompleteStringLiteral invalid"
    putStrLn $ "  字符串: " ++ show test3
    putStrLn $ "  isCompleteStringLiteral: " ++ show (isCompleteStringLiteral test3)
    putStrLn $ "  期望: False (根据测试名称'invalid')"
    putStrLn ""
    
    -- IsProblematicUnclosedString 的失败案例
    let test4 = "\""
    putStrLn $ "测试3: IsProblematicUnclosedString"
    putStrLn $ "  字符串: " ++ show test4
    putStrLn $ "  isProblematicUnclosedString: " ++ show (isProblematicUnclosedString test4)
    putStrLn $ "  期望: True (根据测试名称)"
    putStrLn ""
    
    -- 额外测试：检查这些案例的属性计算
    putStrLn "=== 属性计算分析 ==="
    
    -- 对于 IsCompleteStringLiteral invalid
    let validS = take 50 "'a"
    let stringWithoutEndQuote = "\"" ++ validS ++ "\\"
    let stringWithoutEndQuoteSingle = "'" ++ validS ++ "\\"
    putStrLn $ "IsCompleteStringLiteral invalid 属性计算:"
    putStrLn $ "  validS: " ++ show validS
    putStrLn $ "  stringWithoutEndQuote: " ++ show stringWithoutEndQuote
    putStrLn $ "  stringWithoutEndQuoteSingle: " ++ show stringWithoutEndQuoteSingle
    putStrLn $ "  结果: " ++ show (if null validS 
                                  then not (isCompleteStringLiteral "\"\\") && not (isCompleteStringLiteral "'\\")
                                  else not (isCompleteStringLiteral stringWithoutEndQuote) && not (isCompleteStringLiteral stringWithoutEndQuoteSingle))
    putStrLn ""
    
    -- 对于 IsProblematicUnclosedString
    let validS2 = take 30 "\""
    let problematicString = "\"\\\"" ++ validS2
    putStrLn $ "IsProblematicUnclosedString 属性计算:"
    putStrLn $ "  validS: " ++ show validS2
    putStrLn $ "  problematicString: " ++ show problematicString
    putStrLn $ "  结果: " ++ show (if null validS2 
                                  then isProblematicUnclosedString "\"\\\""
                                  else isProblematicUnclosedString problematicString)