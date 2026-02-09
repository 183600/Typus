module Main where

import Utils

main :: IO ()
main = do
    putStrLn "=== 测试剩余的失败案例 ==="
    
    -- IsCompleteStringLiteral invalid 失败案例: "\"a
    let test1 = "\"a"
    let result1 = isCompleteStringLiteral test1
    putStrLn $ "isCompleteStringLiteral " ++ show test1 ++ " = " ++ show result1 ++ " (期望: False)"
    
    -- IsProblematicUnclosedString 失败案例: ""
    let test2 = ""
    let result2 = isProblematicUnclosedString test2
    putStrLn $ "isProblematicUnclosedString " ++ show test2 ++ " = " ++ show result2 ++ " (期望: True)"
    
    -- 分析 IsCompleteStringLiteral invalid 测试的逻辑
    putStrLn "\n=== 分析 IsCompleteStringLiteral invalid 测试 ==="
    let s = "a"
    let validS = take 50 s
    let stringWithoutEndQuote = "\"" ++ validS ++ "\\"  -- 添加反斜杠确保字符串不完整
    let stringWithoutEndQuoteSingle = "'" ++ validS ++ "\\"  -- 单引号版本
    
    putStrLn $ "validS = " ++ show validS
    putStrLn $ "stringWithoutEndQuote = " ++ show stringWithoutEndQuote
    putStrLn $ "stringWithoutEndQuoteSingle = " ++ show stringWithoutEndQuoteSingle
    
    if null validS
    then do
        let test3 = "\"\\"
        let test4 = "'\\"
        let result3 = isCompleteStringLiteral test3
        let result4 = isCompleteStringLiteral test4
        putStrLn $ "null validS 情况:"
        putStrLn $ "  isCompleteStringLiteral " ++ show test3 ++ " = " ++ show result3 ++ " (期望: False)"
        putStrLn $ "  isCompleteStringLiteral " ++ show test4 ++ " = " ++ show result4 ++ " (期望: False)"
    else do
        let result5 = isCompleteStringLiteral stringWithoutEndQuote
        let result6 = isCompleteStringLiteral stringWithoutEndQuoteSingle
        putStrLn $ "非 null validS 情况:"
        putStrLn $ "  isCompleteStringLiteral " ++ show stringWithoutEndQuote ++ " = " ++ show result5 ++ " (期望: False)"
        putStrLn $ "  isCompleteStringLiteral " ++ show stringWithoutEndQuoteSingle ++ " = " ++ show result6 ++ " (期望: False)"