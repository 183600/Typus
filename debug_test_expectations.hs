module Main where

import Utils

main :: IO ()
main = do
    putStrLn "=== 测试测试期望的案例 ==="
    
    -- prop_is_complete_string_literal_invalid 期望的案例
    let test1 = "\"\\"
    let result1 = isCompleteStringLiteral test1
    putStrLn $ "isCompleteStringLiteral " ++ show test1 ++ " = " ++ show result1 ++ " (测试期望: False)"
    
    let test2 = "'\\"
    let result2 = isCompleteStringLiteral test2
    putStrLn $ "isCompleteStringLiteral " ++ show test2 ++ " = " ++ show result2 ++ " (测试期望: False)"
    
    -- prop_is_complete_string_literal_escaped_quotes 期望的案例
    let test3 = "\"\\\"\""
    let result3 = isCompleteStringLiteral test3
    putStrLn $ "isCompleteStringLiteral " ++ show test3 ++ " = " ++ show result3 ++ " (测试期望: True)"
    
    -- prop_is_problematic_unclosed_string 期望的案例
    let test4 = ""
    let result4 = isProblematicUnclosedString test4
    putStrLn $ "isProblematicUnclosedString " ++ show test4 ++ " = " ++ show result4 ++ " (测试期望: True)"