module Main where

import Utils

main :: IO ()
main = do
    putStrLn "=== 测试确切的失败案例 ==="
    
    -- IsCompleteStringLiteral invalid 失败案例: "\"a
    let test1 = "\"a"
    let result1 = isCompleteStringLiteral test1
    putStrLn $ "isCompleteStringLiteral " ++ show test1 ++ " = " ++ show result1 ++ " (期望: False)"
    
    -- IsCompleteStringLiteral escaped quotes 失败案例: "\
    let test2 = "\\"
    let result2 = isCompleteStringLiteral test2
    putStrLn $ "isCompleteStringLiteral " ++ show test2 ++ " = " ++ show result2 ++ " (期望: True)"
    
    -- IsProblematicUnclosedString 失败案例: ""
    let test3 = ""
    let result3 = isProblematicUnclosedString test3
    putStrLn $ "isProblematicUnclosedString " ++ show test3 ++ " = " ++ show result3 ++ " (期望: True)"