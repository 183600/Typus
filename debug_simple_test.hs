module Main where

import Utils

main :: IO ()
main = do
    putStrLn "=== 测试 isCompleteStringLiteral ==="
    
    -- 测试失败案例: "\""
    let test1 = "\""
    let result1 = isCompleteStringLiteral test1
    putStrLn $ "isCompleteStringLiteral " ++ show test1 ++ " = " ++ show result1 ++ " (期望: True)"
    
    -- 测试失败案例: "'"
    let test2 = "'"
    let result2 = isCompleteStringLiteral test2
    putStrLn $ "isCompleteStringLiteral " ++ show test2 ++ " = " ++ show result2 ++ " (期望: True)"
    
    putStrLn "\n=== 测试 isProblematicUnclosedString ==="
    
    -- 测试失败案例: ""
    let test3 = ""
    let result3 = isProblematicUnclosedString test3
    putStrLn $ "isProblematicUnclosedString " ++ show test3 ++ " = " ++ show result3 ++ " (期望: True)"
    
    -- 测试失败案例: "\""
    let test4 = "\""
    let result4 = isProblematicUnclosedString test4
    putStrLn $ "isProblematicUnclosedString " ++ show test4 ++ " = " ++ show result4 ++ " (期望: True)"