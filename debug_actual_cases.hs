module Main where

import Utils

main :: IO ()
main = do
    putStrLn "=== 测试实际的失败案例 ==="
    
    -- IsCompleteStringLiteral escaped quotes 失败案例: "\""
    let test1 = "\""
    let result1 = isCompleteStringLiteral test1
    putStrLn $ "isCompleteStringLiteral " ++ show test1 ++ " = " ++ show result1
    
    -- IsCompleteStringLiteral escaped quotes 测试期望的: "\"\\\"\""
    let test2 = "\"\\\"\""
    let result2 = isCompleteStringLiteral test2
    putStrLn $ "isCompleteStringLiteral " ++ show test2 ++ " = " ++ show result2
    
    -- IsProblematicUnclosedString 失败案例: ""
    let test3 = ""
    let result3 = isProblematicUnclosedString test3
    putStrLn $ "isProblematicUnclosedString " ++ show test3 ++ " = " ++ show result3