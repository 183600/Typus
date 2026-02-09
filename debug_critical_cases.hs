module Main where

import Utils

main :: IO ()
main = do
    putStrLn "=== 测试关键案例 ==="
    
    -- 测试 "\" (双引号+反斜杠)
    let test1 = "\"\\"
    let result1 = isCompleteStringLiteral test1
    putStrLn $ "isCompleteStringLiteral " ++ show test1 ++ " = " ++ show result1
    
    -- 测试 '\ (单引号+反斜杠)
    let test2 = "'\\"
    let result2 = isCompleteStringLiteral test2
    putStrLn $ "isCompleteStringLiteral " ++ show test2 ++ " = " ++ show result2
    
    -- 测试 \"" (双引号+转义双引号)
    let test3 = "\"\\\""
    let result3 = isCompleteStringLiteral test3
    putStrLn $ "isCompleteStringLiteral " ++ show test3 ++ " = " ++ show result3
    
    -- 测试 \"\"" (双引号+转义双引号+双引号)
    let test4 = "\"\\\"\""
    let result4 = isCompleteStringLiteral test4
    putStrLn $ "isCompleteStringLiteral " ++ show test4 ++ " = " ++ show result4