module Main where

import Utils

main :: IO ()
main = do
    putStrLn "=== 测试 QuickCheck 直接生成的失败案例 ==="
    
    -- IsCompleteStringLiteral invalid 失败案例: "\"a
    let test1 = "\"a"
    let result1 = isCompleteStringLiteral test1
    putStrLn $ "isCompleteStringLiteral " ++ show test1 ++ " = " ++ show result1
    
    -- IsProblematicUnclosedString 失败案例: ""
    let test2 = ""
    let result2 = isProblematicUnclosedString test2
    putStrLn $ "isProblematicUnclosedString " ++ show test2 ++ " = " ++ show result2
    
    -- 测试更多相关的案例
    putStrLn "\n=== 测试相关案例 ==="
    let test1 = "\"\\"
    let result3 = isCompleteStringLiteral test1
    putStrLn $ "双引号+反斜杠: " ++ show test1 ++ " -> " ++ show result3
    
    let test4 = "'"
    let result4 = isCompleteStringLiteral test4
    putStrLn $ "单引号: " ++ show test4 ++ " -> " ++ show result4
    
    let test5 = "\"a\""
    let result5 = isCompleteStringLiteral test5
    putStrLn $ "完整的字符串: " ++ show test5 ++ " -> " ++ show result5
    
    let test6 = "\"a\\"
    let result6 = isCompleteStringLiteral test6
    putStrLn $ "不完整的字符串: " ++ show test6 ++ " -> " ++ show result6