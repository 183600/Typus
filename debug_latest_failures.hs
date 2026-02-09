module Main where

import Utils

main :: IO ()
main = do
    putStrLn "=== 测试最新的失败案例 ==="
    
    -- RemoveComments strings with comments 失败案例: "\\" 和 ""
    let test1 = "\\"
    let result1 = removeComments test1
    putStrLn $ "removeComments " ++ show test1 ++ " = " ++ show result1
    
    let test2 = ""
    let result2 = removeComments test2
    putStrLn $ "removeComments " ++ show test2 ++ " = " ++ show result2
    
    -- IsCompleteStringLiteral invalid 失败案例: \"a
    let test3 = "\"a"
    let result3 = isCompleteStringLiteral test3
    putStrLn $ "isCompleteStringLiteral " ++ show test3 ++ " = " ++ show result3
    
    -- IsProblematicUnclosedString 失败案例: ""
    let test4 = ""
    let result4 = isProblematicUnclosedString test4
    putStrLn $ "isProblematicUnclosedString " ++ show test4 ++ " = " ++ show result4