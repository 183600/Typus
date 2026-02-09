-- 调试失败的测试用例
module Main where

import Utils

main :: IO ()
main = do
    putStrLn "=== 调试失败的测试用例 ===\n"
    
    -- 1. RemoveComments strings with comments
    putStrLn "1. RemoveComments strings with comments 失败案例:"
    let test1_input = "\"\\\"\""
    let test1_expected = "\"\\\"\""
    let test1_result = removeComments test1_input
    putStrLn $ "  输入: " ++ show test1_input
    putStrLn $ "  期望: " ++ show test1_expected
    putStrLn $ "  实际: " ++ show test1_result
    putStrLn $ "  通过: " ++ show (test1_result == test1_expected)
    
    let test2_input = "\"\""
    let test2_expected = "\"\""
    let test2_result = removeComments test2_input
    putStrLn $ "  输入: " ++ show test2_input
    putStrLn $ "  期望: " ++ show test2_expected
    putStrLn $ "  实际: " ++ show test2_result
    putStrLn $ "  通过: " ++ show (test2_result == test2_expected)
    
    -- 2. IsCompleteStringLiteral invalid
    putStrLn "\n2. IsCompleteStringLiteral invalid 失败案例:"
    let test3_input = "'a"
    let test3_expected = False
    let test3_result = isCompleteStringLiteral test3_input
    putStrLn $ "  输入: " ++ show test3_input
    putStrLn $ "  期望: " ++ show test3_expected
    putStrLn $ "  实际: " ++ show test3_result
    putStrLn $ "  通过: " ++ show (test3_result == test3_expected)
    
    -- 3. IsCompleteStringLiteral escaped quotes
    putStrLn "\n3. IsCompleteStringLiteral escaped quotes 失败案例:"
    let test4_input = "\""
    let test4_expected = True
    let test4_result = isCompleteStringLiteral test4_input
    putStrLn $ "  输入: " ++ show test4_input
    putStrLn $ "  期望: " ++ show test4_expected
    putStrLn $ "  实际: " ++ show test4_result
    putStrLn $ "  通过: " ++ show (test4_result == test4_expected)
    
    -- 4. IsProblematicUnclosedString
    putStrLn "\n4. IsProblematicUnclosedString 失败案例:"
    let test5_input = ""
    let test5_expected = True
    let test5_result = isProblematicUnclosedString test5_input
    putStrLn $ "  输入: " ++ show test5_input
    putStrLn $ "  期望: " ++ show test5_expected
    putStrLn $ "  实际: " ++ show test5_result
    putStrLn $ "  通过: " ++ show (test5_result == test5_expected)
    
    putStrLn "\n=== 调试完成 ==="