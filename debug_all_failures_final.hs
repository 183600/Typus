#!/usr/bin/env runhaskell

import qualified Utils

-- 测试所有失败案例
testAllFailures :: IO ()
testAllFailures = do
    putStrLn "=== 所有失败案例测试 ==="
    
    -- IsCompleteStringLiteral valid: "\\"
    let input1 = "\\"
    putStrLn $ "IsCompleteStringLiteral valid 案例:"
    putStrLn $ "  输入 = " ++ show input1
    putStrLn $ "  结果 = " ++ show (Utils.isCompleteStringLiteral input1)
    putStrLn $ "  期望 = True (根据测试失败)"
    putStrLn $ "  符合期望 = " ++ show (Utils.isCompleteStringLiteral input1)
    putStrLn ""
    
    -- IsCompleteStringLiteral invalid: "'"
    let input2 = "'"
    putStrLn $ "IsCompleteStringLiteral invalid 案例:"
    putStrLn $ "  输入 = " ++ show input2
    putStrLn $ "  结果 = " ++ show (Utils.isCompleteStringLiteral input2)
    putStrLn $ "  期望 = False"
    putStrLn $ "  符合期望 = " ++ show (not (Utils.isCompleteStringLiteral input2))
    putStrLn ""
    
    -- IsProblematicUnclosedString: "\""
    let input3 = "\""
    putStrLn $ "IsProblematicUnclosedString 案例:"
    putStrLn $ "  输入 = " ++ show input3
    putStrLn $ "  结果 = " ++ show (Utils.isProblematicUnclosedString input3)
    putStrLn $ "  期望 = True"
    putStrLn $ "  符合期望 = " ++ show (Utils.isProblematicUnclosedString input3)
    
    -- 检查字符编码
    putStrLn "\n=== 字符编码检查 ==="
    putStrLn $ "\\": " ++ show (map (\c -> (c, fromEnum c)) "\\")
    putStrLn $ "': " ++ show (map (\c -> (c, fromEnum c)) "'")
    putStrLn $ "\\\"": " ++ show (map (\c -> (c, fromEnum c)) "\"")

main :: IO ()
main = testAllFailures