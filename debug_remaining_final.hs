#!/usr/bin/env runhaskell

import qualified Utils

-- 测试剩余的失败案例
testRemainingFailures :: IO ()
testRemainingFailures = do
    putStrLn "=== 剩余的失败案例 ==="
    
    -- IsCompleteStringLiteral invalid: "\""
    let input1 = "\""
    putStrLn $ "IsCompleteStringLiteral invalid 案例:"
    putStrLn $ "  输入 = " ++ show input1
    putStrLn $ "  结果 = " ++ show (Utils.isCompleteStringLiteral input1)
    putStrLn $ "  期望 = False"
    putStrLn $ "  符合期望 = " ++ show (not (Utils.isCompleteStringLiteral input1))
    putStrLn ""
    
    -- IsProblematicUnclosedString: "\""
    let input2 = "\""
    putStrLn $ "IsProblematicUnclosedString 案例:"
    putStrLn $ "  输入 = " ++ show input2
    putStrLn $ "  结果 = " ++ show (Utils.isProblematicUnclosedString input2)
    putStrLn $ "  期望 = True"
    putStrLn $ "  符合期望 = " ++ show (Utils.isProblematicUnclosedString input2)
    
    -- 检查字符编码
    putStrLn "\n=== 字符编码检查 ==="
    putStrLn $ "\"\"\": " ++ show (map (\c -> (c, fromEnum c)) "\"")

main :: IO ()
main = testRemainingFailures