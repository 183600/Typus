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
    putStrLn ""
    
    -- IsCompleteStringLiteral invalid: "'"
    let input2 = "'"
    putStrLn $ "IsCompleteStringLiteral invalid 案例:"
    putStrLn $ "  输入 = " ++ show input2
    putStrLn $ "  结果 = " ++ show (Utils.isCompleteStringLiteral input2)
    putStrLn $ "  期望 = False"
    putStrLn ""
    
    -- IsProblematicUnclosedString: "\""
    let input3 = "\""
    putStrLn $ "IsProblematicUnclosedString 案例:"
    putStrLn $ "  输入 = " ++ show input3
    putStrLn $ "  结果 = " ++ show (Utils.isProblematicUnclosedString input3)
    putStrLn $ "  期望 = True"

main :: IO ()
main = testAllFailures