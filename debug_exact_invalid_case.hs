module Main where

import Utils

main :: IO ()
main = do
    putStrLn "=== 测试 IsCompleteStringLiteral invalid 的确切失败案例 ==="
    
    -- 模拟 QuickCheck 生成的案例
    let s = "a"
    let validS = take 50 s
    let stringWithoutEndQuote = "\"" ++ validS ++ "\\"  -- "\"a\\"
    
    putStrLn $ "s = " ++ show s
    putStrLn $ "validS = " ++ show validS
    putStrLn $ "stringWithoutEndQuote = " ++ show stringWithoutEndQuote
    
    let result = isCompleteStringLiteral stringWithoutEndQuote
    putStrLn $ "isCompleteStringLiteral " ++ show stringWithoutEndQuote ++ " = " ++ show result
    putStrLn $ "期望: False"
    
    -- 分析为什么 "\"a\\" 应该是 False
    putStrLn "\n分析:"
    putStrLn "\"a\\ 以双引号开头"
    putStrLn "包含 a"
    putStrLn "以反斜杠结尾"
    putStrLn "没有闭合引号"
    putStrLn "因此应该是不完整的字符串字面量 (False)"