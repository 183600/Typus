#!/usr/bin/env runhaskell

import qualified Utils

-- 分步调试 removeComments
debugRemoveComments :: IO ()
debugRemoveComments = do
    let input = "\" /* a */\""
    putStrLn "=== 分步调试 removeComments ==="
    putStrLn $ "输入: " ++ show input
    
    -- 手动跟踪处理过程
    let result = Utils.removeComments input
    putStrLn $ "输出: " ++ show result
    putStrLn $ "期望: " ++ show "\"\""
    putStrLn $ "匹配: " ++ show (result == "\"\"")
    
    -- 分析字符
    putStrLn "\n=== 字符分析 ==="
    putStrLn $ "输入字符: " ++ show (map (\c -> (c, fromEnum c)) input)
    putStrLn $ "输出字符: " ++ show (map (\c -> (c, fromEnum c)) result)
    putStrLn $ "期望字符: " ++ show (map (\c -> (c, fromEnum c)) "\"\"")

main :: IO ()
main = debugRemoveComments