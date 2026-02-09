module Main where

import Utils

main :: IO ()
main = do
    putStrLn "=== 详细调试 removeComments ==="
    
    let input = "\"\\ /*  */\""
    let result = removeComments input
    
    putStrLn $ "输入: " ++ show input
    putStrLn $ "输出: " ++ show result
    putStrLn $ "期望: \"\\\"\""
    
    -- 逐步分析
    putStrLn "\n逐步分析:"
    putStrLn "1. 遇到第一个 \" -> 进入字符串模式"
    putStrLn "2. 遇到 \\ -> 输出 \\，继续字符串模式"
    putStrLn "3. 遇到空格 -> 检查后面是否是注释"
    putStrLn "4. 发现 /* -> 进入注释跳过模式"
    putStrLn "5. 跳过注释内容"
    putStrLn "6. 遇到 */ -> 注释结束，回到字符串模式"
    putStrLn "7. 遇到 \" -> 字符串结束"
    
    putStrLn "\n期望的输出应该是: \"\\\" + \"\" = \"\\\"\""
    putStrLn $ "实际的输出是: " ++ show result