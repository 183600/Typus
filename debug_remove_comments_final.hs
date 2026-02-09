module Main where

import Utils

main :: IO ()
main = do
    putStrLn "=== 详细测试 RemoveComments ==="
    
    let input = "\"" ++ "\\" ++ " /*  */\""
    let result = removeComments input
    
    putStrLn $ "输入: " ++ show input
    putStrLn $ "输出: " ++ show result
    putStrLn $ "期望包含: \"\\\""
    
    -- 分析输入字符串
    putStrLn "\n输入字符串分析:"
    putStrLn "位置 0: \" (开始字符串)"
    putStrLn "位置 1: \\ (反斜杠)"
    putStrLn "位置 2:   (空格)"
    putStrLn "位置 3-4: /* (注释开始)"
    putStrLn "位置 5-6:   (空格)"
    putStrLn "位置 7-8: */ (注释结束)"
    putStrLn "位置 9: \" (结束字符串)"
    
    -- 逐步分析处理过程
    putStrLn "\n处理过程分析:"
    putStrLn "1. 遇到 \" -> 进入字符串模式"
    putStrLn "2. 遇到 \\ -> 输出 \\"
    putStrLn "3. 遇到空格 -> 检查后面是否是注释"
    putStrLn "4. 发现 /* -> 进入注释跳过模式"
    putStrLn "5. 跳过注释内容"
    putStrLn "6. 遇到 */ -> 注释结束，回到字符串模式"
    putStrLn "7. 遇到 \" -> 字符串结束"
    
    putStrLn "\n期望的输出应该是: \"\\\" + \"\" = \"\\\"\""
    putStrLn $ "实际的输出是: " ++ show result