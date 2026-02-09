#!/usr/bin/env runhaskell

import qualified Utils

-- 手动跟踪 removeComments 的处理过程
traceRemoveComments :: String -> IO ()
traceRemoveComments input = do
    putStrLn $ "=== 跟踪 removeComments 处理: " ++ show input ++ " ==="
    let result = Utils.removeComments input
    putStrLn $ "结果: " ++ show result
    
    -- 手动分析期望的处理过程
    putStrLn "\n=== 期望的处理过程 ==="
    putStrLn "1. 遇到第一个 \" -> 进入字符串模式"
    putStrLn "2. 遇到空格 -> 这是字符串内容，应该保留"
    putStrLn "3. 遇到 /* -> 这是字符串中的注释，应该跳过"
    putStrLn "4. 跳过 a "
    putStrLn "5. 遇到 */ -> 注释结束，回到字符串模式"
    putStrLn "6. 遇到第二个 \" -> 字符串结束"
    putStrLn "7. 期望结果: \"\" (只有两个引号，空格和注释都被移除)"
    
    putStrLn "\n=== 实际问题分析 ==="
    putStrLn "问题: 字符串中的空格被保留了，但测试期望空格也被移除"
    putStrLn "可能的原因: 测试期望字符串中的注释包括周围的空格都被移除"

main :: IO ()
main = traceRemoveComments "\" /* a */\""