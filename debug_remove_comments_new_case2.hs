#!/usr/bin/env runhaskell

import qualified Utils

-- 详细调试 RemoveComments 的新失败案例
debugRemoveCommentsNewCase :: IO ()
debugRemoveCommentsNewCase = do
    let input = "\"" ++ " /* \\ */" ++ "\""
    putStrLn $ "=== 调试 RemoveComments: " ++ show input ++ " ==="
    
    -- 逐步分析
    putStrLn "输入字符分解:"
    putStrLn $ show (map (\c -> (c, fromEnum c)) input)
    
    let result = Utils.removeComments input
    putStrLn $ "输出: " ++ show result
    putStrLn $ "输出字符分解:"
    putStrLn $ show (map (\c -> (c, fromEnum c)) result)
    
    putStrLn "\n期望的处理过程:"
    putStrLn "1. 遇到第一个 \" -> 进入字符串模式"
    putStrLn "2. 遇到空格 -> 检查后面是否是注释"
    putStrLn "3. 遇到 /* -> 是注释，跳过空格和注释"
    putStrLn "4. 在注释中遇到 \\ -> 这是转义字符，应该跳过"
    putStrLn "5. 遇到 */ -> 注释结束"
    putStrLn "6. 遇到第二个 \" -> 字符串结束"
    putStrLn "7. 期望结果: \"\""
    
    putStrLn $ "\n实际结果: " ++ show result
    putStrLn $ "问题: 输出包含了反斜杠和空格，说明注释中的转义字符没有被正确处理"

main :: IO ()
main = debugRemoveCommentsNewCase