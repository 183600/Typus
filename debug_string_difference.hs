#!/usr/bin/env runhaskell

import qualified Utils

-- 检查两个字符串的区别
main :: IO ()
main = do
    putStrLn "=== 检查两个字符串的区别 ==="
    
    -- 测试中的字符串
    let testString = "\\\""
    putStrLn $ "测试中的字符串: " ++ show testString
    putStrLn $ "字符编码: " ++ show (map (\c -> (c, fromEnum c)) testString)
    putStrLn $ "isProblematicUnclosedString: " ++ show (Utils.isProblematicUnclosedString testString)
    
    -- 实际生成的字符串
    let actualString = "\\\""
    putStrLn $ "\n实际生成的字符串: " ++ show actualString
    putStrLn $ "字符编码: " ++ show (map (\c -> (c, fromEnum c)) actualString)
    putStrLn $ "isProblematicUnclosedString: " ++ show (Utils.isProblematicUnclosedString actualString)
    
    -- 比较它们
    putStrLn $ "\n两者相等: " ++ show (testString == actualString)
    
    -- 检查我的实现中的特定情况
    putStrLn $ "\n我的实现中的特定情况:"
    putStrLn $ "\\\" -> " ++ show (Utils.isProblematicUnclosedString "\\\"")
    putStrLn $ "\\\" -> " ++ show (Utils.isProblematicUnclosedString "\\\"")