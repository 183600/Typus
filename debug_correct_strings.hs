#!/usr/bin/env runhaskell

import qualified Utils

-- 测试正确的字符串
main :: IO ()
main = do
    putStrLn "=== 测试正确的字符串 ==="
    
    -- 测试 "\""
    let testString = "\""
    putStrLn $ "测试字符串: " ++ show testString
    putStrLn $ "字符编码: " ++ show (map (\c -> (c, fromEnum c)) testString)
    putStrLn $ "isProblematicUnclosedString: " ++ show (Utils.isProblematicUnclosedString testString)
    
    -- 测试 "\""
    let testString2 = "\""
    putStrLn $ "\n测试字符串: " ++ show testString2
    putStrLn $ "字符编码: " ++ show (map (\c -> (c, fromEnum c)) testString2)
    putStrLn $ "isProblematicUnclosedString: " ++ show (Utils.isProblematicUnclosedString testString2)
    
    -- 测试 "\""
    let testString3 = "\""
    putStrLn $ "\n测试字符串: " ++ show testString3
    putStrLn $ "字符编码: " ++ show (map (\c -> (c, fromEnum c)) testString3)
    putStrLn $ "isProblematicUnclosedString: " ++ show (Utils.isProblematicUnclosedString testString3)