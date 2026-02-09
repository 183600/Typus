#!/usr/bin/env runhaskell

import qualified Utils

-- 检查字符串构建的差异
main :: IO ()
main = do
    putStrLn "=== 检查字符串构建的差异 ==="
    
    -- 直接构建
    let directString = "\"\\\""
    putStrLn $ "直接构建: " ++ show directString
    putStrLn $ "字符编码: " ++ show (map (\c -> (c, fromEnum c)) directString)
    putStrLn $ "isProblematicUnclosedString: " ++ show (Utils.isProblematicUnclosedString directString)
    
    -- 通过连接构建
    let concatenatedString = "\"" ++ "\\\""
    putStrLn $ "\n通过连接构建: " ++ show concatenatedString
    putStrLn $ "字符编码: " ++ show (map (\c -> (c, fromEnum c)) concatenatedString)
    putStrLn $ "isProblematicUnclosedString: " ++ show (Utils.isProblematicUnclosedString concatenatedString)
    
    -- 在 debug_quickcheck_logic.hs 中的构建方式
    let validS = ""
    let problematicString = "\"\\\"" ++ validS
    putStrLn $ "\n在 debug_quickcheck_logic.hs 中的构建方式: " ++ show problematicString
    putStrLn $ "字符编码: " ++ show (map (\c -> (c, fromEnum c)) problematicString)
    putStrLn $ "isProblematicUnclosedString: " ++ show (Utils.isProblematicUnclosedString problematicString)
    
    -- 比较它们是否相等
    putStrLn $ "\ndirectString == concatenatedString: " ++ show (directString == concatenatedString)
    putStrLn $ "directString == problematicString: " ++ show (directString == problematicString)
    putStrLn $ "concatenatedString == problematicString: " ++ show (concatenatedString == problematicString)