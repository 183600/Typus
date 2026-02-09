#!/usr/bin/env runhaskell

import qualified Utils

-- 测试 isProblematicUnclosedString 的特定情况
main :: IO ()
main = do
    putStrLn "=== 测试 isProblematicUnclosedString 的特定情况 ==="
    
    let testCases = 
            [ ("\"", "转义双引号")
            , ("'", "单引号")
            , ("\"\\\"", "双引号+反斜杠+双引号")
            , ("'\\", "单引号+反斜杠")
            ]
    
    mapM_ (\(input, desc) -> do
        let result = Utils.isProblematicUnclosedString input
        putStrLn $ desc ++ " " ++ show input ++ " -> " ++ show result
        ) testCases
    
    -- 特别检查 "\\\""
    let specificCase = "\"\\\""
    putStrLn $ "\n特别检查: isProblematicUnclosedString " ++ show specificCase ++ " = " ++ show (Utils.isProblematicUnclosedString specificCase)
    
    -- 检查字符编码
    putStrLn $ "字符编码: " ++ show (map (\c -> (c, fromEnum c)) specificCase)