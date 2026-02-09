module Main where

import Utils

main :: IO ()
main = do
    putStrLn "=== 详细测试 isCompleteStringLiteral ==="
    
    -- 测试以反斜杠结尾的字符串
    let testCases = 
            [ ("\"", "单个双引号")
            , ("\"\\", "双引号+反斜杠")
            , ("\"\\\\", "双引号+双反斜杠")
            , ("\"\\\"", "双引号+转义双引号")
            , ("\"\\\\\"", "双引号+双反斜杠+双引号")
            , ("'", "单个单引号")
            , ("'\\", "单引号+反斜杠")
            , ("'\\\\", "单引号+双反斜杠")
            , ("'\\'", "单引号+转义单引号")
            , ("'\\\\'", "单引号+双反斜杠+单引号")
            ]
    
    mapM_ (\(input, desc) -> do
        let result = isCompleteStringLiteral input
        putStrLn $ desc ++ ": " ++ show input ++ " -> " ++ show result
    ) testCases