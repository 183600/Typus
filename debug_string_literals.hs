-- 测试 isCompleteStringLiteral 的行为
module Main where

import Utils

main :: IO ()
main = do
    putStrLn "=== 测试 isCompleteStringLiteral ===\n"
    
    let testCases = 
            [ ("\"", "单个双引号")
            , ("'", "单个单引号")
            , ("\\", "单个反斜杠")
            , ("\\\"", "转义双引号")
            , ("\\'", "转义单引号")
            , ("\"\\\"\"", "包含转义双引号的完整字符串")
            , ("'\\'", "包含转义单引号的完整字符")
            , ("\"hello\"", "普通字符串")
            , ("'a'", "普通字符")
            , ("\"unclosed", "未闭合字符串")
            , ("'unclosed", "未闭合字符")
            , ("", "空字符串")
            ]
    
    mapM_ (\(input, desc) -> do
        let result = isCompleteStringLiteral input
        putStrLn $ desc ++ ": " ++ show input ++ " -> " ++ show result
    ) testCases
    
    putStrLn "\n=== 测试 isProblematicUnclosedString ===\n"
    
    let problematicTestCases = 
            [ ("\"", "单个双引号")
            , ("'", "单个单引号")
            , ("'\\", "包含转义引号但不完整的字符串")
            , ("", "空字符串")
            , ("\"unclosed", "未闭合字符串")
            , ("'unclosed", "未闭合字符")
            , ("\"hello\"", "完整字符串")
            , ("'a'", "完整字符")
            ]
    
    mapM_ (\(input, desc) -> do
        let result = isProblematicUnclosedString input
        putStrLn $ desc ++ ": " ++ show input ++ " -> " ++ show result
    ) problematicTestCases