#!/usr/bin/env runhaskell

import qualified Utils

-- 测试所有相关的字符串字面量
testAllStringLiterals :: IO ()
testAllStringLiterals = do
    let testCases = 
            [ ("\"", "转义双引号")
            , ("\\", "反斜杠")
            , ("\"\\", "双引号+反斜杠")
            , ("\\\"", "反斜杠+双引号")
            , ("'\\", "单引号+反斜杠")
            , ("\\'", "反斜杠+单引号")
            ]
    
    putStrLn "=== 测试所有相关的字符串字面量 ==="
    mapM_ (\(input, desc) -> do
        putStrLn $ desc ++ " " ++ show input ++ ":"
        putStrLn $ "  isCompleteStringLiteral = " ++ show (Utils.isCompleteStringLiteral input)
        putStrLn $ "  isProblematicUnclosedString = " ++ show (Utils.isProblematicUnclosedString input)
        putStrLn ""
        ) testCases
    
    -- 特别检查测试用例中的期望
    putStrLn "=== 测试用例期望 ==="
    putStrLn $ "isCompleteStringLiteral \"\\\\\" = " ++ show (Utils.isCompleteStringLiteral "\\")
    putStrLn $ "  期望: False (单个反斜杠不是完整的字符串字面量)"
    putStrLn $ "isCompleteStringLiteral \"'\\\\\" = " ++ show (Utils.isCompleteStringLiteral "'\\")
    putStrLn $ "  期望: False (单引号+反斜杠不是完整的字符串字面量)"
    putStrLn $ "isCompleteStringLiteral \"\\\\\"\" = " ++ show (Utils.isCompleteStringLiteral "\"")
    putStrLn $ "  期望: False (转义双引号不是完整的字符串字面量)"

main :: IO ()
main = testAllStringLiterals