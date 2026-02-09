#!/usr/bin/env runhaskell

-- 导入Utils模块
import Utils

-- 测试特定的边界情况
main :: IO ()
main = do
    putStrLn "=== 测试特定的边界情况 ==="
    
    -- 测试所有可能引起问题的案例
    let testCases = 
            [ ("", "空字符串")
            , ("\"", "单个双引号")
            , ("'", "单个单引号")
            , ("\\", "单个反斜杠")
            , ("\"\\", "双引号+反斜杠")
            , ("'\\", "单引号+反斜杠")
            , ("\"\\\"", "双引号+反斜杠+双引号")
            , ("'\\\"", "单引号+反斜杠+双引号")
            , ("\"\\\"\"", "双引号+反斜杠+双引号+双引号")
            , ("'a", "单引号+a")
            , ("\"a", "双引号+a")
            , ("\"\\a", "双引号+反斜杠+a")
            ]
    
    mapM_ (\(input, desc) -> do
        putStrLn $ desc ++ " (" ++ show input ++ "):"
        putStrLn $ "  isCompleteStringLiteral: " ++ show (isCompleteStringLiteral input)
        putStrLn $ "  isProblematicUnclosedString: " ++ show (isProblematicUnclosedString input)
        
        -- 分析为什么是这个结果
        case input of
            "\"" -> putStrLn $ "    分析: 单个双引号，不是完整的字符串字面量"
            "'" -> putStrLn $ "    分析: 单个单引号，不是完整的字符串字面量"
            "\\" -> putStrLn $ "    分析: 单个反斜杠，是完整的转义反斜杠"
            "\"\\" -> putStrLn $ "    分析: 双引号+反斜杠，反斜杠在末尾，不完整"
            "'\\" -> putStrLn $ "    分析: 单引号+反斜杠，反斜杠在末尾，不完整"
            "\"\\\"" -> putStrLn $ "    分析: 双引号+反斜杠+双引号，包含转义引号，完整"
            "'\\\"" -> putStrLn $ "    分析: 单引号+反斜杠+双引号，包含转义引号，完整"
            "\"\\\"\"" -> putStrLn $ "    分析: 双引号+反斜杠+双引号+双引号，转义引号+闭合引号，完整"
            "'a" -> putStrLn $ "    分析: 单引号+a，没有闭合单引号，不完整"
            "\"a" -> putStrLn $ "    分析: 双引号+a，没有闭合双引号，不完整"
            "\"\\a" -> putStrLn $ "    分析: 双引号+反斜杠+a，转义字符后没有有效字符，不完整"
            _ -> putStrLn $ "    分析: 其他情况"
        putStrLn ""
        ) testCases