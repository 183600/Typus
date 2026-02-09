#!/usr/bin/env runhaskell

-- 导入Utils模块
import Utils
import Data.List (isInfixOf)

-- 测试确切的字符串
main :: IO ()
main = do
    putStrLn "=== 测试确切的字符串表示 ==="
    
    -- 从测试属性中提取的确切字符串
    let exactStrings = 
            [ ("\"\\", "来自 prop_is_complete_string_literal_invalid: \"\\")
            , ("'\\", "来自 prop_is_complete_string_literal_invalid: '\\")
            , ("\"\\\"\"", "来自 prop_is_complete_string_literal_escaped_quotes: \"\\\"\"")
            , ("\"\\", "来自 prop_is_problematic_unclosed_string: \"\\")
            ]
    
    mapM_ (\(str, desc) -> do
        putStrLn $ desc
        putStrLn $ "  字符串: " ++ show str
        putStrLn $ "  长度: " ++ show (length str)
        putStrLn $ "  逐字符: " ++ show (map (\c -> (c, fromEnum c)) str)
        putStrLn $ "  isCompleteStringLiteral: " ++ show (isCompleteStringLiteral str)
        putStrLn $ "  isProblematicUnclosedString: " ++ show (isProblematicUnclosedString str)
        putStrLn ""
        ) exactStrings
    
    -- 检查测试失败输出中的确切案例
    putStrLn "=== 检查测试失败输出中的案例 ==="
    
    -- 从测试输出看到的失败案例
    let failureCases = 
            [ ("\"a", "IsCompleteStringLiteral invalid 的失败案例: \"\\\"a")
            , ("\"", "IsCompleteStringLiteral escaped quotes 的失败案例: \"\\\"\"")
            , ("'", "IsProblematicUnclosedString 的失败案例: \"'\"")
            ]
    
    mapM_ (\(str, desc) -> do
        putStrLn $ desc
        putStrLn $ "  字符串: " ++ show str
        putStrLn $ "  长度: " ++ show (length str)
        putStrLn $ "  逐字符: " ++ show (map (\c -> (c, fromEnum c)) str)
        putStrLn $ "  isCompleteStringLiteral: " ++ show (isCompleteStringLiteral str)
        putStrLn $ "  isProblematicUnclosedString: " ++ show (isProblematicUnclosedString str)
        putStrLn ""
        ) failureCases