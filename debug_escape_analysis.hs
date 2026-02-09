#!/usr/bin/env runhaskell

-- 导入Utils模块
import Utils

-- 测试字符串转义和案例匹配
main :: IO ()
main = do
    putStrLn "=== 字符串转义和案例匹配分析 ==="
    
    -- 测试不同的字符串表示
    let testStrings = 
            [ ("\"\\\"", "字符串1: 反斜杠+双引号")
            , ("\"\\\"\\\"", "字符串2: 双引号+反斜杠+双引号")
            , ("'\\\"", "字符串3: 单引号+反斜杠+双引号")
            , ("'\\\\", "字符串4: 单引号+双反斜杠")
            ]
    
    mapM_ (\(str, desc) -> do
        putStrLn $ desc ++ ": " ++ show str
        putStrLn $ "  长度: " ++ show (length str)
        putStrLn $ "  isCompleteStringLiteral: " ++ show (isCompleteStringLiteral str)
        putStrLn $ "  isProblematicUnclosedString: " ++ show (isProblematicUnclosedString str)
        putStrLn ""
        ) testStrings
    
    -- 手动测试案例匹配
    putStrLn "=== 手动测试案例匹配 ==="
    let testStr = "\"\\\""
    putStrLn $ "测试字符串: " ++ show testStr
    putStrLn $ "案例匹配结果:"
    
    case testStr of
        "\"" -> putStrLn "  匹配案例: \"\\\"\\\"\" -> True"
        "'" -> putStrLn "  匹配案例: \"'\" -> True"  
        "\"\\\"" -> putStrLn "  匹配案例: \"\\\"\\\\\"\\\"\" -> True"
        "'\\" -> putStrLn "  匹配案例: \"'\\\\\"\" -> True"
        "\"\\\"" -> putStrLn "  匹配案例: \"\\\"\\\\\"\\\"\" -> True (重复)"
        "" -> putStrLn "  匹配案例: 空字符串 -> False"
        _ -> putStrLn $ "  匹配默认情况: not (isCompleteStringLiteral) && ... = " ++ 
                       show (not (isCompleteStringLiteral testStr) && not (null testStr) && 
                             case testStr of (c:_) -> c `elem` ['\"', '\''])
    
    putStrLn ""
    putStrLn "=== 逐字符分析测试字符串 ==="
    let testStr2 = "\"\\\""
    putStrLn $ "测试字符串: " ++ show testStr2
    mapM_ (\(i, c) -> putStrLn $ "  位置 " ++ show i ++ ": '" ++ [c] ++ "' (ASCII " ++ show (fromEnum c) ++ ")") (zip [0..] testStr2)
