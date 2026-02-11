import Utils

-- 测试 prop_is_complete_string_literal_escaped 失败的情况
main :: IO ()
main = do
    putStrLn "Testing prop_is_complete_string_literal_escaped..."
    
    -- 测试用例 "c" (这是导致测试失败的情况)
    let s = "c"
    let escaped = "\"" ++ s ++ "\\\"\""
    putStrLn $ "Input s: " ++ show s
    putStrLn $ "Escaped string: " ++ show escaped
    putStrLn $ "isCompleteStringLiteral result: " ++ show (isCompleteStringLiteral escaped)
    
    -- 运行实际的测试
    let result = isCompleteStringLiteral escaped
    putStrLn $ "Test passes: " ++ show (result == True)
    
    -- 测试其他字符
    let otherChars = ["a", "b", "d", "e", "f"]
    mapM_ (\c -> do
        let escaped' = "\"" ++ c ++ "\\\"\""
        let result' = isCompleteStringLiteral escaped'
        putStrLn $ "Character " ++ show c ++ ": " ++ show escaped' ++ " -> " ++ show result') otherChars