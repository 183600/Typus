import Utils

main :: IO ()
main = do
    putStrLn "=== Debugging isCompleteStringLiteral ==="
    
    -- 测试失败案例
    let test1 = "\""
    putStrLn $ "Test1: " ++ show test1 ++ " -> " ++ show (isCompleteStringLiteral test1)
    
    -- 测试双引号 + 反斜杠
    let test2 = "\"\\"
    putStrLn $ "Test2: " ++ show test2 ++ " -> " ++ show (isCompleteStringLiteral test2)
    
    -- 测试双引号 + 双反斜杠
    let test3 = "\"\\\\"
    putStrLn $ "Test3: " ++ show test3 ++ " -> " ++ show (isCompleteStringLiteral test3)
    
    -- 测试双引号 + 双反斜杠 + 双引号
    let test4 = "\"\\\\\""
    putStrLn $ "Test4: " ++ show test4 ++ " -> " ++ show (isCompleteStringLiteral test4)
    
    -- 测试双引号 + 反斜杠 + 双引号
    let test5 = "\"\\\""
    putStrLn $ "Test5: " ++ show test5 ++ " -> " ++ show (isCompleteStringLiteral test5)
    
    -- 测试 prop_is_complete_string_literal_escape_backslash 的逻辑
    let s = ""
    let withBackslash = "\"" ++ s ++ "\\\\"
    putStrLn $ "prop_is_complete_string_literal_escape_backslash with s=" ++ show s ++ ": " ++ show withBackslash ++ " -> " ++ show (isCompleteStringLiteral withBackslash)
    
    let s2 = "\""
    let withBackslash2 = "\"" ++ s2 ++ "\\\\"
    putStrLn $ "prop_is_complete_string_literal_escape_backslash with s=" ++ show s2 ++ ": " ++ show withBackslash2 ++ " -> " ++ show (isCompleteStringLiteral withBackslash2)