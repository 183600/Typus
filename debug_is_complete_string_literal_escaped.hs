import Utils

-- 测试 prop_is_complete_string_literal_escaped 失败的情况
main :: IO ()
main = do
    putStrLn "Testing isCompleteStringLiteral with escaped quotes..."
    
    -- 测试用例 "c"
    let test1 = "\"c\\\"\""
    putStrLn $ "Test 1: " ++ show test1 ++ " -> " ++ show (isCompleteStringLiteral test1)
    
    -- 其他一些测试用例
    let test2 = "\"a\\\"\""
    putStrLn $ "Test 2: " ++ show test2 ++ " -> " ++ show (isCompleteStringLiteral test2)
    
    let test3 = "\"b\\\"\""
    putStrLn $ "Test 3: " ++ show test3 ++ " -> " ++ show (isCompleteStringLiteral test3)
    
    -- 空字符串
    let test4 = "\"\""
    putStrLn $ "Test 4: " ++ show test4 ++ " -> " ++ show (isCompleteStringLiteral test4)
    
    -- 单个引号
    let test5 = "\""
    putStrLn $ "Test 5: " ++ show test5 ++ " -> " ++ show (isCompleteStringLiteral test5)