import Utils

-- 测试 normalizeIndentation preserve empty 失败的情况
main :: IO ()
main = do
    putStrLn "Testing normalizeIndentation preserve empty..."
    
    -- 根据测试失败信息，输入是 ""
    let s = ""
    let normalized = normalizeIndentation s
    
    putStrLn $ "Input s: " ++ show s
    putStrLn $ "Normalized: " ++ show normalized
    putStrLn $ "Test passes: " ++ show (normalized == s)