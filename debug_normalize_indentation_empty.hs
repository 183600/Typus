import Utils

-- 测试 normalizeIndentation empty lines 和 preserve empty 失败的情况
main :: IO ()
main = do
    putStrLn "Testing normalizeIndentation with empty..."
    
    -- 测试 normalize indentation empty lines 失败的情况
    let s1 = ""
    let withEmpty1 = s1 ++ "\n\n"
    let normalized1 = normalizeIndentation withEmpty1
    
    putStrLn $ "Test 1 - normalize indentation empty lines:"
    putStrLn $ "Input s: " ++ show s1
    putStrLn $ "With empty: " ++ show withEmpty1
    putStrLn $ "Normalized: " ++ show normalized1
    putStrLn $ "Expected: \"    \""
    putStrLn $ "Test passes: " ++ show (normalized1 == "    ")
    
    -- 测试 normalizeIndentation preserve empty 失败的情况
    let s2 = ""
    let normalized2 = normalizeIndentation s2
    
    putStrLn $ "\nTest 2 - normalizeIndentation preserve empty:"
    putStrLn $ "Input s: " ++ show s2
    putStrLn $ "Normalized: " ++ show normalized2
    putStrLn $ "Expected: \"\""
    putStrLn $ "Test passes: " ++ show (normalized2 == "")