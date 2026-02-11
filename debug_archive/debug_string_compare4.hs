main :: IO ()
main = do
    let s1 = "\"" ++ "\\"  -- 双引号 + 反斜杠
    let s2 = "\\"  -- 反斜杠
    putStrLn $ "s1: " ++ show s1
    putStrLn $ "s2: " ++ show s2
    putStrLn $ "s1 == s2: " ++ show (s1 == s2)
    
    -- 检查字符串长度
    putStrLn $ "Length s1: " ++ show (length s1)
    putStrLn $ "Length s2: " ++ show (length s2)