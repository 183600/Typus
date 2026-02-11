import Utils

-- 测试 prop_normalize_indentation_mixed 失败的情况
main :: IO ()
main = do
    putStrLn "Testing normalizeIndentation with mixed..."
    
    -- 测试不同的输入情况
    let testCases = [
            ("\t  \t  " ++ " f" ++ "  \t  ", "Case 1: mixed with content"),
            ("\t  \t  ", "Case 2: mixed without content"),
            ("a", "Case 3: single character"),
            ("", "Case 4: empty string")
            ]
    
    mapM_ (\(input, desc) -> do
        let normalized = normalizeIndentation input
        
        putStrLn $ desc
        putStrLn $ "Input: " ++ show input
        putStrLn $ "Normalized: " ++ show normalized
        putStrLn $ "---"
        ) testCases