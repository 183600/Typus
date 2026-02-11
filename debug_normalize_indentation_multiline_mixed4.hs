import Utils

-- 测试 prop_normalize_indentation_multiline_mixed 失败的情况
main :: IO ()
main = do
    putStrLn "Testing normalizeIndentation with multiline mixed..."
    
    -- 测试失败的情况：lines' == [""]
    let lines' = [""]
    let withMixed = map ("\t  " ++) lines'
    let normalized = normalizeIndentation (unlines withMixed)
    
    putStrLn $ "Input lines': " ++ show lines'
    putStrLn $ "With mixed: " ++ show withMixed
    putStrLn $ "Input string: " ++ show (unlines withMixed)
    putStrLn $ "Normalized: " ++ show normalized
    putStrLn $ "Expected: \"    \""
    putStrLn $ "Test passes: " ++ show (normalized == "    ")