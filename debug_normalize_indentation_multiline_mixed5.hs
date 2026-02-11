import Utils

-- 测试 prop_normalize_indentation_multiline_mixed 失败的情况
main :: IO ()
main = do
    putStrLn "Testing normalizeIndentation with multiline mixed..."
    
    -- 测试失败的情况：lines' = ["a\n"]
    let lines' = ["a\n"]
    let withMixed = map ("\t  " ++) lines'
    let normalized = normalizeIndentation (unlines withMixed)
    let normLines = lines normalized
    
    putStrLn $ "Input lines': " ++ show lines'
    putStrLn $ "With mixed: " ++ show withMixed
    putStrLn $ "Input string: " ++ show (unlines withMixed)
    putStrLn $ "Normalized: " ++ show normalized
    putStrLn $ "Normalized lines: " ++ show normLines
    putStrLn $ "Expected length: 1"
    putStrLn $ "Actual length: " ++ show (length normLines)
    putStrLn $ "Test passes: " ++ show (length normLines == 1)