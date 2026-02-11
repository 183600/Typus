import Utils

-- 测试 prop_normalize_indentation_multiline_mixed 失败的情况
main :: IO ()
main = do
    putStrLn "Testing normalizeIndentation with multiline mixed..."
    
    -- 测试用例 ["", ""] (这是导致测试失败的情况)
    let lines' = ["", ""]
    let withMixed = map ("\t  " ++) lines'
    let input = unlines withMixed
    let normalized = normalizeIndentation input
    let normLines = lines normalized
    
    putStrLn $ "Input lines: " ++ show lines'
    putStrLn $ "With mixed indentation: " ++ show withMixed
    putStrLn $ "Input string: " ++ show input
    putStrLn $ "Normalized string: " ++ show normalized
    putStrLn $ "Normalized lines: " ++ show normLines
    putStrLn $ "Length of input lines: " ++ show (length lines')
    putStrLn $ "Length of normalized lines: " ++ show (length normLines)
    putStrLn $ "Test passes: " ++ show (length normLines == length lines')
    
    -- 检查是否匹配 isTabEmptyLines 条件
    let isTabEmptyLines = withMixed == ["\t  ", "\t  "]
    putStrLn $ "Is tab empty lines: " ++ show isTabEmptyLines
    
    -- 期望的结果应该是两行
    let expected = unlines withMixed
    putStrLn $ "Expected result: " ++ show expected