import Utils

-- 测试 prop_remove_line_comments_multiline 失败的情况
main :: IO ()
main = do
    putStrLn "Testing removeLineComments with multiline..."
    
    -- 根据测试失败信息，输入是 ["b\n"]
    let lines' = ["b\n"]
    let code = unlines lines'
    let processed = removeLineComments code
    let procLines = lines processed
    
    putStrLn $ "Input lines: " ++ show lines'
    putStrLn $ "Code: " ++ show code
    putStrLn $ "Processed: " ++ show processed
    putStrLn $ "Processed lines: " ++ show procLines
    putStrLn $ "Expected length: 2"
    putStrLn $ "Actual length: " ++ show (length procLines)
    putStrLn $ "Test passes: " ++ show (length procLines == 2)