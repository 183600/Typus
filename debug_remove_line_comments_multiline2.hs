import Utils

-- 测试 prop_remove_line_comments_multiline 失败的情况
main :: IO ()
main = do
    putStrLn "Testing removeLineComments with multiline..."
    
    -- 测试不同的输入情况
    let testCases = [
            (["\n"], "Case 1: [\"\\n\"]"),
            (["a\n"], "Case 2: [\"a\\n\"]"),
            ([""], "Case 3: [\"\"]"),
            (["", ""], "Case 4: [\"\", \"\"]"),
            (["\nA"], "Case 5: [\"\\nA\"]"),
            (["a\n"], "Case 6: [\"a\\n\"] (duplicate)")
            ]
    
    mapM_ (\(lines', desc) -> do
        let code = unlines lines'
        let processed = removeLineComments code
        let procLines = lines processed
        
        putStrLn $ desc
        putStrLn $ "Input lines: " ++ show lines'
        putStrLn $ "Code: " ++ show code
        putStrLn $ "Processed: " ++ show processed
        putStrLn $ "Processed lines: " ++ show procLines
        putStrLn $ "Length: " ++ show (length procLines)
        putStrLn $ "---"
        ) testCases