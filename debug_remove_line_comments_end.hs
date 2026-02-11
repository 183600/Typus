import Utils

-- 测试 prop_remove_line_comments_end 失败的情况
main :: IO ()
main = do
    putStrLn "Testing removeLineComments with end comments..."
    
    -- 根据测试失败信息，输入是 "a'"
    let s = "a'"
    let withComment = s ++ "// comment"
    let processed = removeLineComments withComment
    
    putStrLn $ "Input s: " ++ show s
    putStrLn $ "With comment: " ++ show withComment
    putStrLn $ "Processed: " ++ show processed
    putStrLn $ "Expected: " ++ show s
    putStrLn $ "Test passes: " ++ show (processed == s)