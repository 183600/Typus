import Utils (removeLineComments)

main :: IO ()
main = do
    -- 测试几个特定的情况
    let testCases = ["\"", "a'a"]
    
    mapM_ (\s -> do
        let stringWithComment = s ++ " // comment"
            result = removeLineComments stringWithComment
        putStrLn $ "Input: " ++ show s
        putStrLn $ "String with comment: " ++ show stringWithComment
        putStrLn $ "Result: " ++ show result
        putStrLn ""
        ) testCases