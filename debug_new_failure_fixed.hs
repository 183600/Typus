import Utils

main :: IO ()
main = do
    -- 测试新的失败情况
    let s = "'a"
    let validS = take 50 s
    let stringWithoutEndQuoteSingle = "'" ++ validS ++ "\\"
    
    putStrLn $ "输入 s = " ++ show s
    putStrLn $ "validS = " ++ show validS
    putStrLn $ "stringWithoutEndQuoteSingle = " ++ show stringWithoutEndQuoteSingle
    
    let result = isCompleteStringLiteral stringWithoutEndQuoteSingle
    putStrLn $ "isCompleteStringLiteral " ++ show stringWithoutEndQuoteSingle ++ " = " ++ show result
    putStrLn $ "not result = " ++ show (not result)
    
    -- 测试一些边界情况
    putStrLn "\n测试边界情况:"
    let testCases = [
            "''\\",
            "'a\\",
            "'ab\\",
            "'''\\",
            "'\\",
            "\"\\",
            "\"a\\"
            ]
    
    mapM_ testCase testCases
    
  where
    testCase :: String -> IO ()
    testCase input = do
        let result = isCompleteStringLiteral input
        putStrLn $ "isCompleteStringLiteral " ++ show input ++ " = " ++ show result