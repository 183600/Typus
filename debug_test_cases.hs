import qualified Utils as U

main :: IO ()
main = do
    -- 测试各种输入
    let testCases = 
            [ ("a/", "a/// comment")
            , ("b", "b// comment")
            , ("'", "'// comment")
            , (" ", " // comment")
            , ("/", "/// comment")
            ]
    
    mapM_ testCase testCases
  where
    testCase (s, withComment) = do
        let processed = U.removeLineComments withComment
        putStrLn $ "Input: " ++ show s
        putStrLn $ "With comment: " ++ show withComment
        putStrLn $ "Processed: " ++ show processed
        putStrLn $ "Expected: " ++ show s
        putStrLn $ "Test passes: " ++ show (processed == s)
        putStrLn ""