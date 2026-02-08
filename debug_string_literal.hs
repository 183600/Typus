import Utils

main :: IO ()
main = do
    let testCases = 
          [ "\\\"
          , "'\\\"
          , "\\\"\\\""
          , "'\\\'\\\'"
          , "\\\\\\\""
          , "\"abc"
          , "\"abc\""
          ]
    
    mapM_ (\testCase -> do
        putStrLn $ "Testing: " ++ show testCase
        putStrLn $ "isCompleteStringLiteral: " ++ show (isCompleteStringLiteral testCase)
        putStrLn "") testCases