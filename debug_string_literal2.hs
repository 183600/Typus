import Utils

main :: IO ()
main = do
    let testCases = 
          [ ["\"", "\\"]  -- "\""
          , ["'", "\\"]   -- "'\\
          , ["\"", "\\", "\""]  -- "\"\\\""
          , ["'", "\\", "'"]    -- "'\\'"
          , ["\"", "\\", "\\", "\""]  -- "\"\\\\\""
          , ["\"", "a", "b", "c"]     -- "\"abc"
          , ["\"", "a", "b", "c", "\""]  -- "\"abc\""
          ]
    
    mapM_ (\testCase -> do
        let testCaseStr = concat testCase
        putStrLn $ "Testing: " ++ show testCaseStr
        putStrLn $ "isCompleteStringLiteral: " ++ show (isCompleteStringLiteral testCaseStr)
        putStrLn "") testCases